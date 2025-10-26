# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# HTTP/1.x request/response object to a backend HTTP server,
# not tied to PublicInbox::WWW but uses public-inbox-{httpd,netd}
# features.  Only used by PublicInbox::PsgiRproxy
package PublicInbox::H1ReqRes;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN EPOLLOUT EPOLLONESHOT);
# HTTP::Parser::XS is already used by most Plack installations
use HTTP::Parser::XS qw(parse_http_response HEADERS_AS_ARRAYREF);
use B qw(cstring);
use Carp qw(confess carp);

use constant { # duplicated from HTTP.pm
	CHUNK_START => -1,   # [a-f0-9]+\r\n
	CHUNK_END => -2,     # \r\n
	CHUNK_TLR_END => -3, # (trailers*)?\r\n
	CHUNK_MAX_HDR => 256,
	IO_SIZE => 1 << 16,
};

sub new {
	my ($cls, $sock, $env, $rproxy, $wcb) = @_;
	my $self = bless {
		wbuf => [ \&send_req_hdr ],
		wcb => $wcb, # write callbacke from PSGI server
		rproxy => $rproxy, # PublicInbox::PsgiRproxy
		env => $env,
		# http_out # PublicInbox::HTTP::{Identity,Chunked}
	}, $cls;
	$self->SUPER::new($sock, EPOLLOUT|EPOLLONESHOT);
}

# expands $fullpath in proxied request destination
sub fullpath {
	my $env = $_[-1];
	my ($sn, $u) = @$env{qw(SCRIPT_NAME REQUEST_URI)};
	$u =~ m!\A(?:https?://[^/]+)?\Q$sn\E(/.+)\z!i ? $1 : $sn.$u;
}

# expands $host in proxied request destination
sub host {
	my ($env) = $_[-1];
	$env->{HTTP_HOST} // $env->{SERVER_NAME};
}

sub send_req_hdr { # called by flush_write
	my ($self) = @_;
	my $env = $self->{env};
	my $path = $self->{rproxy}->{path}; # $fullpath
	my $prot = $env->{SERVER_PROTOCOL} // 'HTTP/1.0';
	$path =~ s!\$(\w+)!$self->$1($env)!sge;
	my ($k, $v);
	my $req = <<EOM;
$env->{REQUEST_METHOD} $path $prot\r
X-Forwarded-Proto: $env->{'psgi.url_scheme'}\r
Connection: close\r
EOM
	$v = $env->{HTTP_X_FORWARDED_FOR} // '';
	$v .= ', ' if $v =~ /\S/;
	$req .= "X-Forwarded-For: $v$env->{REMOTE_ADDR}\r\n";
	while (($k, $v) = each %$env) {
		next if $k =~ m!\AHTTP_(?:VERSION|CONNECTION|EXPECT|KEEP_ALIVE
					|X_FORWARDED_FOR|TRAILER|UPGRADE)\z!sx;
		# PublicInbox::HTTP already decoded chunked encoding, but
		# there may be other encodings which we pass straight through.
		# No idea if non-chunked Transfer-Encoding actually gets
		# used anywhere...
		if ($k eq 'HTTP_TRANSFER_ENCODING') {
			my @v = grep !/\Achunked\z/i, split /\s*,\s*/, $v;
			@v and $req .= 'Transfer-Encoding: '.
					join(', ', @v)."\r\n";
		} elsif ($k =~ /\AHTTP_(.+)\z/) {
			$k = $1;
			$k =~ tr/_/-/;
			$req .= "$k: $v\r\n";
		}
	}
	$v = $env->{CONTENT_TYPE} and $req .= "Content-Type: $v\r\n";
	# psgi.input always has an fstat(2)-able FD behind it w/ pi-httpd
	if ($v = -s $env->{'psgi.input'}) {
		$req .= "Content-Length: $v\r\n";
		$self->{req_left} = $v;
	}
	$req .= "\r\n";
	$self->write(\$req);
	$self->write($v ? \&send_req_body : \&pass_res_hdr);
}

sub _pass_done ($) {
	delete($_[0]->{http_out})->close;
	$_[0]->close;
	undef;
}

sub read_err ($$;$) {
	my ($self, $next_cb, $len) = @_;
	if ($self->{sock}) { # EAGAIN, wait for ->event_step
		push(@{$self->{wbuf}}, $next_cb) > 1 and
			confess 'BUG: attempted read w/ wbuf size='.
				scalar(@{$self->{wbuf}});
		$self->{res_left} = $len if defined $len;
	} elsif (defined $len) {
		die "W: upstream terminated while reading response ($len)\n";
	} else {
		die "W: upstream terminated while reading response\n";
	}
	undef;
}

# when proxy_buffering is disabled
sub h1rr_pull {
	my ($http) = @_; # PublicInbox::HTTP
	my $self = delete $http->{forward} or return; # backend aborted
	$self->{env}->{'pi-httpd.client'} = $http; # no circular ref here
	delete($http->{h1rr_cb})->($self);
};

# returns true if we can continue looping
sub pass_refill ($$$$) {
	my ($self, $n, $cb, $len) = @_;
	my $http = $self->{env}->{'pi-httpd.client'};

	# don't read from backend if remote client is blocked
	if ($http->{wbuf} && $http->{wbuf}->[0] &&
			!$self->{rproxy}->{proxy_buffering}) {
		delete $self->{env}->{'pi-httpd.client'}; # avoid circular ref
		$self->{res_left} = $len if defined $len;
		$http->{forward} = $self;
		$http->{h1rr_cb} = $cb;
		push @{$http->{wbuf}}, \&h1rr_pull;
		undef;
	} else {
		my $rbuf = $self->{rbuf};
		my $r = $self->do_read($rbuf, $n, length($$rbuf));
		(!defined($r) || ($r == 0 && defined($len))) ?
				read_err($self, $cb, $len) : $r;
	}
}

# pass a response body block of $len bytes (both chunk and identity)
sub _pass_res_block ($$) {
	my ($self, $len) = @_;
	my $rbuf = $self->{rbuf} //= \(my $x = '');
	my $cur = length($$rbuf);
	if ($cur > $len) { # CRLF, chunk header or trailer follows in rbuf:
		$self->{http_out}->write(substr($$rbuf, 0, $len, '')) or
			return _pass_done $self;
		0;
	} else { # $cur <= $len done with current rbuf
		$self->{http_out}->write($$rbuf) or return _pass_done $self;
		$$rbuf = '';
		$len - $cur;
	}
}

sub pass_trailers ($$) {
	my ($self, $tlr_buf) = @_;
	my $exp_tlr = delete $self->{tlr}; # TODO validate trailers
	$self->{http_out}->write($tlr_buf); # $tlr_buf includes final CRLF
	_pass_done $self;
}

sub pass_res_chunked ($) {
	my ($self) = @_;
	my $rbuf = $self->{rbuf} //= \(my $x = '');
	my $len = delete $self->{res_left};
	while (1) { # chunk start
		if ($len == CHUNK_TLR_END) {
			# $1: all trailers minus final CRLF
			if ($$rbuf =~ s/\A((?:
					(?:[a-z][a-z0-9\-]*:[ \t]* # key: LWS:
						| [ \t]+ # continuation LWS
					)[^\n]* # trailer value
					\n)* \r\n)//ismx) {
				return pass_trailers $self, $1;
			}
			die 'chunk not terminated' if length($$rbuf) > IO_SIZE;
		}
		if ($len == CHUNK_END) {
			if ($$rbuf =~ s/\A\r\n//s) {
				$self->{http_out}->write("\r\n");
				$len = CHUNK_START; # fall-through
			} elsif (length($$rbuf) > 2) {
				die 'CHUNK_END too long';
			}
		}
		if ($len == CHUNK_START) {
			if ($$rbuf =~ s/\A([a-f0-9]+)(.*?)\r\n//i) {
				die "chunk 0x$1 too large" if length($1) > 8;
				$len = hex $1;
				$self->{http_out}->write("$1$2\r\n");
			} elsif (length($$rbuf) > CHUNK_MAX_HDR) {
				die 'rbuf too large w/o CHUNK_START';
			} # else break from loop since $len >= 0
		}
		if ($len < 0) {
			pass_refill($self, IO_SIZE, \&pass_res_chunked, $len) or
				return;
			# (implicit) goto chunk_start if $r > 0;
		}
		$len = CHUNK_TLR_END if $len == 0;

		# pass the current chunk to client
		while ($len > 0) {
			if ($$rbuf ne '') {
				$len = _pass_res_block($self, $len) // return;
				if ($len == 0) {
					# we may have leftover data to parse
					# in chunk
					$len = CHUNK_END;
				} elsif ($len < 0) {
					die "BUG: len < 0: $len";
				} # len > 0: keep passing current chunk
			}
			if ($$rbuf eq '') {
				# read more of current chunk
				pass_refill($self, IO_SIZE,
					\&pass_res_chunked, $len) or return;
			}
		}
	}
}

sub pass_res_identity ($) {
	my ($self) = @_;
	my $rbuf = $self->{rbuf} //= \(my $x = '');
	my $len = delete $self->{res_left};
	while ($len > 0) {
		if ($$rbuf ne '') { # may clear $rbuf:
			$len = _pass_res_block($self, $len) // return;
		}
		if ($$rbuf eq '' && $len) {
			my $n = $len > IO_SIZE ? IO_SIZE : $len;
			pass_refill($self, $n, \&pass_res_identity, $len) or
				return;
		}
	}
	_pass_done $self;
}

sub pass_res_until_eof ($) { # HTTP/1.0-only
	my ($self) = @_;
	my $rbuf = $self->{rbuf} //= \(my $x = '');
	while (1) {
		if ($$rbuf ne '') {
			$self->{http_out}->write($$rbuf) or
				return _pass_done $self;
			$$rbuf = '';
		}
		if ($$rbuf eq '') {
			my $r = pass_refill($self, IO_SIZE,
					\&pass_res_until_eof, undef) // return;
			last if $r == 0; # done
		}
	}
	_pass_done $self;
}

sub pass_res_hdr ($) { # called by flush_write
	my ($self) = @_;
	my ($rbuf, $r, $code, $phdr);
	$rbuf = $self->{rbuf} // do {
		$self->do_read(\(my $buf = ''), IO_SIZE) or
			return read_err $self, \&pass_res_hdr;
		$self->{rbuf} = \$buf;
	};
	while (1) {
		($r, undef, $code, undef, $phdr) =
			parse_http_response($$rbuf, HEADERS_AS_ARRAYREF);
		last if $r > 0;
		if ($r == -2) { # incomplete
			length($$rbuf) > 0x4000 and
				die 'upstream response headers too large';
			$self->do_read($rbuf, IO_SIZE, length($$rbuf)) or return
				read_err $self, \&pass_res_hdr;
		} else {
			die "upstream sent bad response headers (r=$r)";
		}
	}
	substr $$rbuf, 0, $r, ''; # sv_chop off the header
	my (@clen, @tlr, $k, $v, @hdr);
	my $chunked = 0;
	my $res_hdr_excl = $self->{rproxy}->{res_hdr_excl};
	push @$phdr, @{$self->{rproxy}->{res_hdr_add} // []};
	while (@$phdr) {
		($k, $v) = splice @$phdr, 0, 2;
		next if $res_hdr_excl && $k =~ /$res_hdr_excl/;
		# parse_http_response strips CR from embedded CRLF,
		# HTTP::Server::PSGI and PublicInbox::HTTP will both
		# pass all whitespace in $v straight through
		$v =~ s/(?<!\r)\n/\r\n/sg;

		# not relying on \%special arg for parse_http_response since
		# it doesn't handle multi-value (bugs or malicious behavior)
		if ($k =~ /\AContent-Length\z/i) {
			push @clen, $v;
		} elsif ($k =~ /\ATransfer-Encoding\z/i) {
			++$chunked if $v =~ /\bchunked\b/i;
			# let client deal with stacked encodings
		} elsif ($k =~ /\ATrailer\z/i) {
			push @tlr, $v;
		} elsif ($k =~ /\A(?:Connection|Date|Keep-Alive|Upgrade)\z/i) {
			next;
		}
		push @hdr, $k, $v;
	}
	scalar(@clen) > 1 and
		die 'upstream set Content-Length '.scalar(@clen).' times';
	$chunked > 1 and
		die "upstream set Transfer-Encoding: chunked $chunked times";
	@tlr && !$chunked and
		die 'upstream sent Trailer w/o chunked response';
	my $wcb = delete $self->{wcb} // die 'BUG: no {wcb}';
	if ($self->{env}->{REQUEST_METHOD} eq 'HEAD') {
		$wcb->([ $code, \@hdr, [] ]);
		$self->close;
	} elsif ($chunked) {
		$self->{tlr} = join ',', @tlr;
		$self->{res_left} = CHUNK_START;
		$self->{http_out} = $wcb->([ $code, \@hdr ]);
		pass_res_chunked $self;
	} elsif ($clen[0]) { # non-zero Content-Length
		$clen[0] =~ /\A[0-9]+\z/ or
			die 'upstream sent invalid Content-Length: ',
				cstring($clen[0]);
		$self->{res_left} = $clen[0] + 0;
		$self->{http_out} = $wcb->([ $code, \@hdr ]);
		pass_res_identity $self;
	} elsif (defined $clen[0]) { # no body in response
		$wcb->([ $code, \@hdr, [] ]);
		$self->close;
	} else { # HTTP/1.0-only
		$self->{http_out} = $wcb->([ $code, \@hdr ]);
		pass_res_until_eof $self;
	}
}

sub send_req_body { # called by flush_write
	my ($self) = @_;
	# n.b. PublicInbox::HTTP always reads the entire request body
	# before dispatching the PSGI app
	my $r = $self->{req_left};
	$r = IO_SIZE if $r > IO_SIZE;
	$r = $self->{env}->{'psgi.input'}->read(my $buf, $r) //
			die "input->read: $! ($self->{req_left} bytes left)";
	die "input->read: EOF ($self->{req_left} bytes left)" if $r == 0;
	(($self->{req_left} -= $r) < 0) and
		die "BUG: req_left=$self->{req_left} is negative";
	$self->write(\$buf); # may add to $self->{wbuf}
	if ($self->{req_left}) {
		$self->requeue if push(@{$self->{wbuf}}, \&send_req_body) == 1;
	} else {
		delete $self->{req_left};
		$self->write(\&pass_res_hdr);
	}
}

sub event_step {
	my ($self) = @_;
	local $SIG{__WARN__} = $self->{env}->{'pi-httpd.warn_cb'};
	eval { return unless $self->flush_write && $self->{sock} };
	if ($@) {
		warn $@;
		$self->{env}->{'pi-httpd.client'}->close;
		$self->close;
	}
}

sub close {
	my ($self) = @_;
	$self->SUPER::close;
	my $wcb = delete $self->{wcb};
	$wcb->([502, [], []]) if $wcb;
}

1;
