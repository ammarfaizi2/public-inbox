# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Generic PSGI server for convenience.  It aims to provide
# a consistent experience for public-inbox admins so they don't have
# to learn different ways to admin both NNTP and HTTP components.
# There's nothing which depends on public-inbox, here.
# Each instance of this class represents a HTTP client socket
#
# fields:
# httpd: PublicInbox::HTTPD ref
# env: PSGI env hashref
# input_left: bytes left to read in request body (e.g. POST/PUT)
# remote_addr: remote IP address as a string (e.g. "127.0.0.1")
# remote_port: peer port
# forward: response body object, response to ->getline + ->close
# alive: HTTP keepalive state:
#	0: drop connection when done
#	1: keep connection when done
#	2: keep connection, chunk responses
package PublicInbox::HTTP;
use strict;
use parent qw(PublicInbox::DS);
use bytes qw(length);
use autodie qw(open);
use Fcntl qw(:seek);
use Plack::HTTPParser qw(parse_http_request); # XS or pure Perl
use Plack::Util;
use HTTP::Status qw(status_message);
use HTTP::Date qw(time2str);
use PublicInbox::DS qw(msg_more);
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT);
use PublicInbox::Tmpfile;
use constant {
	CHUNK_START => -1,   # [a-f0-9]+\r\n
	CHUNK_END => -2,     # \r\n
	CHUNK_TLR_END => -3, # (trailers*)?\r\n
	CHUNK_MAX_HDR => 256,
};
use Errno qw(EAGAIN);
use PublicInbox::Compat qw(sum0);
my $NOT_TRAILER = join '|', (qw(Content-Length Content-Type),
	qw(Trailer Transfer-Encoding Content-Encoding Content-Range),
	# RFC 7231 5.1 controls:
	qw(Cache-Control Expect Host Max-Forwards Pragma Range TE),
	# RFC 7231 5.2 conditionals
	qw(If-Match If-None-Match
		If-Modified-Since If-Unmodified-Since If-Range),
	qw(Authorization Proxy-Authorization), # RFC 7235
	'Cookie' # RFC 6265
	# ignoring 7231 7.1 control data since those are for responses
);

# Use the same configuration parameter as git since this is primarily
# a slow-client sponge for git-http-backend
# TODO: support per-respository http.maxRequestBuffer somehow...
our $MAX_REQUEST_BUFFER = $ENV{GIT_HTTP_MAX_REQUEST_BUFFER} ||
			(10 * 1024 * 1024);

open my $null_io, '<', '/dev/null';
{
	my @n = stat($null_io) or die "stat(/dev/null): $!";
	my @i = stat(STDIN) or die "stat(STDIN): $!";
	$null_io = \*STDIN if "@n[0, 1]" eq "@i[0, 1]";
}

my $http_date;
my $prev = 0;
sub http_date () {
	my $now = time;
	$now == $prev ? $http_date : ($http_date = time2str($prev = $now));
}

sub new ($$$) {
	my ($class, $sock, $addr, $srv_env) = @_;
	my $self = bless { srv_env => $srv_env }, $class;
	my $ev = EPOLLIN;
	my $wbuf;
	if ($sock->can('accept_SSL') && !$sock->accept_SSL) {
		return $sock->close if $! != EAGAIN;
		$ev = PublicInbox::TLS::epollbit() or return $sock->close;
		$wbuf = [ \&PublicInbox::DS::accept_tls_step ];
	}
	$self->{wbuf} = $wbuf if $wbuf;
	($addr, $self->{remote_port}) =
		PublicInbox::Daemon::host_with_port($addr);
	$self->{remote_addr} = $addr if $addr ne '127.0.0.1';
	$self->SUPER::new($sock, $ev | EPOLLONESHOT);
}

sub event_step { # called by PublicInbox::DS
	my ($self) = @_;
	local $SIG{__WARN__} = $self->{srv_env}->{'pi-httpd.warn_cb'};
	return unless $self->flush_write && $self->{sock} && !$self->{forward};

	# only read more requests if we've drained the write buffer,
	# otherwise we can be buffering infinitely w/o backpressure

	return read_input($self) if ref($self->{env});

	my $rbuf = $self->{rbuf} // (\(my $x = ''));
	my %env = %{$self->{srv_env}}; # full hash copy
	my $r;
	while (($r = parse_http_request($$rbuf, \%env)) < 0) {
		# this length-check is necessary for PURE_PERL=1:
		($r == -1 || ($r == -2 && length($$rbuf) > 0x4000)) and
			return quit($self, 400);
		$self->do_read($rbuf, 8192, length($$rbuf)) or return;
	}
	return quit($self, 400) if grep(/\s/, keys %env); # stop smugglers
	$$rbuf = substr($$rbuf, $r);
	my $len = input_prepare($self, \%env) //
		return write_err($self, undef); # EMFILE/ENFILE

	$len ? read_input($self, $rbuf) : app_dispatch($self, undef, $rbuf);
}

sub read_input ($;$) {
	my ($self, $rbuf) = @_;
	$rbuf //= $self->{rbuf} // (\(my $x = ''));
	my $env = $self->{env};
	return read_input_chunked($self, $rbuf) if env_chunked($env);

	# env->{CONTENT_LENGTH} (identity)
	my $len = delete $self->{input_left};
	my $input = $env->{'psgi.input'};

	while ($len > 0) {
		if ($$rbuf ne '') {
			my $w = syswrite($input, $$rbuf, $len);
			return write_err($self, $len) unless $w;
			$len -= $w;
			die "BUG: $len < 0 (w=$w)" if $len < 0;
			if ($len == 0) { # next request may be pipelined
				$$rbuf = substr($$rbuf, $w);
				last;
			}
			$$rbuf = '';
		}
		$self->do_read($rbuf, 8192) or return recv_err($self, $len);
		# continue looping if $r > 0;
	}
	app_dispatch($self, $input, $rbuf);
}

sub app_dispatch {
	my ($self, $input, $rbuf) = @_;
	$self->rbuf_idle($rbuf);
	my $env = $self->{env};
	$self->{env} = undef; # for exists() check in ->busy
	$env->{REMOTE_ADDR} = $self->{remote_addr} // '127.0.0.1';
	$env->{REMOTE_PORT} = $self->{remote_port};
	if (defined(my $host = $env->{HTTP_HOST})) {
		$host =~ s/:([0-9]+)\z// and $env->{SERVER_PORT} = $1 + 0;
		$env->{SERVER_NAME} = $host;
	}
	sysseek($input, 0, SEEK_SET) if defined $input;
	# note: NOT $self->{sock}, we want our close (+ PublicInbox::DS::close),
	# to do proper cleanup:
	$env->{'psgix.io'} = $self->{sock}; # for pi-httpd.ckhup
	$env->{'pi-httpd.client'} = $self; # for ->close or async_pass
	my $res = Plack::Util::run_app($env->{'pi-httpd.app'}, $env);
	eval {
		if (ref($res) eq 'CODE') {
			$res->(sub { response_write($self, $env, $_[0]) });
		} else {
			response_write($self, $env, $res);
		}
	};
	if ($@) {
		warn "response_write error: $@";
		$self->close;
	}
}

# we use the non-standard 499 code for client disconnects (matching nginx)
sub status_msg ($) { status_message($_[0]) // 'error' }

sub response_header_write ($$$) {
	my ($self, $env, $res) = @_;
	my $proto = $env->{SERVER_PROTOCOL} or return; # HTTP/0.9 :P
	my $status = $res->[0];
	my $h = "$proto $status " . status_msg($status) . "\r\n";
	my ($len, $chunked, $alive, $res_body, $req_head);
	my $headers = $res->[1];

	for (my $i = 0; $i < @$headers; $i += 2) {
		my $k = $headers->[$i];
		my $v = $headers->[$i + 1];
		next if $k =~ /\A(?:Connection|Date)\z/i;

		$len = $v if $k =~ /\AContent-Length\z/i;
		if ($k =~ /\ATransfer-Encoding\z/i && $v =~ /\bchunked\b/i) {
			$chunked = 1;
		}
		$h .= "$k: $v\r\n";
	}

	my $conn = $env->{HTTP_CONNECTION} || '';
	my $term = defined($len) || $chunked ||
		Plack::Util::status_with_no_entity_body($status);
	my $prot_persist = ($proto eq 'HTTP/1.1');
	if (!$term && ref($res->[2]) eq 'ARRAY') {
		($res_body, $res->[2]) = ($res->[2], []);
		$len = sum0(map length, @$res_body);
		$h .= "Content-Length: $len\r\n";
		$term = 1;
	}
	if ($conn =~ /\bclose\b/i) {
		$alive = 0;
		$h .= "Connection: close\r\n";
	}
	$req_head = $env->{REQUEST_METHOD} eq 'HEAD';
	if (!$term && $prot_persist) {
		$h .= "Transfer-Encoding: chunked\r\n";
		$chunked //= 2 if !$req_head; # auto-chunk
		$alive //= 1;
		# no need for "Connection: keep-alive" with HTTP/1.1
	} elsif ($term && ($prot_persist || ($conn =~ /\bkeep-alive\b/i))) {
		$alive //= 1 and $h .= "Connection: keep-alive\r\n";
	} elsif (!defined $alive) {
		$alive = 0;
		$h .= "Connection: close\r\n";
	}
	$h .= 'Date: ' . http_date() . "\r\n\r\n";

	if ($res_body) {
		$self->writev($h, @$res_body);
	} elsif (($len || $chunked) && !$req_head) {
		msg_more($self, $h);
	} else {
		$self->write(\$h);
	}
	$self->{alive} = $alive;
	($chunked // 0) == 2 ? ($self->{do_chunk} = 1) : undef;
}

# middlewares such as Deflater may write empty strings
sub chunked_write ($$) {
	my ($self, $buf) = @_;
	$buf eq '' or
		msg_more $self, sprintf("%x\r\n", length($buf)), $buf, "\r\n";
}

sub identity_write ($$) {
	my $self = $_[0];
	$self->write(\($_[1])) if $_[1] ne '';
}

sub response_done ($) {
	my ($self) = @_;
	if (my $forward = delete $self->{forward}) { # avoid recursion
		eval { $forward->close };
		if ($@) {
			warn "response forward->close error: $@";
			return $self->close; # idempotent
		}
	}
	delete $self->{env}; # we're no longer busy (env == undef here)
	my ($alive, $do_chunk) = delete @$self{qw(alive do_chunk)};
	$self->write(\"0\r\n\r\n") if $do_chunk;
	$self->write(\&close) if !$alive;
	$self->requeue if $alive && !$self->{wbuf};
}

sub getline_pull {
	my ($self) = @_;
	my $forward = $self->{forward};

	# limit our own running time for fairness with other
	# clients and to avoid buffering too much:
	my $buf = eval {
		local $/ = \65536;
		$forward->getline;
	} if $forward;

	if (defined $buf) {
		# may close in PublicInbox::DS::write
		if ($self->{do_chunk}) {
			chunked_write($self, $buf);
		} else {
			identity_write($self, $buf);
		}

		if ($self->{sock}) {
			# autovivify wbuf
			my $new_size = push(@{$self->{wbuf}}, \&getline_pull);

			# wbuf may be populated by {chunked,identity}_write()
			# above, no need to rearm if so:
			$self->requeue if $new_size == 1;
			return; # likely
		}
	} elsif ($@) {
		warn "response ->getline error: $@";
		return $self->close;
	}
	response_done($self);
}

sub response_write {
	my ($self, $env, $res) = @_;
	my $do_chunk = response_header_write($self, $env, $res);
	if (defined(my $body = $res->[2])) {
		if (ref $body eq 'ARRAY') {
			if ($do_chunk) {
				chunked_write($self, $_) for @$body;
			} else {
				identity_write($self, $_) for @$body;
			}
			response_done($self);
		} else {
			$self->{forward} = $body;
			getline_pull($self); # kick-off!
		}
	# these are returned to the calling application:
	} elsif ($do_chunk) {
		bless \$self, 'PublicInbox::HTTP::Chunked';
	} else {
		bless \$self, 'PublicInbox::HTTP::Identity';
	}
}

sub input_prepare {
	my ($self, $env) = @_;
	my ($input, $len);

	# rfc 7230 3.3.2, 3.3.3,: favor Transfer-Encoding over Content-Length
	my $hte = $env->{HTTP_TRANSFER_ENCODING};
	my $tlr = $env->{HTTP_TRAILER};
	if (defined $hte) {
		# rfc7230 3.3.3, point 3 says only chunked is accepted
		# as the final encoding.  Since neither public-inbox-httpd,
		# git-http-backend, or our WWW-related code uses "gzip",
		# "deflate" or "compress" as the Transfer-Encoding, we'll
		# reject them:
		return quit($self, 400) if $hte !~ /\Achunked\z/i ||
				(defined($tlr) && grep(/\A($NOT_TRAILER)\z/,
						       split /\s*,\s*/s, $tlr));
		$len = CHUNK_START;
		$input = tmpfile('http.input', $self->{sock});
	} else {
		# while (AFAIK) no RFC says Trailer: is explicitly disallowed
		# w/o `Transfer-Encoding: chunked', allowing it makes no sense
		# and it could be a confusion attack to downstream proxies
		return quit($self, 400) if defined $tlr;
		$len = $env->{CONTENT_LENGTH};
		if (defined $len) {
			# rfc7230 3.3.3.4
			return quit($self, 400) if $len !~ /\A[0-9]+\z/;
			return quit($self, 413) if $len > $MAX_REQUEST_BUFFER;
			$input = $len ? tmpfile('http.input', $self->{sock})
				: $null_io;
		} else {
			$input = $null_io;
		}
	}
	# curl sends `Expect: 100-continue' by default on uploads and our
	# buffer-in-full behavior can't delegate the decision to apps, so
	# assume we're OK with the client continuing at this point
	if (($env->{HTTP_EXPECT} // '') =~ /\A100-continue\z/i) {
		$self->write($env->{SERVER_PROTOCOL} .
				" 100 Continue\r\n\r\n");
		$self->{sock} or return;
	}
	# TODO: expire idle clients on ENFILE / EMFILE
	$env->{'psgi.input'} = $input // return;
	$self->{env} = $env;
	$self->{input_left} = $len || 0;
}

sub env_chunked { ($_[0]->{HTTP_TRANSFER_ENCODING} // '') =~ /\Achunked\z/i }

sub write_err {
	my ($self, $len) = @_;
	my $msg = $! || '(zero write)';
	$msg .= " ($len bytes remaining)" if defined $len;
	warn "error buffering to input: $msg";
	quit($self, 500);
}

sub recv_err {
	my ($self, $len) = @_;
	if ($! == EAGAIN) { # epoll/kevent watch already set by do_read
		$self->{input_left} = $len;
	} else {
		warn "error reading input: $! ($len bytes remaining)";
	}
}

sub merge_trailers ($$) {
	my ($self, $tlr_buf) = @_;
	my $env = $self->{env};
	my $exp_tlr = $env->{HTTP_TRAILER};
	return quit($self, 400) if !!$tlr_buf ne !!$exp_tlr;
	$exp_tlr // return 1;
	# copy expected entries from existing $env for append, if any
	my @k = map { tr/-/_/; "HTTP_\U$_" } split /\s*,\s*/s, $exp_tlr;
	# there's no public API in Plack to parse w/o the request line,
	# so we make it look like a new request and avoid clobbering $env
	my %tenv = map { defined($env->{$_}) ? ($_ => $env->{$_}) : () } @k;
	substr($tlr_buf, 0, 0) = "GET / HTTP/1.0\r\n";
	my $r = parse_http_request($tlr_buf .= "\r\n", \%tenv);
	if ($r <= 0 || scalar @tenv{qw(CONTENT_TYPE CONTENT_LENGTH)}) {
		warn 'BUG: incomplete trailer (non-fatal)' if $r == -2;
		return quit($self, 400);
	}
	my %need = map { $_ => 1 } @k;
	for my $k (grep /^HTTP_/, keys %tenv) {
		# maybe the client sent more than promised:
		delete $need{$k} // return quit($self, 400);
		$env->{$k} = delete $tenv{$k};
	}
	# maybe the client sent less than promised...
	keys %need ? quit($self, 400) : 1;
}

sub read_input_chunked { # unlikely...
	my ($self, $rbuf) = @_;
	$rbuf //= $self->{rbuf} // (\(my $x = ''));
	my $input = $self->{env}->{'psgi.input'};
	my $len = delete $self->{input_left};

	while (1) { # chunk start
		if ($len == CHUNK_TLR_END) {
			# $1: all trailers minus final CRLF
			if ($$rbuf =~ s/\A((?:
					(?:[a-z][a-z0-9\-]*:[ \t]* # key: LWS
						| [ \t]+ # continuation LWS
					)[^\n]* # trailer value
					\n)* )\r\n//ismx) {
				return if !merge_trailers($self, $1);
				return app_dispatch($self, $input, $rbuf);
			}
			return quit($self, 400) if length($$rbuf) > 0x4000;
		}
		if ($len == CHUNK_END) {
			if ($$rbuf =~ s/\A\r\n//s) {
				$len = CHUNK_START;
			} elsif (length($$rbuf) > 2) {
				return quit($self, 400);
			}
		}
		if ($len == CHUNK_START) {
			if ($$rbuf =~ s/\A([a-f0-9]+).*?\r\n//i) {
				return quit($self, 400) if length($1) > 8;
				$len = hex $1;
				if (($len + -s $input) > $MAX_REQUEST_BUFFER) {
					return quit($self, 413);
				}
			} elsif (length($$rbuf) > CHUNK_MAX_HDR) {
				return quit($self, 400);
			}
			# will break from loop since $len >= 0
		}

		if ($len < 0) { # chunk header is trickled, read more
			$self->do_read($rbuf, 8192, length($$rbuf)) or
				return recv_err($self, $len);
			# (implicit) goto chunk_start if $r > 0;
		}
		$len = CHUNK_TLR_END if $len == 0;

		# drain the current chunk
		until ($len <= 0) {
			if ($$rbuf ne '') {
				my $w = syswrite($input, $$rbuf, $len);
				return write_err($self, "$len chunk") if !$w;
				$len -= $w;
				if ($len == 0) {
					# we may have leftover data to parse
					# in chunk
					$$rbuf = substr($$rbuf, $w);
					$len = CHUNK_END;
				} elsif ($len < 0) {
					die "BUG: len < 0: $len";
				} else {
					$$rbuf = '';
				}
			}
			if ($$rbuf eq '') {
				# read more of current chunk
				$self->do_read($rbuf, 8192) or
					return recv_err($self, $len);
			}
		}
	}
}

sub quit {
	my ($self, $status) = @_;
	my $h = "HTTP/1.1 $status " . status_msg($status) . "\r\n\r\n";
	$self->write(\$h);
	$self->close;
	undef; # input_prepare expects this
}

sub close {
	my $self = $_[0];
	if (my $forward = delete $self->{forward}) {
		eval { $forward->close };
		warn "forward ->close error: $@" if $@;
	}
	$self->SUPER::close; # PublicInbox::DS::close
}

sub busy { # for graceful shutdown in PublicInbox::Daemon:
	my ($self) = @_;
	defined($self->{rbuf}) || exists($self->{env}) || defined($self->{wbuf})
}

# runs $cb on the next iteration of the event loop at earliest
sub next_step {
	my ($self, $cb) = @_;
	return unless exists $self->{sock};
	$self->requeue if 1 == push(@{$self->{wbuf}}, $cb);
}

# Chunked and Identity packages are used for writing responses.
# They may be exposed to the PSGI application when the PSGI app
# returns a CODE ref for "push"-based responses
package PublicInbox::HTTP::Chunked;
use v5.12;

sub write {
	my $http = ${$_[0]};
	PublicInbox::HTTP::chunked_write($http, $_[1]);
	$http->{sock} ? bytes::length($_[1]) : undef;
}

sub close { PublicInbox::HTTP::response_done ${$_[0]} }

package PublicInbox::HTTP::Identity;
use v5.12;
our @ISA = qw(PublicInbox::HTTP::Chunked);

sub write {
	my $http = ${$_[0]};
	PublicInbox::HTTP::identity_write($http, $_[1]);
	$http->{sock} ? bytes::length($_[1]) : undef;
}

1;
