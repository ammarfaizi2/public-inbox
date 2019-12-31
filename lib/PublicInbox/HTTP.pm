# Copyright (C) 2016-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Generic PSGI server for convenience.  It aims to provide
# a consistent experience for public-inbox admins so they don't have
# to learn different ways to admin both NNTP and HTTP components.
# There's nothing which depends on public-inbox, here.
# Each instance of this class represents a HTTP client socket

package PublicInbox::HTTP;
use strict;
use warnings;
use base qw(PublicInbox::DS);
use fields qw(httpd env input_left remote_addr remote_port forward alive);
use bytes (); # only for bytes::length
use Fcntl qw(:seek);
use Plack::HTTPParser qw(parse_http_request); # XS or pure Perl
use HTTP::Status qw(status_message);
use HTTP::Date qw(time2str);
use IO::Handle;
use PublicInbox::DS qw(msg_more);
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT);
use PublicInbox::Tmpfile;
use constant {
	CHUNK_START => -1,   # [a-f0-9]+\r\n
	CHUNK_END => -2,     # \r\n
	CHUNK_ZEND => -3,    # \r\n
	CHUNK_MAX_HDR => 256,
};
use Errno qw(EAGAIN);

my $pipelineq = [];
sub process_pipelineq () {
	my $q = $pipelineq;
	$pipelineq = [];
	foreach (@$q) {
		next unless $_->{sock};
		rbuf_process($_);
	}
}

# Use the same configuration parameter as git since this is primarily
# a slow-client sponge for git-http-backend
# TODO: support per-respository http.maxRequestBuffer somehow...
our $MAX_REQUEST_BUFFER = $ENV{GIT_HTTP_MAX_REQUEST_BUFFER} ||
			(10 * 1024 * 1024);

open(my $null_io, '<', '/dev/null') or die "failed to open /dev/null: $!";
my $http_date;
my $prev = 0;
sub http_date () {
	my $now = time;
	$now == $prev ? $http_date : ($http_date = time2str($prev = $now));
}

sub new ($$$) {
	my ($class, $sock, $addr, $httpd) = @_;
	my $self = fields::new($class);
	my $ev = EPOLLIN;
	my $wbuf;
	if (ref($sock) eq 'IO::Socket::SSL' && !$sock->accept_SSL) {
		return CORE::close($sock) if $! != EAGAIN;
		$ev = PublicInbox::TLS::epollbit();
		$wbuf = [ \&PublicInbox::DS::accept_tls_step ];
	}
	$self->SUPER::new($sock, $ev | EPOLLONESHOT);
	$self->{httpd} = $httpd;
	$self->{wbuf} = $wbuf if $wbuf;
	($self->{remote_addr}, $self->{remote_port}) =
		PublicInbox::Daemon::host_with_port($addr);
	$self;
}

sub event_step { # called by PublicInbox::DS
	my ($self) = @_;

	return unless $self->flush_write && $self->{sock};

	# only read more requests if we've drained the write buffer,
	# otherwise we can be buffering infinitely w/o backpressure

	return read_input($self) if defined $self->{env};
	my $rbuf = $self->{rbuf} // (\(my $x = ''));
	$self->do_read($rbuf, 8192, bytes::length($$rbuf)) or return;
	rbuf_process($self, $rbuf);
}

sub rbuf_process {
	my ($self, $rbuf) = @_;
	$rbuf //= $self->{rbuf} // (\(my $x = ''));

	my %env = %{$self->{httpd}->{env}}; # full hash copy
	my $r = parse_http_request($$rbuf, \%env);

	# We do not support Trailers in chunked requests, for now
	# (they are rarely-used and git (as of 2.7.2) does not use them)
	if ($r == -1 || $env{HTTP_TRAILER} ||
			# this length-check is necessary for PURE_PERL=1:
			($r == -2 && bytes::length($$rbuf) > 0x4000)) {
		return quit($self, 400);
	}
	if ($r < 0) { # incomplete
		$self->rbuf_idle($rbuf);
		return $self->requeue;
	}
	$$rbuf = substr($$rbuf, $r);
	my $len = input_prepare($self, \%env);
	defined $len or return write_err($self, undef); # EMFILE/ENFILE

	$len ? read_input($self, $rbuf) : app_dispatch($self, undef, $rbuf);
}

# IO::Handle::write returns boolean, this returns bytes written:
sub xwrite ($$$) {
	my ($fh, $rbuf, $max) = @_;
	my $w = bytes::length($$rbuf);
	$w = $max if $w > $max;
	$fh->write($$rbuf, $w) or return;
	$w;
}

sub read_input ($;$) {
	my ($self, $rbuf) = @_;
	$rbuf //= $self->{rbuf} // (\(my $x = ''));
	my $env = $self->{env};
	return if $env->{REMOTE_ADDR}; # in app dispatch
	return read_input_chunked($self, $rbuf) if env_chunked($env);

	# env->{CONTENT_LENGTH} (identity)
	my $len = delete $self->{input_left};
	my $input = $env->{'psgi.input'};

	while ($len > 0) {
		if ($$rbuf ne '') {
			my $w = xwrite($input, $rbuf, $len);
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
	$env->{REMOTE_ADDR} = $self->{remote_addr};
	$env->{REMOTE_PORT} = $self->{remote_port};
	if (my $host = $env->{HTTP_HOST}) {
		$host =~ s/:([0-9]+)\z// and $env->{SERVER_PORT} = $1;
		$env->{SERVER_NAME} = $host;
	}
	if (defined $input) {
		sysseek($input, 0, SEEK_SET) or
			die "BUG: psgi.input seek failed: $!";
	}
	# note: NOT $self->{sock}, we want our close (+ PublicInbox::DS::close),
	# to do proper cleanup:
	$env->{'psgix.io'} = $self; # for ->close or async_pass
	my $res = Plack::Util::run_app($self->{httpd}->{app}, $env);
	eval {
		if (ref($res) eq 'CODE') {
			$res->(sub { response_write($self, $env, $_[0]) });
		} else {
			response_write($self, $env, $res);
		}
	};
	$self->close if $@;
}

sub response_header_write {
	my ($self, $env, $res) = @_;
	my $proto = $env->{SERVER_PROTOCOL} or return; # HTTP/0.9 :P
	my $status = $res->[0];
	my $h = "$proto $status " . status_message($status) . "\r\n";
	my ($len, $chunked);
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
	my $term = defined($len) || $chunked;
	my $prot_persist = ($proto eq 'HTTP/1.1') && ($conn !~ /\bclose\b/i);
	my $alive;
	if (!$term && $prot_persist) { # auto-chunk
		$chunked = $alive = 2;
		$h .= "Transfer-Encoding: chunked\r\n";
		# no need for "Connection: keep-alive" with HTTP/1.1
	} elsif ($term && ($prot_persist || ($conn =~ /\bkeep-alive\b/i))) {
		$alive = 1;
		$h .= "Connection: keep-alive\r\n";
	} else {
		$alive = 0;
		$h .= "Connection: close\r\n";
	}
	$h .= 'Date: ' . http_date() . "\r\n\r\n";

	if (($len || $chunked) && $env->{REQUEST_METHOD} ne 'HEAD') {
		msg_more($self, $h);
	} else {
		$self->write(\$h);
	}
	$alive;
}

# middlewares such as Deflater may write empty strings
sub chunked_write ($$) {
	my $self = $_[0];
	return if $_[1] eq '';
	msg_more($self, sprintf("%x\r\n", bytes::length($_[1])));
	msg_more($self, $_[1]);

	# use $self->write(\"\n\n") if you care about real-time
	# streaming responses, public-inbox WWW does not.
	msg_more($self, "\r\n");
}

sub identity_write ($$) {
	my $self = $_[0];
	$self->write(\($_[1])) if $_[1] ne '';
}

sub next_request ($) {
	my ($self) = @_;
	if ($self->{rbuf}) {
		# avoid recursion for pipelined requests
		PublicInbox::DS::requeue(\&process_pipelineq) if !@$pipelineq;
		push @$pipelineq, $self;
	} else { # wait for next request
		$self->requeue;
	}
}

sub response_done {
	my ($self, $alive) = @_;
	delete $self->{env}; # we're no longer busy
	$self->write(\"0\r\n\r\n") if $alive == 2;
	$self->write($alive ? \&next_request : \&close);
}

sub getline_pull {
	my ($self) = @_;
	my $forward = $self->{forward};

	# limit our own running time for fairness with other
	# clients and to avoid buffering too much:
	my $buf = eval {
		local $/ = \8192;
		$forward->getline;
	} if $forward;

	if (defined $buf) {
		# may close in PublicInbox::DS::write
		if ($self->{alive} == 2) {
			chunked_write($self, $buf);
		} else {
			identity_write($self, $buf);
		}

		if ($self->{sock}) {
			my $wbuf = $self->{wbuf} //= [];
			push @$wbuf, \&getline_pull;

			# wbuf may be populated by {chunked,identity}_write()
			# above, no need to rearm if so:
			$self->requeue if scalar(@$wbuf) == 1;
			return; # likely
		}
	} elsif ($@) {
		err($self, "response ->getline error: $@");
		$self->close;
	}
	# avoid recursion
	if (delete $self->{forward}) {
		eval { $forward->close };
		if ($@) {
			err($self, "response ->close error: $@");
			$self->close; # idempotent
		}
	}
	response_done($self, delete $self->{alive});
}

sub response_write {
	my ($self, $env, $res) = @_;
	my $alive = response_header_write($self, $env, $res);
	if (defined(my $body = $res->[2])) {
		if (ref $body eq 'ARRAY') {
			if ($alive == 2) {
				chunked_write($self, $_) for @$body;
			} else {
				identity_write($self, $_) for @$body;
			}
			response_done($self, $alive);
		} else {
			$self->{forward} = $body;
			$self->{alive} = $alive;
			getline_pull($self); # kick-off!
		}
	# these are returned to the calling application:
	} elsif ($alive == 2) {
		bless [ $self, $alive ], 'PublicInbox::HTTP::Chunked';
	} else {
		bless [ $self, $alive ], 'PublicInbox::HTTP::Identity';
	}
}

sub input_tmpfile ($) {
	my $input = tmpfile('http.input', $_[0]->{sock}) or return;
	$input->autoflush(1);
	$input;
}

sub input_prepare {
	my ($self, $env) = @_;
	my $input;
	my $len = $env->{CONTENT_LENGTH};
	if ($len) {
		if ($len > $MAX_REQUEST_BUFFER) {
			quit($self, 413);
			return;
		}
		$input = input_tmpfile($self);
	} elsif (env_chunked($env)) {
		$len = CHUNK_START;
		$input = input_tmpfile($self);
	} else {
		$input = $null_io;
	}

	# TODO: expire idle clients on ENFILE / EMFILE
	return unless $input;

	$env->{'psgi.input'} = $input;
	$self->{env} = $env;
	$self->{input_left} = $len || 0;
}

sub env_chunked { ($_[0]->{HTTP_TRANSFER_ENCODING} || '') =~ /\bchunked\b/i }

sub err ($$) {
	eval { $_[0]->{httpd}->{env}->{'psgi.errors'}->print($_[1]."\n") };
}

sub write_err {
	my ($self, $len) = @_;
	my $msg = $! || '(zero write)';
	$msg .= " ($len bytes remaining)" if defined $len;
	err($self, "error buffering to input: $msg");
	quit($self, 500);
}

sub recv_err {
	my ($self, $len) = @_;
	if ($! == EAGAIN) { # epoll/kevent watch already set by do_read
		$self->{input_left} = $len;
	} else {
		err($self, "error reading input: $! ($len bytes remaining)");
	}
}

sub read_input_chunked { # unlikely...
	my ($self, $rbuf) = @_;
	$rbuf //= $self->{rbuf} // (\(my $x = ''));
	my $input = $self->{env}->{'psgi.input'};
	my $len = delete $self->{input_left};

	while (1) { # chunk start
		if ($len == CHUNK_ZEND) {
			$$rbuf =~ s/\A\r\n//s and
				return app_dispatch($self, $input, $rbuf);

			return quit($self, 400) if bytes::length($$rbuf) > 2;
		}
		if ($len == CHUNK_END) {
			if ($$rbuf =~ s/\A\r\n//s) {
				$len = CHUNK_START;
			} elsif (bytes::length($$rbuf) > 2) {
				return quit($self, 400);
			}
		}
		if ($len == CHUNK_START) {
			if ($$rbuf =~ s/\A([a-f0-9]+).*?\r\n//i) {
				$len = hex $1;
				if (($len + -s $input) > $MAX_REQUEST_BUFFER) {
					return quit($self, 413);
				}
			} elsif (bytes::length($$rbuf) > CHUNK_MAX_HDR) {
				return quit($self, 400);
			}
			# will break from loop since $len >= 0
		}

		if ($len < 0) { # chunk header is trickled, read more
			$self->do_read($rbuf, 8192, bytes::length($$rbuf)) or
				return recv_err($self, $len);
			# (implicit) goto chunk_start if $r > 0;
		}
		$len = CHUNK_ZEND if $len == 0;

		# drain the current chunk
		until ($len <= 0) {
			if ($$rbuf ne '') {
				my $w = xwrite($input, $rbuf, $len);
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
	my $h = "HTTP/1.1 $status " . status_message($status) . "\r\n\r\n";
	$self->write(\$h);
	$self->close;
}

sub close {
	my $self = $_[0];
	delete $self->{env}; # prevent circular references
	if (my $forward = delete $self->{forward}) {
		eval { $forward->close };
		err($self, "forward ->close error: $@") if $@;
	}
	$self->SUPER::close; # PublicInbox::DS::close
}

# for graceful shutdown in PublicInbox::Daemon:
sub busy () {
	my ($self) = @_;
	($self->{rbuf} || $self->{env} || $self->{wbuf});
}

# Chunked and Identity packages are used for writing responses.
# They may be exposed to the PSGI application when the PSGI app
# returns a CODE ref for "push"-based responses
package PublicInbox::HTTP::Chunked;
use strict;

sub write {
	# ([$http], $buf) = @_;
	PublicInbox::HTTP::chunked_write($_[0]->[0], $_[1])
}

sub close {
	# $_[0] = [$http, $alive]
	PublicInbox::HTTP::response_done(@{$_[0]});
}

package PublicInbox::HTTP::Identity;
use strict;
our @ISA = qw(PublicInbox::HTTP::Chunked);

sub write {
	# ([$http], $buf) = @_;
	PublicInbox::HTTP::identity_write($_[0]->[0], $_[1]);
}

1;
