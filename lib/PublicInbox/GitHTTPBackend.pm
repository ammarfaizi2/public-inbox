# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# when no endpoints match, fallback to this and serve a static file
# or smart HTTP
package PublicInbox::GitHTTPBackend;
use strict;
use warnings;
use Fcntl qw(:seek);
use POSIX qw(dup2);

# n.b. serving "description" and "cloneurl" should be innocuous enough to
# not cause problems.  serving "config" might...
my @text = qw[HEAD info/refs
	objects/info/(?:http-alternates|alternates|packs)
	cloneurl description];

my @binary = qw!
	objects/[a-f0-9]{2}/[a-f0-9]{38}
	objects/pack/pack-[a-f0-9]{40}\.(?:pack|idx)
	!;

our $ANY = join('|', @binary, @text);
my $BIN = join('|', @binary);
my $TEXT = join('|', @text);

sub r {
	[ $_[0] , [qw(Content-Type text/plain Content-Length 0) ], [] ]
}

sub serve {
	my ($cgi, $git, $path) = @_;
	my $service = $cgi->param('service') || '';
	if ($service =~ /\Agit-\w+-pack\z/ || $path =~ /\Agit-\w+-pack\z/) {
		my $ok = serve_smart($cgi, $git, $path);
		return $ok if $ok;
	}

	my $type;
	if ($path =~ /\A(?:$BIN)\z/o) {
		$type = 'application/octet-stream';
	} elsif ($path =~ /\A(?:$TEXT)\z/o) {
		$type = 'text/plain';
	} else {
		return r(404);
	}
	my $f = "$git->{git_dir}/$path";
	return r(404) unless -f $f && -r _;
	my @st = stat(_);
	my $size = $st[7];

	# TODO: If-Modified-Since and Last-Modified
	open my $in, '<', $f or return r(404);
	my $code = 200;
	my $len = $size;
	my @h;

	my $env = $cgi->{env};
	my $range = $env->{HTTP_RANGE};
	if (defined $range && $range =~ /\bbytes=(\d*)-(\d*)\z/) {
		($code, $len) = prepare_range($cgi, $in, \@h, $1, $2, $size);
		if ($code == 416) {
			push @h, 'Content-Range', "bytes */$size";
			return [ 416, \@h, [] ];
		}
	}

	push @h, 'Content-Type', $type, 'Content-Length', $len;
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([ $code, \@h ]);
		my $buf;
		my $n = 8192;
		while ($len > 0) {
			$n = $len if $len < $n;
			my $r = read($in, $buf, $n);
			last if (!defined($r) || $r <= 0);
			$len -= $r;
			$fh->write($buf);
		}
		$fh->close;
	}
}

sub prepare_range {
	my ($cgi, $in, $h, $beg, $end, $size) = @_;
	my $code = 200;
	my $len = $size;
	if ($beg eq '') {
		if ($end ne '') { # "bytes=-$end" => last N bytes
			$beg = $size - $end;
			$beg = 0 if $beg < 0;
			$end = $size - 1;
			$code = 206;
		} else {
			$code = 416;
		}
	} else {
		if ($beg > $size) {
			$code = 416;
		} elsif ($end eq '' || $end >= $size) {
			$end = $size - 1;
			$code = 206;
		} elsif ($end < $size) {
			$code = 206;
		} else {
			$code = 416;
		}
	}
	if ($code == 206) {
		$len = $end - $beg + 1;
		if ($len <= 0) {
			$code = 416;
		} else {
			seek($in, $beg, SEEK_SET) or return [ 500, [], [] ];
			push @$h, qw(Accept-Ranges bytes Content-Range);
			push @$h, "bytes $beg-$end/$size";

			# FIXME: Plack::Middleware::Deflater bug?
			$cgi->{env}->{'psgix.no-compress'} = 1;
		}
	}
	($code, $len);
}

# returns undef if 403 so it falls back to dumb HTTP
sub serve_smart {
	my ($cgi, $git, $path) = @_;
	my $env = $cgi->{env};

	my $input = $env->{'psgi.input'};
	my $buf;
	my $in;
	my $err = $env->{'psgi.errors'};
	if (fileno($input) >= 0) {
		$in = $input;
	} else { # FIXME untested
		$in = IO::File->new_tmpfile;
		while (1) {
			my $r = $input->read($buf, 8192);
			unless (defined $r) {
				$err->print('error reading input: ', $!, "\n");
				return r(500);
			}
			last if ($r == 0);
			$in->write($buf);
		}
		$in->flush;
		$in->sysseek(0, SEEK_SET);
	}
	my ($rpipe, $wpipe);
	unless (pipe($rpipe, $wpipe)) {
		$err->print('error creating pipe', $!, "\n");
		return r(500);
	}
	my $pid = fork; # TODO: vfork under Linux...
	unless (defined $pid) {
		$err->print('error forking: ', $!, "\n");
		return r(500);
	}
	my $git_dir = $git->{git_dir};
	if ($pid == 0) {
		# GIT_HTTP_EXPORT_ALL, GIT_COMMITTER_NAME, GIT_COMMITTER_EMAIL
		# may be set in the server-process and are passed as-is
		foreach my $name (qw(QUERY_STRING
					REMOTE_USER REMOTE_ADDR
					HTTP_CONTENT_ENCODING
					CONTENT_TYPE
					SERVER_PROTOCOL
					REQUEST_METHOD)) {
			my $val = $env->{$name};
			$ENV{$name} = $val if defined $val;
		}
		# $ENV{GIT_PROJECT_ROOT} = $git->{git_dir};
		$ENV{GIT_HTTP_EXPORT_ALL} = '1';
		$ENV{PATH_TRANSLATED} = "$git_dir/$path";
		dup2(fileno($in), 0) or die "redirect stdin failed: $!\n";
		dup2(fileno($wpipe), 1) or die "redirect stdout failed: $!\n";
		my @cmd = qw(git http-backend);
		exec(@cmd) or die 'exec `' . join(' ', @cmd). "' failed: $!\n";
	}
	$wpipe = $in = undef;
	$buf = '';
	my ($vin, $fh, $res);
	my $end = sub {
		if ($fh) {
			$fh->close;
			$fh = undef;
		} else {
			$res->(r(500)) if $res;
		}
		if ($rpipe) {
			$rpipe->close; # _may_ be Danga::Socket::close
			$rpipe = undef;
		}
		if (defined $pid) {
			my $wpid = $pid;
			$pid = undef;
			return if $wpid == waitpid($wpid, 0);
			$err->print("git http-backend ($git_dir): $?\n");
		}
	};
	my $fail = sub {
		my ($e) = @_;
		if ($e eq 'EAGAIN') {
			select($vin, undef, undef, undef) if defined $vin;
			# $vin is undef on async, so this is a noop on EAGAIN
			return;
		}
		$end->();
		$err->print("git http-backend ($git_dir): $e\n");
	};
	my $cb = sub { # read git-http-backend output and stream to client
		my $r = $rpipe ? $rpipe->sysread($buf, 8192, length($buf)) : 0;
		return $fail->($!{EAGAIN} ? 'EAGAIN' : $!) unless defined $r;
		return $end->() if $r == 0; # EOF
		if ($fh) { # stream body from git-http-backend to HTTP client
			$fh->write($buf);
			$buf = '';
		} elsif ($buf =~ s/\A(.*?)\r?\n\r?\n//s) { # parse headers
			my $h = $1;
			my $code = 200;
			my @h;
			foreach my $l (split(/\r?\n/, $h)) {
				my ($k, $v) = split(/:\s*/, $l, 2);
				if ($k =~ /\AStatus\z/i) {
					$code = int($v);
				} else {
					push @h, $k, $v;
				}
			}
			# write response header:
			$fh = $res->([ $code, \@h ]);
			$res = undef;
			$fh->write($buf);
			$buf = '';
		} # else { keep reading ... }
	};
	if (my $async = $env->{'pi-httpd.async'}) {
		$rpipe = $async->($rpipe, $cb);
		sub { ($res) = @_ } # let Danga::Socket handle the rest.
	} else { # synchronous loop
		$vin = '';
		vec($vin, fileno($rpipe), 1) = 1;
		sub {
			($res) = @_;
			while ($rpipe) { $cb->() }
		}
	}
}

1;
