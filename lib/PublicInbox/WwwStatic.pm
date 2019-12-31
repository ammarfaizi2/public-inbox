# Copyright (C) 2016-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::WwwStatic;
use strict;
use Fcntl qw(:seek);

sub prepare_range {
	my ($env, $in, $h, $beg, $end, $size) = @_;
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
			sysseek($in, $beg, SEEK_SET) or return [ 500, [], [] ];
			push @$h, qw(Accept-Ranges bytes Content-Range);
			push @$h, "bytes $beg-$end/$size";

			# FIXME: Plack::Middleware::Deflater bug?
			$env->{'psgix.no-compress'} = 1;
		}
	}
	($code, $len);
}

sub response {
	my ($env, $h, $path, $type) = @_;
	return unless -f $path && -r _; # just in case it's a FIFO :P

	# TODO: If-Modified-Since and Last-Modified?
	open my $in, '<', $path or return;
	my $size = -s $in;
	my $len = $size;
	my $code = 200;
	push @$h, 'Content-Type', $type;
	if (($env->{HTTP_RANGE} || '') =~ /\bbytes=([0-9]*)-([0-9]*)\z/) {
		($code, $len) = prepare_range($env, $in, $h, $1, $2, $size);
		if ($code == 416) {
			push @$h, 'Content-Range', "bytes */$size";
			return [ 416, $h, [] ];
		}
	}
	push @$h, 'Content-Length', $len;
	my $body = bless {
		initial_rd => 65536,
		len => $len,
		in => $in,
		path => $path,
		env => $env,
	}, __PACKAGE__;
	[ $code, $h, $body ];
}

# called by PSGI servers:
sub getline {
	my ($self) = @_;
	my $len = $self->{len} or return; # undef, tells server we're done
	my $n = delete($self->{initial_rd}) // 8192;
	$n = $len if $len < $n;
	my $r = sysread($self->{in}, my $buf, $n);
	if (defined $r && $r > 0) { # success!
		$self->{len} = $len - $r;
		return $buf;
	}
	my $m = defined $r ? "EOF with $len bytes left" : "read error: $!";
	my $env = $self->{env};
	$env->{'psgi.errors'}->print("$self->{path} $m\n");

	# drop the client on error
	if (my $io = $env->{'psgix.io'}) {
		$io->close; # this is likely PublicInbox::DS::close
	} else { # for some PSGI servers w/o psgix.io
		die "dropping client socket\n";
	}
	undef;
}

sub close {} # noop, just let everything go out-of-scope

1;
