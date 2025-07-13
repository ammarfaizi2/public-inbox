# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# corner case tests for the generic PSGI server
# Usage: plackup [OPTIONS] /path/to/this/file
# used by httpd-{corner,https,unix}.t and psgi_rproxy.t
use v5.12;
use Plack::Builder;
require PublicInbox::SHA;
use autodie qw(close open);
if (defined(my $f = $ENV{TEST_OPEN_FIFO})) {
	open my $fh, '>', $f;
	say $fh 'hi';
	close $fh;
}

END {
	if (defined(my $f = $ENV{TEST_EXIT_FIFO})) {
		open my $fh, '>', $f;
		say $fh "bye from $$";
		close $fh;
	}
}

my $pi_config = $ENV{PI_CONFIG} // 'unset'; # capture ASAP
my $app = sub {
	my ($env) = @_;
	use Data::Dumper; warn Dumper($env);
	my $path = $env->{PATH_INFO};
	my $in = $env->{'psgi.input'};
	my $actual = -s $in;
	my $code = 500;
	my $h = [ 'Content-Type' => 'text/plain' ];
	my $body = [];
	if ($path eq '/sha1') {
		my $sha1 = PublicInbox::SHA->new(1);
		my $buf;
		while (1) {
			my $r = $in->read($buf, 4096);
			die "read err: $!" unless defined $r;
			last if $r == 0;
			$sha1->add($buf);
		}
		$code = 200;
		push @$body, $sha1->hexdigest;
	} elsif ($path eq '/env') {
		$code = 200;
		require Data::Dumper;
		my %env;
		while (my ($k, $v) = each %$env) {
			$env{$k} = $v if !ref $v;
		}
		my $in = $env->{'psgi.input'};
		my $buf = '';
		while (1) {
			my $r = $in->read($buf, 0x4000, length($buf)) //
				die "psgi read: $!";
			last if $r == 0;
		}
		$env{'test.input_data'} = $buf;
		push @$body, Data::Dumper->new([\%env])->
			Useqq(1)->Terse(1)->Sortkeys(1)->Dump;
		push @$body, "\n.\n"; # for readline w/ $/;
	} elsif (my $fifo = $env->{HTTP_X_CHECK_FIFO}) {
		if ($path eq '/slow-header') {
			return sub {
				open my $f, '<', $fifo;
				local $/ = "\n";
				my @r = <$f>;
				$_[0]->([200, $h, \@r ]);
			};
		} elsif ($path eq '/slow-body') {
			return sub {
				my $fh = $_[0]->([200, $h]);
				open my $f, '<', $fifo;
				local $/ = "\n";
				while (defined(my $l = <$f>)) {
					$fh->write($l);
				}
				$fh->close;
			};
		}
	} elsif ($path eq '/host-port') {
		$code = 200;
		push @$body, "$env->{REMOTE_ADDR} $env->{REMOTE_PORT}";
	} elsif ($path eq '/session_reused') {
		my $sock = $env->{'psgix.io'}; # IO::Socket::SSL
		$body = [ $sock->get_session_reused ? "y\n" : "n\n" ];
		$code = 200;
	} elsif ($path eq '/callback') {
		return sub {
			my ($res) = @_;
			my $buf = "hello world\n";
			push @$h, 'Content-Length', length($buf);
			my $fh = $res->([200, $h]);
			$fh->write($buf);
			$fh->close;
		}
	} elsif ($path eq '/callback-truncated') {
		return sub {
			my ($res) = @_;
			push @$h, 'Transfer-Encoding', 'chunked';
			my $fh = $res->([200, $h]);
			$fh->write("9\r\ntoo ort\r\n");
			$fh->close;
		}
	} elsif ($path eq '/empty') {
		$code = 200;
	} elsif ($path eq '/getline-die') {
		$code = 200;
		$body = Plack::Util::inline_object(
			getline => sub { die 'GETLINE FAIL' },
			close => sub { die 'CLOSE FAIL' },
		);
	} elsif ($path eq '/close-die') {
		$code = 200;
		$body = Plack::Util::inline_object(
			getline => sub { undef },
			close => sub { die 'CLOSE FAIL' },
		);
	} elsif ($path eq '/async-big') {
		require PublicInbox::Qspawn;
		open my $null, '>', '/dev/null';
		my $rdr = { 2 => fileno($null) };
		my $cmd = [qw(dd if=/dev/zero count=30 bs=1024k)];
		my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
		return $qsp->psgi_yield($env, undef, sub {
			my ($r, $bref) = @_;
			# make $rd_hdr retry sysread + $parse_hdr in Qspawn:
			return until length($$bref) > 8000;
			close $null;
			[ 200, [ qw(Content-Type application/octet-stream) ]];
		});
	} elsif ($path eq '/psgi-yield-gzip') {
		require PublicInbox::Qspawn;
		require PublicInbox::GzipFilter;
		my $cmd = [qw(echo hello world)];
		my $qsp = PublicInbox::Qspawn->new($cmd);
		$env->{'qspawn.filter'} = PublicInbox::GzipFilter->new;
		return $qsp->psgi_yield($env, undef, sub {
			[ 200, [ qw(Content-Type application/octet-stream)]]
		});
	} elsif ($path eq '/psgi-yield-compressible') {
		require PublicInbox::Qspawn;
		my $cmd = [qw(echo goodbye world)];
		my $qsp = PublicInbox::Qspawn->new($cmd);
		return $qsp->psgi_yield($env, undef, sub {
			[200, [qw(Content-Type text/plain)]]
		});
	} elsif ($path eq '/psgi-yield-enoent') {
		require PublicInbox::Qspawn;
		my $cmd = [ 'this-better-not-exist-in-PATH'.rand ];
		my $qsp = PublicInbox::Qspawn->new($cmd);
		return $qsp->psgi_yield($env, undef, sub {
			[ 200, [ qw(Content-Type application/octet-stream)]]
		});
	} elsif ($path eq '/pid') {
		$code = 200;
		push @$body, "$$\n";
	} elsif ($path eq '/url_scheme') {
		$code = 200;
		push @$body, $env->{'psgi.url_scheme'}
	} elsif ($path eq '/server') {
		$code = 200;
		push @$h, 'Server', 'none-of-your-business';
		push @$body, 'trying to advertise server behind rproxy';
	} elsif ($path eq '/PI_CONFIG') {
		$code = 200;
		push @$body, $pi_config; # show value at ->refresh_groups
	} elsif ($path =~ m!\A/exit-fifo(.+)\z!) {
		$code = 200;
		$ENV{TEST_EXIT_FIFO} = $1; # for END {}
		push @$body, "fifo $1 registered";
	}
	[ $code, $h, $body ]
};

builder {
	enable 'Head';
	$app;
}
