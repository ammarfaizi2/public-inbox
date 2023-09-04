#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
use v5.12;
use Test::More;
use IO::Handle;
use POSIX qw(:signal_h);
use Errno qw(ENOSYS);
require_ok 'PublicInbox::Sigfd';
use PublicInbox::DS;
my ($linux_sigfd, $has_sigfd);

SKIP: {
	if ($^O ne 'linux' && !eval { require IO::KQueue }) {
		skip 'signalfd requires Linux or IO::KQueue to emulate', 10;
	}

	my $old = PublicInbox::DS::block_signals();
	my $hit = {};
	my $sig = {};
	local $SIG{USR2} = sub { $hit->{USR2}->{normal}++ };
	local $SIG{HUP} = sub { $hit->{HUP}->{normal}++ };
	local $SIG{TERM} = sub { $hit->{TERM}->{normal}++ };
	local $SIG{INT} = sub { $hit->{INT}->{normal}++ };
	local $SIG{WINCH} = sub { $hit->{WINCH}->{normal}++ };
	for my $s (qw(USR2 HUP TERM INT WINCH)) {
		$sig->{$s} = sub { $hit->{$s}->{sigfd}++ };
	}
	kill 'USR2', $$ or die "kill $!";
	ok(!defined($hit->{USR2}), 'no USR2 yet') or diag explain($hit);
	PublicInbox::DS->Reset;
	my $sigfd = PublicInbox::Sigfd->new($sig, 0);
	if ($sigfd) {
		$linux_sigfd = 1 if $^O eq 'linux';
		$has_sigfd = 1;
		ok($sigfd, 'Sigfd->new works');
		kill('HUP', $$) or die "kill $!";
		kill('INT', $$) or die "kill $!";
		kill('WINCH', $$) or die "kill $!";
		my $fd = fileno($sigfd->{sock});
		ok($fd >= 0, 'fileno(Sigfd->{sock}) works');
		my $rvec = '';
		vec($rvec, $fd, 1) = 1;
		is(select($rvec, undef, undef, undef), 1, 'select() works');
		ok($sigfd->wait_once, 'wait_once reported success');
		for my $s (qw(HUP INT)) {
			is($hit->{$s}->{sigfd}, 1, "sigfd fired $s");
			is($hit->{$s}->{normal}, undef,
				"normal \$SIG{$s} not fired");
		}
		SKIP: {
			skip 'Linux sigfd-only behavior', 1 if !$linux_sigfd;
			is($hit->{USR2}->{sigfd}, 1,
				'USR2 sent before signalfd created received');
		}
		ok(!$hit->{USR2}->{normal}, 'USR2 not fired normally');
		PublicInbox::DS->Reset;
		$sigfd = undef;

		my $nbsig = PublicInbox::Sigfd->new($sig, 1);
		ok($nbsig, 'Sigfd->new SFD_NONBLOCK works');
		is($nbsig->wait_once, undef, 'nonblocking ->wait_once');
		ok($! == Errno::EAGAIN, 'got EAGAIN');
		kill('HUP', $$) or die "kill $!";
		local @PublicInbox::DS::post_loop_do = (sub {}); # loop once
		PublicInbox::DS::event_loop();
		is($hit->{HUP}->{sigfd}, 2, 'HUP sigfd fired in event loop') or
			diag explain($hit); # sometimes fails on FreeBSD 11.x
		kill('TERM', $$) or die "kill $!";
		kill('HUP', $$) or die "kill $!";
		PublicInbox::DS::event_loop();
		PublicInbox::DS->Reset;
		is($hit->{TERM}->{sigfd}, 1, 'TERM sigfd fired in event loop');
		is($hit->{HUP}->{sigfd}, 3, 'HUP sigfd fired in event loop');
		is($hit->{WINCH}->{sigfd}, 1, 'WINCH sigfd fired in event loop');
	} else {
		skip('signalfd disabled?', 10);
	}
	ok(!$hit->{USR2}->{normal}, 'USR2 still not fired normally');
	PublicInbox::DS::sig_setmask($old);
	SKIP: {
		($has_sigfd && !$linux_sigfd) or
			skip 'EVFILT_SIGNAL-only behavior check', 1;
		is($hit->{USR2}->{normal}, 1,
			"USR2 fired normally after unblocking on $^O");
	}
}

done_testing;
