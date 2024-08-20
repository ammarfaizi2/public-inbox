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
use autodie qw(kill);

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
		$sig->{$s} = sub { die "SHOULD NOT BE CALLED ($s)" }
	}
	kill 'USR2', $$ or die "kill $!";
	ok(!defined($hit->{USR2}), 'no USR2 yet') or diag explain($hit);
	PublicInbox::DS->Reset;
	ok($PublicInbox::Syscall::SIGNUM{WINCH}, 'SIGWINCH number defined');
	my $sigfd = PublicInbox::Sigfd->new($sig);
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
			is($hit->{$s}->{normal}, 1, "sigfd fired $s");
		}
		SKIP: {
			skip 'Linux sigfd-only behavior', 1 if !$linux_sigfd;
			is($hit->{USR2}->{normal}, 1,
				'USR2 sent before signalfd created received');
		}
		PublicInbox::DS->Reset;
		$sigfd = undef;

		my $nbsig = PublicInbox::Sigfd->new($sig);
		ok($nbsig, 'Sigfd->new SFD_NONBLOCK works');
		is($nbsig->wait_once, undef, 'nonblocking ->wait_once');
		ok($! == Errno::EAGAIN, 'got EAGAIN');
		kill('HUP', $$) or die "kill $!";
		local @PublicInbox::DS::post_loop_do = (sub {}); # loop once
		PublicInbox::DS::event_loop();
		is($hit->{HUP}->{normal}, 2, 'HUP sigfd fired in event loop') or
			diag explain($hit); # sometimes fails on FreeBSD 11.x
		kill('TERM', $$) or die "kill $!";
		kill('HUP', $$) or die "kill $!";
		PublicInbox::DS::event_loop();
		PublicInbox::DS->Reset;
		is($hit->{TERM}->{normal}, 1, 'TERM sigfd fired in event loop');
		is($hit->{HUP}->{normal}, 3, 'HUP sigfd fired in event loop');
		ok($hit->{WINCH}->{normal}, 'WINCH sigfd fired in event loop');

		my $restore = PublicInbox::DS::allow_sigs 'HUP';
		kill 'HUP', $$;
		select undef, undef, undef, 0;
		is $hit->{HUP}->{normal}, 4, 'HUP sigfd fired after allow_sigs';

		undef $restore;
		kill 'HUP', $$;
		vec($rvec = '', fileno($nbsig->{sock}), 1) = 1;
		ok select($rvec, undef, undef, 1),
			'select reports sigfd readiness';
		is $hit->{HUP}->{normal}, 4, 'HUP not fired when sigs blocked';
		$nbsig->event_step;
		is $hit->{HUP}->{normal}, 5, 'HUP fires only on ->event_step';

		kill 'HUP', $$;
		is $hit->{HUP}->{normal}, 5, 'HUP not fired, yet';
		$restore = PublicInbox::DS::allow_sigs 'HUP';
		select(undef, undef, undef, 0);
		is $hit->{HUP}->{normal}, 6, 'HUP fires from allow_sigs';
	} else {
		skip('signalfd disabled?', 10);
	}
	PublicInbox::DS::sig_setmask($old);
}

done_testing;
