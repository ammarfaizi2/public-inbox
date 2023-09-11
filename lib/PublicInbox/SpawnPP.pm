# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Pure-Perl implementation of "spawn".  This can't take advantage
# of vfork, so no speedups under Linux for spawning from large processes.
# Do not require this directly, only use from PublicInbox::Spawn
package PublicInbox::SpawnPP;
use v5.12;
use POSIX qw(dup2 _exit setpgid :signal_h);
# this is loaded by PublicInbox::Spawn, so we can't use/require it, here

# Pure Perl implementation for folks that do not use Inline::C
sub pi_fork_exec ($$$$$$$) {
	my ($redir, $f, $cmd, $env, $rlim, $cd, $pgid) = @_;
	my $old = POSIX::SigSet->new();
	my $set = POSIX::SigSet->new();
	$set->fillset or die "sigfillset: $!";
	for (POSIX::SIGABRT, POSIX::SIGBUS, POSIX::SIGFPE,
			POSIX::SIGILL, POSIX::SIGSEGV) {
		$set->delset($_) or die "delset($_): $!";
	}
	sigprocmask(SIG_SETMASK, $set, $old) or die "SIG_SETMASK(set): $!";
	my $syserr;
	pipe(my ($r, $w)) or die "pipe: $!";
	my $pid = fork // die "fork (+exec) @$cmd: $!\n";
	if ($pid == 0) {
		close $r;
		$SIG{__DIE__} = sub {
			warn(@_);
			syswrite($w, my $num = $! + 0);
			_exit(1);
		};
		for my $child_fd (0..$#$redir) {
			my $parent_fd = $redir->[$child_fd];
			next if $parent_fd == $child_fd;
			dup2($parent_fd, $child_fd) or
				die "dup2($parent_fd, $child_fd): $!";
		}
		if ($pgid >= 0 && !defined(setpgid(0, $pgid))) {
			die "setpgid(0, $pgid): $!";
		}
		$SIG{$_} = 'DEFAULT' for grep(!/\A__/, keys %SIG);
		if ($cd ne '') {
			chdir $cd or die "cd $cd: $!";
		}
		while (@$rlim) {
			my ($r, $soft, $hard) = splice(@$rlim, 0, 3);
			BSD::Resource::setrlimit($r, $soft, $hard) or
				die "setrlimit($r=[$soft,$hard]: $!)";
		}
		$old->delset(POSIX::SIGCHLD) or die "sigdelset CHLD: $!";
		sigprocmask(SIG_SETMASK, $old) or die "SIG_SETMASK ~CHLD: $!";
		$cmd->[0] = $f;
		if ($ENV{MOD_PERL}) {
			$f = PublicInbox::Spawn::which('env');
			@$cmd = ('env', '-i', @$env, @$cmd);
		} else {
			%ENV = map { split(/=/, $_, 2) } @$env;
		}
		exec { $f } @$cmd;
		die "exec @$cmd failed: $!";
	}
	close $w;
	sigprocmask(SIG_SETMASK, $old) or die "SIG_SETMASK(old): $!";
	if (my $cerrnum = do { local $/, <$r> }) {
		$! = $cerrnum;
		die "forked child $@: $!";
	}
	$pid;
}

1;
