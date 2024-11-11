#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use Test::More;
use PublicInbox::Spawn qw(which spawn popen_rd run_qx);
require PublicInbox::Sigfd;
require PublicInbox::DS;
use PublicInbox::OnDestroy;
my $rlimit_map = PublicInbox::Spawn->can('rlimit_map');
{
	my $true = which('true');
	ok($true, "'true' command found with which()");
}

{
	my $pid = spawn(['true']);
	ok($pid, 'spawned process');
	is(waitpid($pid, 0), $pid, 'waitpid succeeds on spawned process');
	is($?, 0, 'true exited successfully');
}

{
	my $opt = { 0 => \'in', 2 => \(my $e) };
	my $out = run_qx(['sh', '-c', 'echo e >&2; cat'], undef, $opt);
	is($e, "e\n", 'captured stderr');
	is($out, 'in', 'stdin read and stdout captured');
	$opt->{0} = \"IN\n3\nLINES";
	my @out = run_qx(['sh', '-c', 'echo E >&2; cat'], undef, $opt);
	is($e, "E\n", 'captured stderr clobbers string');
	is_deeply(\@out, [ "IN\n", "3\n", 'LINES' ], 'stdout array');
}

SKIP: {
	my $pid = spawn(['true'], undef, { pgid => 0 });
	ok($pid, 'spawned process with new pgid');
	is(waitpid($pid, 0), $pid, 'waitpid succeeds on spawned process');
	is($?, 0, 'true exited successfully');
	pipe(my ($r, $w)) or BAIL_OUT;

	# Find invalid PID to try to join its process group.
	my $wrong_pgid = 1;
	for (my $i=0x7fffffff; $i >= 2; $i--) {
		if (kill(0, $i) == 0) {
			$wrong_pgid = $i;
			last;
		}
	}

	# Test spawn behavior when it can't join the requested process group.
	$pid = eval { spawn(['true'], undef, { pgid => $wrong_pgid, 2 => $w }) };
	close $w;
	my $err = do { local $/; <$r> };
	if (defined $pid) {
		waitpid($pid, 0);
		isnt($?, 0, 'child error (pure-Perl)');
	} else {
		ok($@, 'exception raised');
	}
}

{ # ensure waitpid(-1, 0) and SIGCHLD works in spawned process
	my $script = <<'EOF';
$| = 1; # unbuffer stdout
defined(my $pid = fork) or die "fork: $!";
if ($pid == 0) { exit }
elsif ($pid > 0) {
	my $waited = waitpid(-1, 0);
	$waited == $pid or die "mismatched child $pid != $waited";
	$? == 0 or die "child err: $>";
	$SIG{CHLD} = sub { print "HI\n"; exit };
	print "RDY $$\n";
	select(undef, undef, undef, 0.01) while 1;
}
EOF
	my $oldset = PublicInbox::DS::block_signals();
	my $rd = popen_rd([$^X, qw(-w -e), $script]);
	diag 'waiting for child to reap grandchild...';
	chomp(my $line = readline($rd));
	my ($rdy, $pid) = split(/ /, $line);
	is($rdy, 'RDY', 'got ready signal, waitpid(-1) works in child');
	ok(kill('CHLD', $pid), 'sent SIGCHLD to child');
	is(readline($rd), "HI\n", '$SIG{CHLD} works in child');
	ok($rd->close, 'popen_rd close works');
	PublicInbox::DS::sig_setmask($oldset);
}

{
	my ($r, $w);
	pipe $r, $w or die "pipe failed: $!";
	my $pid = spawn(['echo', 'hello world'], undef, { 1 => fileno($w) });
	close $w or die "close pipe[1] failed: $!";
	is(<$r>, "hello world\n", 'read stdout of spawned from pipe');
	is(waitpid($pid, 0), $pid, 'waitpid succeeds on spawned process');
	is($?, 0, 'true exited successfully');
}

{
	my ($r, $w);
	pipe $r, $w or die "pipe failed: $!";
	my $pid = spawn(['sh', '-c', 'echo $HELLO'],
		{ 'HELLO' => 'world' }, { 1 => $w });
	close $w or die "close pipe[1] failed: $!";
	is(<$r>, "world\n", 'read stdout of spawned from pipe');
	is(waitpid($pid, 0), $pid, 'waitpid succeeds on spawned process');
	is($?, 0, 'sh exited successfully');
}

{
	my $fh = popen_rd([qw(echo hello)]);
	ok(fileno($fh) >= 0, 'fileno works');
	my $l = <$fh>;
	is($l, "hello\n", 'readline works');
	$l = <$fh>;
	ok(!$l, 'readline works for EOF');
}

{
	my $fh = popen_rd([qw(printf foo\nbar)]);
	ok(fileno($fh) >= 0, 'fileno works');
	is($fh->blocking(0), 1, '->blocking was true');
	is($fh->blocking, 0, '->blocking is false');
	is($fh->blocking(1), 0, '->blocking was true');
	is($fh->blocking, 1, '->blocking is true');
	my @line = <$fh>;
	is_deeply(\@line, [ "foo\n", 'bar' ], 'wantarray works on readline');
}

{
	my $fh = popen_rd([qw(echo hello)]);
	like($fh->attached_pid, qr/\A[0-9]+\z/, 'have a PID');
	my $buf;
	is(sysread($fh, $buf, 6), 6, 'sysread got 6 bytes');
	is($buf, "hello\n", 'sysread works');
	is(sysread($fh, $buf, 6), 0, 'sysread got EOF');
	$? = 1;
	ok($fh->close, 'close succeeds');
	is($?, 0, '$? set properly');
	is($fh->attached_pid, undef, 'attached_pid cleared after close');
}

{
	my $fh = popen_rd([qw(false)]);
	ok(!$fh->close, 'close fails on false');
	isnt($?, 0, '$? set properly: '.$?);
}

{
	local $ENV{GIT_CONFIG} = '/path/to/this/better/not/exist';
	my $fh = popen_rd([qw(env)], { GIT_CONFIG => undef });
	ok(!grep(/^GIT_CONFIG=/, <$fh>), 'GIT_CONFIG clobbered');
}

{ # ->CLOSE vs ->DESTROY waitpid caller distinction
	my @c;
	my $fh = popen_rd(['true'], undef, undef, sub { @c = caller });
	ok($fh->close, '->CLOSE fired and successful');
	ok(scalar(@c), 'callback fired by ->CLOSE');
	ok(grep(!m[/PublicInbox/DS\.pm\z], @c), 'callback not invoked by DS');

	@c = ();
	$fh = popen_rd(['true'], undef, undef, sub { @c = caller });
	undef $fh; # ->DESTROY
	ok(scalar(@c), 'callback fired by ->DESTROY');
	ok(grep(!m[/PublicInbox/IO\.pm\z], @c),
		'callback not invoked by PublicInbox::IO');
}

{ # children don't wait on siblings
	use POSIX qw(_exit);
	pipe(my ($r, $w)) or BAIL_OUT $!;
	my @arg;
	my $fh = popen_rd(['cat'], undef, { 0 => $r },
			sub { @arg = @_; warn "x=$$\n" }, 'hi');
	my $pid = PublicInbox::OnDestroy::fork_tmp;
	local $SIG{__WARN__} = sub { _exit(1) };
	if ($pid == 0) {
		local $SIG{__DIE__} = sub { _exit(2) };
		undef $fh;
		_exit(0);
	}
	waitpid($pid, 0);
	is($?, 0, 'forked process exited');
	my @w;
	local $SIG{__WARN__} = sub { push @w, @_ };
	close $w;
	$fh->close; # may set $?
	is($?, 0, 'cat exited');
	is(scalar(@arg), 2, 'callback got args');
	is($arg[1], 'hi', 'passed arg');
	like($arg[0], qr/\A\d+\z/, 'PID');
	is_deeply(\@w, [ "x=$$\n" ], 'callback fired from owner');
}

SKIP: {
	if ($rlimit_map) { # Inline::C installed
		my %rlim = $rlimit_map->();
		ok defined($rlim{RLIMIT_CPU}), 'RLIMIT_CPU defined';
	} else {
		eval {
			require BSD::Resource;
			defined(BSD::Resource::RLIMIT_CPU())
		} or skip 'BSD::Resource::RLIMIT_CPU missing', 3;
	}
	my $cmd = [ $^X, qw(-w -e), <<'EOM' ];
use POSIX qw(:signal_h);
use Time::HiRes qw(time); # gettimeofday
my $have_bsd_resource = eval { require BSD::Resource };
my $set = POSIX::SigSet->new;
$set->emptyset; # spawn() defaults to blocking all signals
sigprocmask(SIG_SETMASK, $set) or die "SIG_SETMASK: $!";
my $tot = 0;
$SIG{XCPU} = sub { print "SIGXCPU $tot\n"; exit(1) };
my $next = time + 1.1;
while (1) {
	# OpenBSD needs some syscalls (e.g. `times', `gettimeofday'
	# and `write' (via Perl warn)) on otherwise idle systems to
	# hit RLIMIT_CPU and fire signals:
	# https://marc.info/?i=02A4BB8D-313C-464D-845A-845EB6136B35@gmail.com
	my @t = $have_bsd_resource ? BSD::Resource::times() : (0, 0);
	$tot = $t[0] + $t[1];
	if (time > $next) {
		warn "# T: @t (utime, ctime, cutime, cstime)\n" if @t;
		$next = time + 1.1;
	}
}
EOM
	pipe(my($r, $w)) or die "pipe: $!";
	my $fd = fileno($w);
	my $opt = { RLIMIT_CPU => [ 1, 9 ], RLIMIT_CORE => [ 0, 0 ], 1 => $fd };
	vec(my $rset = '', fileno($r), 1) = 1;
	my $pid = spawn($cmd, undef, $opt);
	close $w or die "close(w): $!";
	ok(select($rset, undef, undef, 8), 'child died before timeout');
	is(waitpid($pid, 0), $pid, 'XCPU child process reaped');
	my $line;
	like($line = readline($r), qr/SIGXCPU/, 'SIGXCPU handled') or
		diag explain($line);
	is($? >> 8, 1, 'non-zero exit status');
}

SKIP: {
	require PublicInbox::SpawnPP;
	require File::Temp;
	my $tmp = File::Temp->newdir('spawnpp-XXXX', TMPDIR => 1);
	my $cmd = [ qw(/bin/sh -c), 'echo $HI >foo' ];
	my $env = [ 'HI=hihi' ];
	my $rlim = [];
	my $pgid = -1;
	my $pid = PublicInbox::SpawnPP::pi_fork_exec([], '/bin/sh', $cmd, $env,
						$rlim, "$tmp", $pgid);
	is(waitpid($pid, 0), $pid, 'spawned process exited');
	is($?, 0, 'no error');
	open my $fh, '<', "$tmp/foo" or die "open: $!";
	is(readline($fh), "hihi\n", 'env+chdir worked for SpawnPP');
	close $fh;
	unlink("$tmp/foo") or die "unlink: $!";
	{
		local $ENV{MOD_PERL} = 1;
		$pid = PublicInbox::SpawnPP::pi_fork_exec([],
				'/bin/sh', $cmd, $env, $rlim, "$tmp", $pgid);
	}
	is(waitpid($pid, 0), $pid, 'spawned process exited');
	open $fh, '<', "$tmp/foo" or die "open: $!";
	is(readline($fh), "hihi\n", 'env+chdir SpawnPP under (faked) MOD_PERL');
}

done_testing();
