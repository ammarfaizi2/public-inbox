# Copyright (C) 2016-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Tests for binding Unix domain sockets
use strict;
use warnings;
use Test::More;
use PublicInbox::TestCommon;
use Errno qw(EADDRINUSE);
use Cwd qw(abs_path);
use Carp qw(croak);
require_mods(qw(Plack::Util Plack::Builder HTTP::Date HTTP::Status));
use IO::Socket::UNIX;
my ($tmpdir, $for_destroy) = tmpdir();
my $unix = "$tmpdir/unix.sock";
my $psgi = './t/httpd-corner.psgi';
my $out = "$tmpdir/out.log";
my $err = "$tmpdir/err.log";
my $td;

my $spawn_httpd = sub {
	my (@args) = @_;
	my $cmd = [ '-httpd', @args, "--stdout=$out", "--stderr=$err", $psgi ];
	$td = start_script($cmd);
};

{
	require PublicInbox::Daemon;
	my $l = "$tmpdir/named.sock";
	my $s = IO::Socket::UNIX->new(Listen => 5, Local => $l,
					Type => SOCK_STREAM);
	is(PublicInbox::Daemon::sockname($s), $l, 'sockname works for UNIX');
}

ok(!-S $unix, 'UNIX socket does not exist, yet');
$spawn_httpd->("-l$unix", '-W0');
my %o = (Peer => $unix, Type => SOCK_STREAM);
for (1..1000) {
	last if -S $unix && IO::Socket::UNIX->new(%o);
	select undef, undef, undef, 0.02
}

ok(-S $unix, 'UNIX socket was bound by -httpd');
sub check_sock ($) {
	my ($unix) = @_;
	my $sock = IO::Socket::UNIX->new(Peer => $unix, Type => SOCK_STREAM);
	warn "E: $! connecting to $unix\n" unless defined $sock;
	ok($sock, 'client UNIX socket connected');
	ok($sock->write("GET /host-port HTTP/1.0\r\n\r\n"),
		'wrote req to server');
	ok($sock->read(my $buf, 4096), 'read response');
	like($buf, qr!\r\n\r\n127\.0\.0\.1:0\z!,
		'set REMOTE_ADDR and REMOTE_PORT for Unix socket');
}

check_sock($unix);

{ # do not clobber existing socket
	my %err = ( 'linux' => EADDRINUSE, 'freebsd' => EADDRINUSE );
	open my $out, '>>', "$tmpdir/1" or die "redirect failed: $!";
	open my $err, '>>', "$tmpdir/2" or die "redirect failed: $!";
	my $cmd = ['-httpd', '-l', $unix, '-W0', $psgi];
	my $ftd = start_script($cmd, undef, { 1 => $out, 2 => $err });
	$ftd->join;
	isnt($?, 0, 'httpd failure set $?');
	SKIP: {
		my $ec = $err{$^O} or
			skip("not sure if $^O fails with EADDRINUSE", 1);
		is($? >> 8, $ec, 'httpd failed with EADDRINUSE');
	};
	open my $fh, "$tmpdir/2" or die "failed to open $tmpdir/2: $!";
	local $/;
	my $e = <$fh>;
	like($e, qr/no listeners bound/i, 'got error message');
	is(-s "$tmpdir/1", 0, 'stdout was empty');
}

{
	is($td->kill, 1, 'terminate existing process');
	$td->join;
	is($?, 0, 'existing httpd exited successfully');
	ok(-S $unix, 'unix socket still exists');
}

sub delay_until {
	my $cond = shift;
	for (1..1000) {
		return if $cond->();
		select undef, undef, undef, 0.012;
	}
	Carp::croak('condition failed');
}

SKIP: {
	require_mods('Net::Server::Daemonize', 20);
	my $pid_file = "$tmpdir/pid";
	my $read_pid = sub {
		my $f = shift;
		open my $fh, '<', $f or die "open $f failed: $!";
		my $pid = do { local $/; <$fh> };
		chomp $pid;
		$pid || 0;
	};

	for my $w (qw(-W0 -W1)) {
		# wait for daemonization
		$spawn_httpd->("-l$unix", '-D', '-P', $pid_file, $w);
		$td->join;
		is($?, 0, "daemonized $w process");
		check_sock($unix);

		ok(-f $pid_file, "$w pid file written");
		my $pid = $read_pid->($pid_file);
		is(kill('TERM', $pid), 1, "signaled daemonized $w process");
		delay_until(sub { !kill(0, $pid) });
		is(kill(0, $pid), 0, "daemonized $w process exited");
		ok(!-e $pid_file, "$w pid file unlinked at exit");
	}

	# try a USR2 upgrade with workers:
	my $httpd = abs_path('blib/script/public-inbox-httpd');
	$psgi = abs_path($psgi);
	my $opt = { run_mode => 0 };

	my @args = ("-l$unix", '-D', '-P', $pid_file, -1, $out, -2, $err);
	$td = start_script([$httpd, @args, $psgi], undef, $opt);
	$td->join;
	is($?, 0, "daemonized process again");
	check_sock($unix);
	my $pid = $read_pid->($pid_file);

	# stop worker to ensure check_sock below hits $new_pid
	kill('TTOU', $pid) or die "TTOU failed: $!";

	kill('USR2', $pid) or die "USR2 failed: $!";
	delay_until(sub {
		$pid != (eval { $read_pid->($pid_file) } // $pid)
	});
	my $new_pid = $read_pid->($pid_file);
	isnt($new_pid, $pid, 'new child started');
	my $old_pid = $read_pid->("$pid_file.oldbin");
	is($old_pid, $pid, '.oldbin pid file written');

	check_sock($unix); # ensures $new_pid is ready to receive signals

	# first, back out of the upgrade
	kill('QUIT', $new_pid) or die "kill new PID failed: $!";
	delay_until(sub {
		$pid == (eval { $read_pid->($pid_file) } // 0)
	});
	is($read_pid->($pid_file), $pid, 'old PID file restored');
	ok(!-f "$pid_file.oldbin", '.oldbin PID file gone');

	# retry USR2 upgrade
	kill('USR2', $pid) or die "USR2 failed: $!";
	delay_until(sub {
		$pid != (eval { $read_pid->($pid_file) } // $pid)
	});
	$new_pid = $read_pid->($pid_file);
	isnt($new_pid, $pid, 'new child started again');
	$old_pid = $read_pid->("$pid_file.oldbin");
	is($old_pid, $pid, '.oldbin pid file written');

	# drop the old parent
	kill('QUIT', $old_pid) or die "QUIT failed: $!";
	delay_until(sub { !kill(0, $old_pid) });

	# drop the new child
	check_sock($unix);
	kill('QUIT', $new_pid) or die "QUIT failed: $!";
	delay_until(sub { !kill(0, $new_pid) });
	ok(!-f $pid_file, 'PID file is gone');


	# try USR2 without workers (-W0)
	$td = start_script([$httpd, @args, '-W0', $psgi], undef, $opt);
	$td->join;
	is($?, 0, 'daemonized w/o workers');
	check_sock($unix);
	$pid = $read_pid->($pid_file);

	# replace running process
	kill('USR2', $pid) or die "USR2 failed: $!";
	delay_until(sub { !kill(0, $pid) });

	check_sock($unix);
	$pid = $read_pid->($pid_file);
	kill('QUIT', $pid) or die "USR2 failed: $!";
	delay_until(sub { !kill(0, $pid) });
	ok(!-f $pid_file, 'PID file is gone');
}

done_testing();
