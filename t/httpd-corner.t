# Copyright (C) 2016-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# note: our HTTP server should be standalone and capable of running
# generic PSGI/Plack apps.
use strict;
use warnings;
use Test::More;
use Time::HiRes qw(gettimeofday tv_interval);
use PublicInbox::Spawn qw(which);

foreach my $mod (qw(Plack::Util Plack::Builder
			HTTP::Date HTTP::Status IPC::Run)) {
	eval "require $mod";
	plan skip_all => "$mod missing for httpd-corner.t" if $@;
}

use Digest::SHA qw(sha1_hex);
use File::Temp qw/tempdir/;
use IO::Socket;
use IO::Socket::UNIX;
use Fcntl qw(:seek);
use Socket qw(IPPROTO_TCP TCP_NODELAY SOL_SOCKET);
use POSIX qw(mkfifo);
require './t/common.perl';
my $tmpdir = tempdir('httpd-corner-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $fifo = "$tmpdir/fifo";
ok(defined mkfifo($fifo, 0777), 'created FIFO');
my $err = "$tmpdir/stderr.log";
my $out = "$tmpdir/stdout.log";
my $httpd = 'blib/script/public-inbox-httpd';
my $psgi = "./t/httpd-corner.psgi";
my $sock = tcp_server();

# make sure stdin is not a pipe for lsof test to check for leaking pipes
open(STDIN, '<', '/dev/null') or die 'no /dev/null: $!';

# Make sure we don't clobber socket options set by systemd or similar
# using socket activation:
my ($defer_accept_val, $accf_arg);
if ($^O eq 'linux') {
	setsockopt($sock, IPPROTO_TCP, Socket::TCP_DEFER_ACCEPT(), 5) or die;
	my $x = getsockopt($sock, IPPROTO_TCP, Socket::TCP_DEFER_ACCEPT());
	defined $x or die "getsockopt: $!";
	$defer_accept_val = unpack('i', $x);
	if ($defer_accept_val <= 0) {
		die "unexpected TCP_DEFER_ACCEPT value: $defer_accept_val";
	}
} elsif ($^O eq 'freebsd' && system('kldstat -m accf_data >/dev/null') == 0) {
	require PublicInbox::Daemon;
	my $var = PublicInbox::Daemon::SO_ACCEPTFILTER();
	$accf_arg = pack('a16a240', 'dataready', '');
	setsockopt($sock, SOL_SOCKET, $var, $accf_arg) or die "setsockopt: $!";
}

my $upath = "$tmpdir/s";
my $unix = unix_server($upath);
ok($unix, 'UNIX socket created');
my $pid;
END { kill 'TERM', $pid if defined $pid };
my $spawn_httpd = sub {
	my (@args) = @_;
	my $cmd = [ $httpd, @args, "--stdout=$out", "--stderr=$err", $psgi ];
	$pid = spawn_listener(undef, $cmd, [ $sock, $unix ]);
	ok(defined $pid, 'forked httpd process successfully');
};

{
	ok($sock, 'sock created');
	$spawn_httpd->('-W0');
}

{
	my $conn = conn_for($sock, 'streaming callback');
	$conn->write("GET /callback HTTP/1.0\r\n\r\n");
	ok($conn->read(my $buf, 8192), 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	is($body, "hello world\n", 'callback body matches expected');
}

{
	my $conn = conn_for($sock, 'getline-die');
	$conn->write("GET /getline-die HTTP/1.1\r\nHost: example.com\r\n\r\n");
	ok($conn->read(my $buf, 8192), 'read some response');
	like($buf, qr!HTTP/1\.1 200\b[^\r]*\r\n!, 'got some sort of header');
	is($conn->read(my $nil, 8192), 0, 'read EOF');
	$conn = undef;
	my $after = capture($err);
	is(scalar(grep(/GETLINE FAIL/, @$after)), 1, 'failure logged');
	is(scalar(grep(/CLOSE FAIL/, @$after)), 1, 'body->close not called');
}

{
	my $conn = conn_for($sock, 'close-die');
	$conn->write("GET /close-die HTTP/1.1\r\nHost: example.com\r\n\r\n");
	ok($conn->read(my $buf, 8192), 'read some response');
	like($buf, qr!HTTP/1\.1 200\b[^\r]*\r\n!, 'got some sort of header');
	is($conn->read(my $nil, 8192), 0, 'read EOF');
	$conn = undef;
	my $after = capture($err);
	is(scalar(grep(/GETLINE FAIL/, @$after)), 0, 'getline not failed');
	is(scalar(grep(/CLOSE FAIL/, @$after)), 1, 'body->close not called');
}

SKIP: {
	my $conn = conn_for($sock, 'excessive header');
	$SIG{PIPE} = 'IGNORE';
	$conn->write("GET /callback HTTP/1.0\r\n");
	foreach my $i (1..500000) {
		last unless $conn->write("X-xxxxxJunk-$i: omg\r\n");
	}
	ok(!$conn->write("\r\n"), 'broken request');
	ok($conn->read(my $buf, 8192), 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	like($head, qr/\b400\b/, 'got 400 response');
}

{
	my $conn = conn_for($sock, 'excessive body Content-Length');
	$SIG{PIPE} = 'IGNORE';
	my $n = (10 * 1024 * 1024) + 1;
	$conn->write("PUT /sha1 HTTP/1.0\r\nContent-Length: $n\r\n\r\n");
	ok($conn->read(my $buf, 8192), 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	like($head, qr/\b413\b/, 'got 413 response');
}

{
	my $conn = conn_for($sock, 'excessive body chunked');
	$SIG{PIPE} = 'IGNORE';
	my $n = (10 * 1024 * 1024) + 1;
	$conn->write("PUT /sha1 HTTP/1.1\r\nTransfer-Encoding: chunked\r\n");
	$conn->write("\r\n".sprintf("%x\r\n", $n));
	ok($conn->read(my $buf, 8192), 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	like($head, qr/\b413\b/, 'got 413 response');
}

{
	my $conn = conn_for($sock, 'chunk with pipeline');
	my $n = 10;
	my $payload = 'b'x$n;
	$conn->write("PUT /sha1 HTTP/1.1\r\nTransfer-Encoding: chunked\r\n");
	$conn->write("\r\n".sprintf("%x\r\n", $n));
	$conn->write($payload . "\r\n0\r\n\r\nGET /empty HTTP/1.0\r\n\r\n");
	$conn->read(my $buf, 4096);
	my $lim = 0;
	$lim++ while ($conn->read($buf, 4096, bytes::length($buf)) && $lim < 9);
	my $exp = sha1_hex($payload);
	like($buf, qr!\r\n\r\n${exp}HTTP/1\.0 200 OK\r\n!s,
		'chunk parser can handled pipelined requests');
}

# Unix domain sockets
{
	my $u = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $upath);
	ok($u, 'unix socket connected');
	$u->write("GET /host-port HTTP/1.0\r\n\r\n");
	$u->read(my $buf, 4096);
	like($buf, qr!\r\n\r\n127\.0\.0\.1:0\z!,
		'set REMOTE_ADDR and REMOTE_PORT for Unix socket');
}

sub conn_for {
	my ($dest, $msg) = @_;
	my $conn = tcp_connect($dest);
	ok($conn, "connected for $msg");
	setsockopt($conn, IPPROTO_TCP, TCP_NODELAY, 1);
	return $conn;
}

{
	my $conn = conn_for($sock, 'host-port');
	$conn->write("GET /host-port HTTP/1.0\r\n\r\n");
	$conn->read(my $buf, 4096);
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	my ($addr, $port) = split(/:/, $body);
	is($addr, $conn->sockhost, 'host matches addr');
	is($port, $conn->sockport, 'port matches');
}

# graceful termination
{
	my $conn = conn_for($sock, 'graceful termination via slow header');
	$conn->write("GET /slow-header HTTP/1.0\r\n" .
			"X-Check-Fifo: $fifo\r\n\r\n");
	open my $f, '>', $fifo or die "open $fifo: $!\n";
	$f->autoflush(1);
	ok(print($f "hello\n"), 'wrote something to fifo');
	my $kpid = $pid;
	$pid = undef;
	is(kill('TERM', $kpid), 1, 'started graceful shutdown');
	ok(print($f "world\n"), 'wrote else to fifo');
	close $f or die "close fifo: $!\n";
	$conn->read(my $buf, 8192);
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr!\AHTTP/1\.[01] 200 OK!, 'got 200 for slow-header');
	is($body, "hello\nworld\n", 'read expected body');
	is(waitpid($kpid, 0), $kpid, 'reaped httpd');
	is($?, 0, 'no error');
	$spawn_httpd->('-W0');
}

{
	my $conn = conn_for($sock, 'graceful termination via slow-body');
	$conn->write("GET /slow-body HTTP/1.0\r\n" .
			"X-Check-Fifo: $fifo\r\n\r\n");
	open my $f, '>', $fifo or die "open $fifo: $!\n";
	$f->autoflush(1);
	my $buf;
	$conn->sysread($buf, 8192);
	like($buf, qr!\AHTTP/1\.[01] 200 OK!, 'got 200 for slow-body');
	like($buf, qr!\r\n\r\n!, 'finished HTTP response header');

	foreach my $c ('a'..'c') {
		$c .= "\n";
		ok(print($f $c), 'wrote line to fifo');
		$conn->sysread($buf, 8192);
		is($buf, $c, 'got trickle for reading');
	}
	my $kpid = $pid;
	$pid = undef;
	is(kill('TERM', $kpid), 1, 'started graceful shutdown');
	ok(print($f "world\n"), 'wrote else to fifo');
	close $f or die "close fifo: $!\n";
	$conn->sysread($buf, 8192);
	is($buf, "world\n", 'read expected body');
	is($conn->sysread($buf, 8192), 0, 'got EOF from server');
	is(waitpid($kpid, 0), $kpid, 'reaped httpd');
	is($?, 0, 'no error');
	$spawn_httpd->('-W0');
}

sub delay { select(undef, undef, undef, shift || rand(0.02)) }

my $str = 'abcdefghijklmnopqrstuvwxyz';
my $len = length $str;
is($len, 26, 'got the alphabet');
my $check_self = sub {
	my ($conn) = @_;
	$conn->read(my $buf, 4096);
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex($str), 'read expected body');
};

SKIP: {
	which('curl') or skip('curl(1) missing', 4);
	my $base = 'http://' . $sock->sockhost . ':' . $sock->sockport;
	my $url = "$base/sha1";
	my ($r, $w);
	pipe($r, $w) or die "pipe: $!";
	my $cmd = [qw(curl --tcp-nodelay --no-buffer -T- -HExpect: -sS), $url];
	my ($out, $err) = ('', '');
	my $h = IPC::Run::start($cmd, $r, \$out, \$err);
	$w->autoflush(1);
	foreach my $c ('a'..'z') {
		print $w $c or die "failed to write to curl: $!";
		delay();
	}
	close $w or die "close write pipe: $!";
	close $r or die "close read pipe: $!";
	IPC::Run::finish($h);
	is($?, 0, 'curl exited successfully');
	is($err, '', 'no errors from curl');
	is($out, sha1_hex($str), 'read expected body');

	open my $fh, '-|', qw(curl -sS), "$base/async-big" or die $!;
	my $n = 0;
	my $non_zero = 0;
	while (1) {
		my $r = sysread($fh, my $buf, 4096) or last;
		$n += $r;
		$buf =~ /\A\0+\z/ or $non_zero++;
	}
	close $fh or die "curl errored out \$?=$?";
	is($n, 30 * 1024 * 1024, 'got expected output from curl');
	is($non_zero, 0, 'read all zeros');
}

{
	my $conn = conn_for($sock, '1.1 pipeline together');
	$conn->write("PUT /sha1 HTTP/1.1\r\nUser-agent: hello\r\n\r\n" .
			"PUT /sha1 HTTP/1.1\r\n\r\n");
	my $buf = '';
	my @r;
	until (scalar(@r) >= 2) {
		my $r = $conn->sysread(my $tmp, 4096);
		die $! unless defined $r;
		die "EOF <$buf>" unless $r;
		$buf .= $tmp;
		@r = ($buf =~ /\r\n\r\n([a-f0-9]{40})/g);
	}
	is(2, scalar @r, 'got 2 responses');
	my $i = 3;
	foreach my $hex (@r) {
		is($hex, sha1_hex(''), "read expected body $i");
		$i++;
	}
}

{
	my $conn = conn_for($sock, 'no TCP_CORK on empty body');
	$conn->write("GET /empty HTTP/1.1\r\nHost:example.com\r\n\r\n");
	my $buf = '';
	my $t0 = [ gettimeofday ];
	until ($buf =~ /\r\n\r\n/s) {
		$conn->sysread($buf, 4096, length($buf));
	}
	my $elapsed = tv_interval($t0, [ gettimeofday ]);
	ok($elapsed < 0.190, 'no 200ms TCP cork delay on empty body');
}

{
	my $conn = conn_for($sock, 'graceful termination during slow request');
	$conn->write("PUT /sha1 HTTP/1.0\r\n");
	delay();
	$conn->write("Content-Length: $len\r\n");
	delay();
	$conn->write("\r\n");
	my $kpid = $pid;
	$pid = undef;
	is(kill('TERM', $kpid), 1, 'started graceful shutdown');
	delay();
	my $n = 0;
	foreach my $c ('a'..'z') {
		$n += $conn->write($c);
	}
	is($n, $len, 'wrote alphabet');
	$check_self->($conn);
	is(waitpid($kpid, 0), $kpid, 'reaped httpd');
	is($?, 0, 'no error');
	$spawn_httpd->('-W0');
}

# various DoS attacks against the chunk parser:
{
	local $SIG{PIPE} = 'IGNORE';
	my $conn = conn_for($sock, '1.1 chunk header excessive');
	$conn->write("PUT /sha1 HTTP/1.1\r\nTransfer-Encoding:chunked\r\n\r\n");
	my $n = 0;
	my $w;
	while ($w = $conn->write('ffffffff')) {
		$n += $w;
	}
	ok($!, 'got error set in $!');
	is($w, undef, 'write error happened');
	ok($n > 0, 'was able to write');
	my $r = $conn->read(my $buf, 66666);
	ok($r > 0, 'got non-empty response');
	like($buf, qr!HTTP/1\.\d 400 !, 'got 400 response');

	$conn = conn_for($sock, '1.1 chunk trailer excessive');
	$conn->write("PUT /sha1 HTTP/1.1\r\nTransfer-Encoding:chunked\r\n\r\n");
	is($conn->syswrite("1\r\na"), 4, 'wrote first header + chunk');
	delay();
	$n = 0;
	while ($w = $conn->write("\r")) {
		$n += $w;
	}
	ok($!, 'got error set in $!');
	ok($n > 0, 'wrote part of chunk end (\r)');
	$r = $conn->read($buf, 66666);
	ok($r > 0, 'got non-empty response');
	like($buf, qr!HTTP/1\.\d 400 !, 'got 400 response');
}

{
	my $conn = conn_for($sock, '1.1 chunked close trickle');
	$conn->write("PUT /sha1 HTTP/1.1\r\nConnection:close\r\n");
	$conn->write("Transfer-encoding: chunked\r\n\r\n");
	foreach my $x ('a'..'z') {
		delay();
		$conn->write('1');
		delay();
		$conn->write("\r");
		delay();
		$conn->write("\n");
		delay();
		$conn->write($x);
		delay();
		$conn->write("\r");
		delay();
		$conn->write("\n");
	}
	$conn->write('0');
	delay();
	$conn->write("\r");
	delay();
	$conn->write("\n");
	delay();
	$conn->write("\r");
	delay();
	$conn->write("\n");
	delay();
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, '1.1 chunked close');
	$conn->write("PUT /sha1 HTTP/1.1\r\nConnection:close\r\n");
	my $xlen = sprintf('%x', $len);
	$conn->write("Transfer-Encoding: chunked\r\n\r\n$xlen\r\n" .
		"$str\r\n0\r\n\r\n");
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, 'chunked body + pipeline');
	$conn->write("PUT /sha1 HTTP/1.1\r\n" .
			"Transfer-Encoding: chunked\r\n");
	delay();
	$conn->write("\r\n1\r\n");
	delay();
	$conn->write('a');
	delay();
	$conn->write("\r\n0\r\n\r\nPUT /sha1 HTTP/1.1\r\n");
	delay();

	my $buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		$conn->sysread(my $tmp, 4096);
		$buf .= $tmp;
	}
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex('a'), 'read expected body');

	$conn->write("Connection: close\r\n");
	$conn->write("Content-Length: $len\r\n\r\n$str");
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, 'trickle header, one-shot body + pipeline');
	$conn->write("PUT /sha1 HTTP/1.0\r\n" .
			"Connection: keep-alive\r\n");
	delay();
	$conn->write("Content-Length: $len\r\n\r\n${str}PUT");
	my $buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		$conn->sysread(my $tmp, 4096);
		$buf .= $tmp;
	}
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex($str), 'read expected body');

	$conn->write(" /sha1 HTTP/1.0\r\nContent-Length: $len\r\n\r\n$str");
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, 'trickle body');
	$conn->write("PUT /sha1 HTTP/1.0\r\n");
	$conn->write("Content-Length: $len\r\n\r\n");
	my $beg = substr($str, 0, 10);
	my $end = substr($str, 10);
	is($beg . $end, $str, 'substr setup correct');
	delay();
	$conn->write($beg);
	delay();
	$conn->write($end);
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, 'one-shot write');
	$conn->write("PUT /sha1 HTTP/1.0\r\n" .
			"Content-Length: $len\r\n\r\n$str");
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, 'trickle header, one-shot body');
	$conn->write("PUT /sha1 HTTP/1.0\r\n");
	delay();
	$conn->write("Content-Length: $len\r\n\r\n$str");
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, '1.1 Connnection: close');
	$conn->write("PUT /sha1 HTTP/1.1\r\nConnection:close\r\n");
	delay();
	$conn->write("Content-Length: $len\r\n\r\n$str");
	$check_self->($conn);
}

{
	my $conn = conn_for($sock, '1.1 pipeline start');
	$conn->write("PUT /sha1 HTTP/1.1\r\n\r\nPUT");
	my $buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		$conn->sysread(my $tmp, 4096);
		$buf .= $tmp;
	}
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex(''), 'read expected body');

	# 2nd request
	$conn->write(" /sha1 HTTP/1.1\r\n\r\n");
	$buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		$conn->sysread(my $tmp, 4096);
		$buf .= $tmp;
	}
	($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex(''), 'read expected body #2');
}

SKIP: {
	skip 'TCP_DEFER_ACCEPT is Linux-only', 1 if $^O ne 'linux';
	my $var = Socket::TCP_DEFER_ACCEPT();
	defined(my $x = getsockopt($sock, IPPROTO_TCP, $var)) or die;
	is(unpack('i', $x), $defer_accept_val,
		'TCP_DEFER_ACCEPT unchanged if previously set');
};
SKIP: {
	skip 'SO_ACCEPTFILTER is FreeBSD-only', 1 if $^O ne 'freebsd';
	skip 'accf_data not loaded: kldload accf_data' if !defined $accf_arg;
	my $var = PublicInbox::Daemon::SO_ACCEPTFILTER();
	defined(my $x = getsockopt($sock, SOL_SOCKET, $var)) or die;
	is($x, $accf_arg, 'SO_ACCEPTFILTER unchanged if previously set');
};
SKIP: {
	skip 'only testing lsof(8) output on Linux', 1 if $^O ne 'linux';
	skip 'no lsof in PATH', 1 unless which('lsof');
	my @lsof = `lsof -p $pid`;
	is_deeply([grep(/\bdeleted\b/, @lsof)], [], 'no lingering deleted inputs');
	is_deeply([grep(/\bpipe\b/, @lsof)], [], 'no extra pipes with -W0');
};

done_testing();

sub capture {
	my ($f) = @_;
	open my $fh, '+<', $f or die "failed to open $f: $!\n";
	local $/ = "\n";
	my @r = <$fh>;
	truncate($fh, 0) or die "truncate failed on $f: $!\n";
	\@r
}

1;
