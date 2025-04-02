#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# note: our HTTP server should be standalone and capable of running
# generic PSGI/Plack apps.
use v5.12; use PublicInbox::TestCommon;
use autodie qw(close getsockopt open pipe read rename seek setsockopt sysread
	syswrite truncate unlink);
use PublicInbox::DS qw(now);
use PublicInbox::IO qw(poll_in write_file);
use PublicInbox::Spawn qw(spawn popen_rd);
require_mods '-httpd';
use PublicInbox::SHA qw(sha1_hex);
use IO::Handle ();
use IO::Socket::UNIX;
use Fcntl qw(:seek);
use Socket qw(IPPROTO_TCP TCP_NODELAY SOL_SOCKET);
use POSIX qw(mkfifo);
use Carp ();
my $tmpdir = tmpdir;
my $fifo = "$tmpdir/fifo";
ok(defined mkfifo($fifo, 0777), 'created FIFO');
my $err = "$tmpdir/stderr.log";
my $out = "$tmpdir/stdout.log";
my $psgi = "./t/httpd-corner.psgi";
my $sock = tcp_server();
my @zmods = qw(PublicInbox::GzipFilter IO::Uncompress::Gunzip);
my $base_url = 'http://'.tcp_host_port($sock);
use Config;

# Make sure we don't clobber socket options set by systemd or similar
# using socket activation:
my ($defer_accept_val, $accf_arg, $TCP_DEFER_ACCEPT);
SKIP: {
	skip 'TCP_DEFER_ACCEPT is Linux-only', 1 if $^O ne 'linux';
	$TCP_DEFER_ACCEPT = eval { Socket::TCP_DEFER_ACCEPT() } // 9;
	setsockopt($sock, IPPROTO_TCP, $TCP_DEFER_ACCEPT, 5);
	my $x = getsockopt($sock, IPPROTO_TCP, $TCP_DEFER_ACCEPT);
	$defer_accept_val = unpack('i', $x);
	ok($defer_accept_val > 0, 'TCP_DEFER_ACCEPT val non-zero') or
		xbail "unexpected TCP_DEFER_ACCEPT value: $defer_accept_val";
}
SKIP: {
	require_mods '+accf_data';
	require PublicInbox::Daemon;
	my $var = $PublicInbox::Daemon::SO_ACCEPTFILTER;
	$accf_arg = pack('a16a240', 'dataready', '');
	setsockopt($sock, SOL_SOCKET, $var, $accf_arg);
}

my $upath = "$tmpdir/s";
my $unix = IO::Socket::UNIX->new(Listen => 1024,
		Type => Socket::SOCK_STREAM(),
		Local => $upath,
	) or BAIL_OUT "bind + listen $upath: $!";
$unix->blocking(0);
my $alt = tcp_server();
my $td;
my $spawn_httpd = sub {
	my (@args) = @_;
	my $x = tcp_host_port($alt);
	my $cmd = [ '-httpd', @args, "--stdout=$out", "--stderr=$err", $psgi,
		'-l', "http://$x/?psgi=t/alt.psgi,env.PI_CONFIG=/path/to/alt".
			",err=$tmpdir/alt.err" ];
	my $env = { PI_CONFIG => '/dev/null' };
	$td = start_script($cmd, $env, { 3 => $sock, 4 => $unix, 5 => $alt });
};

my $capture = sub {
	my ($f) = @_;
	open my $fh, '+<', $f;
	local $/ = "\n";
	my @r = <$fh>;
	truncate $fh, 0;
	\@r
};

my $mkreq = sub ($$@) {
	my $srv = shift;
	my $msg = shift;
	my $c = tcp_connect $srv;
	setsockopt($c, IPPROTO_TCP, TCP_NODELAY, 1) if @_ > 1;
	for (@_) {
		print $c $_ or Carp::croak "print ($msg) $!";
	}
	$c;
};

$spawn_httpd->();
{
	my $conn = $mkreq->($alt, 'alt PSGI path', "GET / HTTP/1.0\r\n\r\n");
	read $conn, my $buf, 4096;
	like($buf, qr!^/path/to/alt\z!sm,
		'alt.psgi loaded on alt socket with correct env');

	$conn = $mkreq->($sock, 'default PSGI path',
		"GET /PI_CONFIG HTTP/1.0\r\n\r\n");
	read $conn, $buf, 4096;
	like $buf, qr!^/dev/null\z!sm, 'default PSGI on original socket';
	my $log = $capture->("$tmpdir/alt.err");
	ok(grep(/ALT/, @$log), 'alt psgi.errors written to');
	$log = $capture->($err);
	ok(!grep(/ALT/, @$log), 'STDERR not written to');
	is(unlink($err, "$tmpdir/alt.err"), 2, 'unlinked stderr and alt.err');

	$td->kill('USR1'); # trigger reopen_logs
}

if ('test worker death') {
	my $conn = $mkreq->($sock, 'killed worker',
		"GET /pid HTTP/1.1\r\nHost:example.com\r\n\r\n");
	my $pid;
	while (defined(my $line = <$conn>)) {
		next unless $line eq "\r\n";
		chomp($pid = <$conn>);
		last;
	}
	like($pid, qr/\A[0-9]+\z/, '/pid response');
	is(kill('KILL', $pid), 1, 'killed worker');
	is <$conn>, undef, 'worker died and EOF-ed client';

	$conn = $mkreq->($sock, 'respawned worker',
		"GET /pid HTTP/1.0\r\n\r\n");
	read $conn, my $buf, 8192;
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	chomp($body);
	like($body, qr/\A[0-9]+\z/, '/pid response');
	ok kill(0, $body), 'valid PID for new worker';
	isnt($body, $pid, 'respawned worker');
}
{ # check on prior USR1 signal
	ok(-e $err, 'stderr recreated after USR1');
	ok(-e "$tmpdir/alt.err", 'alt.err recreated after USR1');
}

my $ck_env = sub {
	my ($env, $exp, $only) = @_;
	for my $k (sort keys %$exp) {
		my $v = $exp->{$k};
		is $env->{$k}, $v, "`$k' matches expected ($v)";
	}
	return if !$only;
	for (sort grep { !defined($exp->{$_}) } grep /^HTTP_/, keys %$env) {
		ok !(defined $env->{$_}), "`$_' defined unexpectedly";
	}
};
my $put = <<EOM;
PUT /env HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: chunked\r
EOM
my $chunk_body = "\r\n1\r\n0\r\n0\r\n";
if ('successful Trailer cases') {
	for my $t (
['2 good trailers',  { a => 'z', b => 'y' }],
['1 good trailer', { a => 'b' }],
['1 multi-line trailer', { a => "multi\r\n line" }],
['2 multi-line trailers', { two => "m\r\n ul", ti => "li\r\n e" }],
			) {
		my ($d, $tlr) = @$t;
		my ($hdr, $end) = ('Trailer: ', '');
		my %exp = (
			HTTP_TRAILER => join(', ', keys %$tlr),
			HTTP_HOST => 'example.com',
			HTTP_TRANSFER_ENCODING => 'chunked',
			'test.input_data' => '0',
		);
		$hdr .= $exp{HTTP_TRAILER} . "\r\n";
		for my $k (keys %$tlr) {
			my $v = $tlr->{$k};
			$end .= "$k: $v\r\n";
			$v =~ s/\r\n[ \t]+/ /sg;
			$exp{"HTTP_\U$k"} = $v;
		}
		my $c = $mkreq->($sock, $d, "$put$hdr$chunk_body$end\r\n");
		my $buf = do { local $/ = "\r\n\r\n"; <$c> };
		like $buf, qr!^HTTP/1\.. 200\b!, "request w/ $d" or next;
		{ local $/ = "\n.\n"; chomp($buf = <$c>) }
		my $env = eval $buf // die "eval $@ ($buf)"; # Perl hashref
		$ck_env->($env, \%exp, 1);
	}
}

if ('test rejected trailers') {
	for my $t (
['unexpected trailer only', $put.$chunk_body."unexpected: bye\r\n"],
['expected + unexpected trailer',
	$put."Trailer: a\r\n".$chunk_body."unexpected: !\r\na: b\r\n"],
['missing expected trailer', $put."Trailer: a\r\n".$chunk_body."\r\n"],
['Content-Length in trailer',
	$put."Trailer: Content-Length\r\n".$chunk_body."Content-Length: 1\r\n"],
['Host in trailer',
	$put."Trailer: Host\r\n".$chunk_body."Host: example.com\r\n"],
['long trailer',
	$put."Trailer: long\r\n".$chunk_body.'Long: '.('a' x 0x8000)."\r\n"],
['trailer w/ Content-Length header',
	"PUT /env HTTP/1.1\r\nHost: example.com\r\n".
	"Content-Length: 11\r\nTrailer: a\r\n".$chunk_body."a: b\r\n"]
	) {
		my ($d, $req) = @$t;
		my $c = $mkreq->($sock, $d, $req."\r\n");
		poll_in $c, 30_000 or Carp::croak "timeout";
		my $buf = do { local $/ = "\r\n\r\n"; <$c> };
		like $buf, qr!^HTTP/1\.. 400\b!, "$d rejected";
	}
}

{
	my $d = 'trailer appends to header';
	my $hdr = "Trailer: c\r\nc: a\r\n";
	my $end = "C: b\r\n";
	my $c = $mkreq->($sock, $d, "$put$hdr$chunk_body$end\r\n");
	poll_in $c, 30_000 or Carp::croak "timeout";
	my $buf = do { local $/ = "\r\n\r\n"; <$c> };
	like $buf, qr!^HTTP/1\.. 200\b!, "request w/ $d" or next;
	{ local $/ = "\n.\n"; chomp($buf = <$c>) }
	my $env = eval $buf // die "eval $@ ($buf)"; # Perl hashref
	my %exp = (
		HTTP_TRAILER => 'c',
		HTTP_HOST => 'example.com',
		HTTP_TRANSFER_ENCODING => 'chunked',
		HTTP_C => 'a, b',
		'test.input_data' => '0',
	);
	$ck_env->($env, \%exp, 1);
}

# I don't trust myself to read RFCs properly and need a 3rd-party client:
my $tup = "t/trailer-up-$Config{archname}";
my @tup_h_st = stat 't/trailer-up.h' or die "stat('t/trailer-up.h'): $!";
SKIP: if (!-e $tup || (stat(_))[10] < $tup_h_st[10]) {
	my $curl_config = require_cmd 'curl-config', 1;
	my %ccfg;
	for my $f (qw(version cc cflags libs)) {
		chomp($ccfg{$f} = xqx [ $curl_config, "--$f" ]);
		skip "$curl_config --$f \$?=$?", 1 if $?;
	}
	$ccfg{version} =~ /([0-9]+\.[0-9\.]+)/ or
		skip "can't parse `$curl_config --version`: $ccfg{version}", 1;
	my $curl_ver = eval 'v'.$1;
	skip "libcurl $ccfg{version} <7.64.0 for CURLOPT_TRAILERFUNCTION", 1
		if $curl_ver lt v7.64.0;
	write_file '>', "$tmpdir/trailer-up.c", qq{#include <trailer-up.h>\n};
	my $cc = require_cmd $ccfg{cc}, 1;
	my @build = split ' ',
		"$cc $ccfg{cflags} -I t -o $tup.$<.$$.tmp ".
		"$tmpdir/trailer-up.c $ccfg{libs}";
	xsys(\@build) and skip "@build failed: \$?=$?", 1;
	rename "$tup.$<.$$.tmp", $tup;
	stat $tup; # for _ below:
} # SKIP
if (-x _) {
	my %opt = (0 => \'i', 1 => \(my $o = ''), 2 => \(my $e = ''));
	xsys [ $tup, "$base_url/env" ], undef,  \%opt;
	is $?, 0, 'trailer-up using libcurl';
	my ($buf) = split /\n\.\n/, $o;
	my $env = eval $buf // die "eval $@ ($buf)"; # Perl hashref
	$ck_env->($env, { 'test.input_data' => 'i', HTTP_A => 'b',
		HTTP_TRAILER => 'a' });
}

{
	my $conn = $mkreq->($sock, 'Header spaces bogus',
		"GET /empty HTTP/1.1\r\nSpaced-Out : 3\r\n\r\n");
	sysread $conn, my $buf, 4096;
	like($buf, qr!\AHTTP/1\.[0-9] 400 !, 'got 400 response on bad request');
}
{
	my $conn = $mkreq->($sock, 'streaming callback',
		"GET /callback HTTP/1.0\r\n\r\n");
	read $conn, my $buf, 8192;
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	is($body, "hello world\n", 'callback body matches expected');
}

{
	my $conn = $mkreq->($sock, 'getline-die',
		"GET /getline-die HTTP/1.1\r\nHost: example.com\r\n\r\n");
	read $conn, my $buf, 8192;
	like($buf, qr!HTTP/1\.1 200\b[^\r]*\r\n!, 'got some sort of header');
	is read($conn, my $nil, 8192), 0, 'read EOF';
	$conn = undef;
	my $after = $capture->($err);
	is(scalar(grep(/GETLINE FAIL/, @$after)), 1, 'failure logged');
	is(scalar(grep(/CLOSE FAIL/, @$after)), 1, 'body->close not called');
}

{
	my $conn = $mkreq->($sock, 'close-die',
		"GET /close-die HTTP/1.1\r\nHost: example.com\r\n\r\n");
	read $conn, my $buf, 8192;
	like($buf, qr!HTTP/1\.1 200\b[^\r]*\r\n!, 'got some sort of header');
	is read($conn, my $nil, 8192), 0, 'read EOF';
	$conn = undef;
	my $after = $capture->($err);
	is(scalar(grep(/GETLINE FAIL/, @$after)), 0, 'getline not failed');
	is(scalar(grep(/CLOSE FAIL/, @$after)), 1, 'body->close not called');
}

my $check_400 = sub {
	my ($conn) = @_;
	my $r = CORE::sysread $conn, my $buf, 8192;
	# ECONNRESET and $r==0 are both observed on FreeBSD 11.2
	if (!defined($r)) {
		ok($!{ECONNRESET}, 'ECONNRESET on read (BSD sometimes)');
	} elsif ($r > 0) {
		like($buf, qr!\AHTTP/1\.\d 400 !, 'got 400 response');
	} else {
		is($r, 0, 'got EOF (BSD sometimes)');
	}
	CORE::close($conn); # ensure we don't get SIGPIPE later
};

{
	local $SIG{PIPE} = 'IGNORE';
	my $conn = $mkreq->($sock, 'excessive header',
		"GET /callback HTTP/1.0\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	for my $i (1..500000) {
		print $conn "X-xxxxxJunk-$i: omg\r\n" or last;
	}
	ok !(print $conn "\r\n"), 'broken request';
	$check_400->($conn);
}

{
	my $n = (10 * 1024 * 1024) + 1;
	my $conn = $mkreq->($sock, 'excessive body Content-Length',
		"PUT /sha1 HTTP/1.0\r\nContent-Length: $n\r\n\r\n");
	my $r = read $conn, my $buf, 8192;
	ok($r > 0, 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	like($head, qr/\b413\b/, 'got 413 response');
}

{
	my $n = (10 * 1024 * 1024) + 1;
	my $conn = $mkreq->($sock, 'excessive body chunked',
		"PUT /sha1 HTTP/1.1\r\nTransfer-Encoding: chunked\r\n",
		"\r\n".sprintf("%x\r\n", $n));
	my $r = read $conn, my $buf, 8192;
	ok($r > 0, 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	like($head, qr/\b413\b/, 'got 413 response');
}

{
	my $conn = $mkreq->($sock, '1.1 Transfer-Encoding bogus',
		"PUT /sha1 HTTP/1.1\r\nTransfer-Encoding: bogus\r\n\r\n");
	sysread $conn, my $buf, 4096;
	like($buf, qr!\AHTTP/1\.[0-9] 400 !, 'got 400 response on bogus TE');
}
{
	my $conn = $mkreq->($sock, '1.1 Content-Length bogus',
		"PUT /sha1 HTTP/1.1\r\nContent-Length: 3.3\r\n\r\n");
	sysread $conn, my $buf, 4096;
	like($buf, qr!\AHTTP/1\.[0-9] 400 !, 'got 400 response on bad length');
}

{
	my $req = "PUT /sha1 HTTP/1.1\r\nContent-Length: 3\r\n" .
			"Content-Length: 3\r\n\r\n";
	# this is stricter than it needs to be.  Due to the way
	# Plack::HTTPParser, PSGI specs, and how hash tables work in common
	# languages; it's not possible to tell the difference between folded
	# and intentionally bad commas (e.g. "Content-Length: 3, 3")
	if (0) {
		require Plack::HTTPParser; # XS or pure Perl
		Plack::HTTPParser::parse_http_request($req, my $env = {});
		diag explain($env); # "Content-Length: 3, 3"
	}
	my $conn = $mkreq->($sock, '1.1 Content-Length dupe', $req);
	sysread $conn, my $buf, 4096;
	like($buf, qr!\AHTTP/1\.[0-9] 400 !, 'got 400 response on dupe length');
}

{
	my $n = 10;
	my $payload = 'b'x$n;
	my $conn = $mkreq->($sock, 'chunk with pipeline',
		"PUT /sha1 HTTP/1.1\r\nTransfer-Encoding: chunked\r\n",
		"\r\n".sprintf("%x\r\n", $n),
		$payload . "\r\n0\r\n\r\nGET /empty HTTP/1.0\r\n\r\n");
	sysread $conn, my $buf, 4096;
	my $lim = 0;
	$lim++ while (sysread($conn, $buf, 4096, length($buf)) && $lim < 9);
	my $exp = sha1_hex($payload);
	like($buf, qr!\r\n\r\n${exp}HTTP/1\.0 200 OK\r\n!s,
		'chunk parser can handled pipelined requests');
}

# Unix domain sockets
{
	my $u = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $upath);
	ok($u, 'unix socket connected');
	print $u "GET /host-port HTTP/1.0\r\n\r\n";
	read $u, my $buf, 4096;
	like($buf, qr!\r\n\r\n127\.0\.0\.1 0\z!,
		'set REMOTE_ADDR and REMOTE_PORT for Unix socket');
}

{
	my $conn = $mkreq->($sock, 'host-port',
		"GET /host-port HTTP/1.0\r\n\r\n");
	read $conn, my $buf, 4096;
	my ($head, $body) = split(/\r\n\r\n/, $buf);
	my ($addr, $port) = split(/ /, $body);
	is($addr, (tcp_host_port($conn))[0], 'host matches addr');
	is($port, $conn->sockport, 'port matches');
}

# graceful termination
{
	my $conn = $mkreq->($sock, 'graceful termination via slow header',
			"GET /slow-header HTTP/1.0\r\n" .
			"X-Check-Fifo: $fifo\r\n\r\n");
	open my $f, '>', $fifo;
	$f->autoflush(1);
	ok(print($f "hello\n"), 'wrote something to fifo');
	is($td->kill, 1, 'started graceful shutdown');
	ok(print($f "world\n"), 'wrote else to fifo');
	close $f;
	read $conn, my $buf, 8192; # read until EOF (no sysread)
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr!\AHTTP/1\.[01] 200 OK!, 'got 200 for slow-header');
	is($body, "hello\nworld\n", 'read expected body');
	$td->join;
	is($?, 0, 'no error');
	$spawn_httpd->('-W0');
}

{
	my $conn = $mkreq->($sock, 'graceful termination via slow-body',
		"GET /slow-body HTTP/1.0\r\nX-Check-Fifo: $fifo\r\n\r\n");
	open my $f, '>', $fifo;
	$f->autoflush(1);
	sysread $conn, my $buf, 8192;
	like($buf, qr!\AHTTP/1\.[01] 200 OK!, 'got 200 for slow-body');
	like($buf, qr!\r\n\r\n!, 'finished HTTP response header');

	foreach my $c ('a'..'c') {
		$c .= "\n";
		ok(print($f $c), 'wrote line to fifo');
		sysread $conn, $buf, 8192;
		is($buf, $c, 'got trickle for reading');
	}
	is($td->kill, 1, 'started graceful shutdown');
	ok(print($f "world\n"), 'wrote else to fifo');
	close $f;
	sysread $conn, $buf, 8192;
	is($buf, "world\n", 'read expected body');
	is(sysread($conn, $buf, 8192), 0, 'got EOF from server');
	$td->join;
	is($?, 0, 'no error');
	$spawn_httpd->('-W0');
}

my $delay = sub { tick(shift || rand(0.02)) };

my $str = 'abcdefghijklmnopqrstuvwxyz';
my $len = length $str;
is($len, 26, 'got the alphabet');
my $check_self = sub {
	my ($conn) = @_;
	poll_in $conn, 30_000 or Carp::croak "timeout";
	read $conn, my $buf, 4096;
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex($str), 'read expected body');
};

SKIP: {
	my $curl = require_cmd('curl', 1) or skip('curl(1) missing', 4);
	my $url = "$base_url/sha1";
	my ($r, $w);
	pipe $r, $w;
	my $cmd = [$curl, qw(--tcp-nodelay -T- -HExpect: -gsSN), $url];
	open my $cout, '+>', undef;
	open my $cerr, '>', undef;
	my $rdr = { 0 => $r, 1 => $cout, 2 => $cerr };
	my $pid = spawn($cmd, undef, $rdr);
	close $r;
	foreach my $c ('a'..'z') {
		print $w $c or die "failed to write to curl: $!";
		$delay->();
	}
	close $w;
	waitpid($pid, 0);
	is($?, 0, 'curl exited successfully');
	is(-s $cerr, 0, 'no errors from curl');
	seek($cout, 0, SEEK_SET);
	is(<$cout>, sha1_hex($str), 'read expected body');

	my $fh = popen_rd([$curl, '-gsS', "$base_url/async-big"]);
	my $n = 0;
	my $non_zero = 0;
	while (1) {
		my $r = sysread($fh, my $buf, 4096) or last;
		$n += $r;
		$buf =~ /\A\0+\z/ or $non_zero++;
	}
	$fh->close or die "close curl pipe: $!";
	is($?, 0, 'curl succesful');
	is($n, 30 * 1024 * 1024, 'got expected output from curl');
	is($non_zero, 0, 'read all zeros');

	require_mods(@zmods, 4);
	my $buf = xqx([$curl, '-gsS', "$base_url/psgi-yield-gzip"]);
	is($?, 0, 'curl succesful');
	IO::Uncompress::Gunzip::gunzip(\$buf => \(my $out));
	is($out, "hello world\n");
	my $curl_rdr = { 2 => \(my $curl_err = '') };
	$buf = xqx([$curl, qw(-gsSv --compressed),
			"$base_url/psgi-yield-compressible"], undef, $curl_rdr);
	is($?, 0, 'curl --compressed successful');
	is($buf, "goodbye world\n", 'gzipped response as expected');
	like($curl_err, qr/\bContent-Encoding: gzip\b/,
		'curl got gzipped response');
}

{
	my $conn = $mkreq->($sock, 'psgi_yield ENOENT',
		"GET /psgi-yield-enoent HTTP/1.1\r\n\r\n");
	my $buf = '';
	sysread($conn, $buf, 16384, length($buf)) until $buf =~ /\r\n\r\n/;
	like($buf, qr!HTTP/1\.[01] 500\b!, 'got 500 error on ENOENT');
}

{
	my $conn = $mkreq->($sock, '1.1 pipeline together',
		"PUT /sha1 HTTP/1.1\r\nUser-agent: hello\r\n\r\n" .
			"PUT /sha1 HTTP/1.1\r\n\r\n");
	my $buf = '';
	my @r;
	until (scalar(@r) >= 2) {
		my $r = sysread $conn, my $tmp, 4096;
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
	my $conn = $mkreq->($sock, 'no TCP_CORK on empty body',
		"GET /empty HTTP/1.1\r\nHost:example.com\r\n\r\n");
	my $buf = '';
	my $t0 = now;
	until ($buf =~ /\r\n\r\n/s) {
		sysread $conn, $buf, 4096, length($buf);
	}
	my $elapsed = now - $t0;
	ok($elapsed < 0.190, 'no 200ms TCP cork delay on empty body');
}

{
	my $conn = $mkreq->($sock, 'graceful termination during slow request',
		"PUT /sha1 HTTP/1.0\r\nContent-Length: $len\r\n\r\n");

	# XXX ugh, want a reliable and non-intrusive way to detect
	# that the server has started buffering our partial request so we
	# can reliably test graceful termination.  Maybe making this and
	# similar tests dependent on Linux strace is a possibility?
	$delay->(0.1);

	is($td->kill, 1, 'start graceful shutdown');
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	my $n = 0;
	$n += syswrite($conn, $_) for ('a'..'z');
	ok(kill(0, $td->{pid}), 'graceful shutdown did not kill httpd');
	is($n, $len, 'wrote alphabet');
	$check_self->($conn);
	$td->join;
	is($?, 0, 'no error');
	$spawn_httpd->('-W0');
}

# various DoS attacks against the chunk parser:
{
	local $SIG{PIPE} = 'IGNORE';
	my $conn = $mkreq->($sock, '1.1 chunk header excessive',
		"PUT /sha1 HTTP/1.1\r\nTransfer-Encoding:chunked\r\n\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	my $n = 0;
	my $w;
	while ($w = print $conn 'ffffffff') {
		$n += $w;
	}
	ok($!, 'got error set in $!');
	is($w, undef, 'write error happened');
	ok($n > 0, 'was able to write');
	$check_400->($conn);
	$conn = $mkreq->($sock, '1.1 chunk trailer excessive',
		"PUT /sha1 HTTP/1.1\r\nTransfer-Encoding:chunked\r\n\r\n");

	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	is(syswrite($conn, "1\r\na"), 4, 'wrote first header + chunk');
	$delay->();
	$n = 0;
	while ($w = print $conn "\r") {
		$n += $w;
	}
	ok($!, 'got error set in $!');
	ok($n > 0, 'wrote part of chunk end (\r)');
	$check_400->($conn);
}

{
	my $conn = $mkreq->($sock, '1.1 chunked close trickle',
		"PUT /sha1 HTTP/1.1\r\nConnection:close\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	print $conn "Transfer-encoding: chunked\r\n\r\n";
	foreach my $x ('a'..'z') {
		for (split //, "1\r\n$x\r\n") {
			$delay->();
			print $conn $_;
		}
	}
	for (split //, "0\r\n\r\n") {
		$delay->();
		print $conn $_;
	}
	$delay->();
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, '1.1 chunked close',
		"PUT /sha1 HTTP/1.1\r\nConnection:close\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	my $xlen = sprintf('%x', $len);
	print $conn "Transfer-Encoding: chunked\r\n\r\n$xlen\r\n",
			$str, "\r\n0\r\n\r\n";
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, 'chunked body + pipeline',
		"PUT /sha1 HTTP/1.1\r\n"."Transfer-Encoding: chunked\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	for ("\r\n1\r\n", 'a', "\r\n0\r\n\r\nPUT /sha1 HTTP/1.1\r\n") {
		$delay->();
		print $conn $_;
	}
	$delay->();

	my $buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		sysread $conn, $buf, 4096, length($buf);
	}
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex('a'), 'read expected body');

	print $conn "Connection: close\r\n";
	print $conn "Content-Length: $len\r\n\r\n$str";
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, 'trickle header, one-shot body + pipeline',
		"PUT /sha1 HTTP/1.0\r\n"."Connection: keep-alive\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	$delay->();
	print $conn "Content-Length: $len\r\n\r\n", $str, 'PUT';
	my $buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		sysread $conn, $buf, 4096, length($buf);
	}
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex($str), 'read expected body');

	print $conn " /sha1 HTTP/1.0\r\nContent-Length: $len\r\n\r\n", $str;
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, 'trickle body',
		"PUT /sha1 HTTP/1.0\r\n", "Content-Length: $len\r\n\r\n");
	my $beg = substr($str, 0, 10);
	my $end = substr($str, 10);
	is($beg . $end, $str, 'substr setup correct');
	$delay->();
	print $conn $beg;
	$delay->();
	print $conn $end;
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, 'one-shot write',
		"PUT /sha1 HTTP/1.0\r\n" . "Content-Length: $len\r\n\r\n$str");
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, 'trickle header, one-shot body',
		"PUT /sha1 HTTP/1.0\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	$delay->();
	print $conn "Content-Length: $len\r\n\r\n", $str;
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, '1.1 Connection: close',
		"PUT /sha1 HTTP/1.1\r\nConnection:close\r\n");
	setsockopt $conn, IPPROTO_TCP, TCP_NODELAY, 1;
	$delay->();
	print $conn "Content-Length: $len\r\n\r\n$str";
	$check_self->($conn);
}

{
	my $conn = $mkreq->($sock, '1.1 pipeline start',
		"PUT /sha1 HTTP/1.1\r\n\r\nPUT");
	my $buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		sysread $conn, $buf, 4096, length($buf);
	}
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex(''), 'read expected body');

	# finish 2nd request
	print $conn " /sha1 HTTP/1.1\r\n\r\n";
	$buf = '';
	until ($buf =~ /\r\n\r\n[a-f0-9]{40}\z/) {
		sysread $conn, $buf, 4096, length($buf);
	}
	($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr/\r\nContent-Length: 40\r\n/s, 'got expected length');
	is($body, sha1_hex(''), 'read expected body #2');
}

SKIP: {
	skip 'TCP_DEFER_ACCEPT is Linux-only', 1 if $^O ne 'linux';
	my $var = $TCP_DEFER_ACCEPT;
	my $x = getsockopt($sock, IPPROTO_TCP, $var);
	is(unpack('i', $x), $defer_accept_val,
		'TCP_DEFER_ACCEPT unchanged if previously set');
};
SKIP: {
	require_mods '+accf_data';
	my $var = $PublicInbox::Daemon::SO_ACCEPTFILTER;
	my $x = getsockopt($sock, SOL_SOCKET, $var);
	is($x, $accf_arg, 'SO_ACCEPTFILTER unchanged if previously set');
};

SKIP: {
	skip 'only testing /proc/PID/fd on Linux', 1 if $^O ne 'linux';
	my $fd_dir = "/proc/$td->{pid}/fd";
	-d $fd_dir or skip '/proc/$PID/fd missing', 1;
	my @child = grep defined, map readlink, glob "$fd_dir/*";
	my @d = grep /\(deleted\)/, @child;
	is_deeply(\@d, [], 'no lingering deleted inputs') or diag explain(\@d);

	# filter out pipes inherited from the parent
	my @this = grep defined, map readlink, glob "/proc/$$/fd/*";
	my $extract_inodes = sub { map { $_ => 1 } grep /\bpipe\b/, @_ };
	my %child = $extract_inodes->(@child);
	my %parent = $extract_inodes->(@this);
	delete @child{(keys %parent)};
	is_deeply([], [keys %child], 'no extra pipes with -W0') or
		diag explain([child => \%child, parent => \%parent]);
};

# ensure compatibility with other PSGI servers
SKIP: {
	require_mods @zmods, 'psgi', 3;
	STDERR->flush;
	open my $olderr, '>&', \*STDERR;
	open my $tmperr, '+>', undef;
	open STDERR, '>&', $tmperr;
	STDERR->autoflush(1);
	my $app = require $psgi;
	test_psgi($app, sub {
		my ($cb) = @_;
		my $req = GET('http://example.com/psgi-yield-gzip');
		my $res = $cb->($req);
		my $buf = $res->content;
		IO::Uncompress::Gunzip::gunzip(\$buf => \(my $out));
		is($out, "hello world\n", 'got expected output');

		$req = GET('http://example.com/psgi-yield-enoent');
		$res = $cb->($req);
		is($res->code, 500, 'got error on ENOENT');
		seek $tmperr, 0, SEEK_SET;
		my $errbuf = do { local $/; <$tmperr> };
		like($errbuf, qr/this-better-not-exist/,
			'error logged about missing command');
	});
	open STDERR, '>&', $olderr;
}

done_testing();
