#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12; use PublicInbox::TestCommon; use autodie;
use Socket ();
use POSIX ();
use PublicInbox::Spawn qw(popen_rd);
use PublicInbox::IO qw(write_file);
my $_curl = require_cmd 'curl';
my @curl = ($_curl, qw(--max-time 30));
require_mods qw(-httpd Plack::Builder HTTP::Parser::XS);
my $psgi = "./t/httpd-corner.psgi";
my $tmpdir = tmpdir;
my $fifo_new = sub {
	state $nr = 0;
	my $fifo = "$tmpdir/fifo.$nr";
	++$nr;
	POSIX::mkfifo($fifo, 0777) // xbail "mkfifo: $!";
	$fifo;
};
my $unix_dest = "$tmpdir/u.sock";
my ($back_out, $back_err) = ("$tmpdir/back.out", "$tmpdir/back.err");
my ($front_out, $front_err) = ("$tmpdir/front.out", "$tmpdir/front.err");
my $back_unix = IO::Socket::UNIX->new(Local => $unix_dest,
		Listen => 4096, Type => Socket::SOCK_STREAM) or
	xbail "bind+listen $unix_dest: $!";
my $back_tcp = tcp_server();
my $back_tcp_host_port = tcp_host_port($back_tcp);
$back_unix->blocking(0);
my $front_tcp = tcp_server();
my $front_url = 'http://'.tcp_host_port($front_tcp);
my $front_psgi = "$tmpdir/u.psgi";
write_file '>>', $front_psgi, <<EOM;
use PublicInbox::PsgiRproxy;
use Plack::Builder;
builder {
	mount 'http://nobuffer.example/' =>
		PublicInbox::PsgiRproxy->new(
			"http://$back_tcp_host_port",
			proxy_buffering => 0)->to_app;
	mount '/' => PublicInbox::PsgiRproxy->new("unix:$unix_dest")->to_app;
};
EOM
my $back_cmd = [ '-httpd', '-W0',
	"--stdout=$back_out", "--stderr=$back_err", $psgi ];
my $back_td = start_script($back_cmd, {}, { 3 => $back_unix, 4 => $back_tcp });

my $front_cmd = [ '-httpd', '-W0',
	"--stdout=$front_out", "--stderr=$front_err", $front_psgi];
my $front_td = start_script($front_cmd, {}, { 3 => $front_tcp });

for my $opt (map { (['-0', @$_], $_) } (['-HHost:nobuffer.example'], [])) {
	my $fifo = $fifo_new->();
	my $cmd = [ @curl, @$opt, "-HX-Check-Fifo:$fifo",
		qw(-NsSf), "$front_url/slow-header" ];
	my $rd = popen_rd $cmd;
	open my $f, '>', $fifo;
	$f->autoflush(1);
	print $f "hello\n" or xbail "print $fifo: $!";
	close $f;
	my $buf = do { local $/; <$rd>; };
	$rd->close or xbail "curl failed: $?";
	is $buf, "hello\n", "got expected response w/ (@$opt)";

	$fifo = $fifo_new->();
	$cmd = [ @curl, "-HX-Check-Fifo:$fifo", @$opt,
		qw(-NsSf), "$front_url/slow-body" ];
	$rd = popen_rd $cmd;
	open $f, '>', $fifo;
	$f->autoflush(1);
	for my $c ('a'..'c') {
		$c .= "\n";
		print $f $c or xbail "print to FIFO: $!";
		$buf = <$rd>;
		is $buf, $c, "got trickle for reading (@$opt)";
	}
	print $f "world\n" or xbail "print final line to FIFO: $!";
	close $f;
	$buf = <$rd>;
	is $buf, "world\n", "read expected body from curl (@$opt)";
	$rd->close or xbail "curl failed: $? (@$opt)";
}

{
	open my $fh, '<', 'COPYING';
	my $csum = '78e50e186b04c8fe1defaa098f1c192181b3d837';
	for my $exp (map { (['-HExpect:', @$_], $_) } (
			['-HHost:nobuffer.example'], [])) {
		my $cmd = [ @curl, @$exp, qw(--tcp-nodelay -NsSf),
			"$front_url/sha1", '-T-' ];
		pipe(my $r, my $w);
		my $rd = popen_rd $cmd, undef, { 0 => $r };
		close $r;
		$w->autoflush(1);
		my $n;
		do {
			$n = read($fh, my $buf, 8192) //
				xbail "read(COPYING): $!";
			if ($n) {
				print $w $buf or xbail "print: $!";
			}
		} while ($n);
		close $w;
		my $sha = do { local $/; <$rd> };
		is $sha, $csum, "largish chunked upload accepted (@$exp)";
		seek $fh, 0, 0;
		pop(@$cmd) eq '-T-' or xbail "BUG `-T-' not popped";
		push @$cmd, '-T', 'COPYING';
		$sha = xqx $cmd;
		is $sha, $csum, "largish identity upload accepted (@$exp)";
	}
}

# HTTP/1.1-only
for my $host (['-HHost:nobuffer.example'], []) {
	my $cmd = [ @curl, @$host, qw(-NsSf), "$front_url/getline-die" ];
	xsys $cmd, undef, { 2 => \(my $cerr = '') };
	is($? >> 8, 18, "curl @$host saw partial response on getline-die") or
		diag $cerr;

	$cmd = [ @curl, @$host, qw(-NsSf), "$front_url/close-die" ];
	xsys $cmd, undef, { 2 => \($cerr = '') };
	is($? >> 8, 18, "curl @$host saw partial response on close-die") or
		diag $cerr;

	$cmd = [ @curl, @$host, qw(-NsSf), "$front_url/callback-truncated" ];
	xsys $cmd, undef, { 1 => \(my $cout = ''), 2 => \($cerr = '') };
	is($? >> 8, 18,
		"curl @$host saw partial response on truncated response") or
		diag $cerr;
}
{
	my $c = tcp_connect $front_tcp;
	print $c "GET /server HTTP/1.0\r\n\r\n";
	my $buf = do { local $/; <$c> };
	unlike $buf, qr/^server:/smi, 'Server: tag filtered out by default';
	like $buf, qr!^trying to advertise!sm,
		'actually made correct request to test with';
}

done_testing;
