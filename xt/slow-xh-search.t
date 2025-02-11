#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use autodie;
use PublicInbox::TestCommon;
use Socket ();
ref($find_xh_pid) or plan skip_all => $find_xh_pid;
require_mods qw(-httpd v2 psgi Xapian);
my ($ro_home, $cfg_path) = setup_public_inboxes;
my $tmpdir = tmpdir;
my $unix = "$tmpdir/unix.sock";
my $out = "$tmpdir/out.log";
my $err = "$tmpdir/err.log";
my $cmd = [ qw(-httpd -W0 -X0), "--stdout=$out", "--stderr=$err" ];
my $env = { PI_CONFIG => $cfg_path };
my $srv = IO::Socket::UNIX->new(Listen => 4096, Local => $unix,
			Type => Socket::SOCK_STREAM);
my $rdr = { 3 => $srv };
$srv->blocking(0);
my $td = start_script($cmd, $env, $rdr);
chomp(my $nfd = `/bin/sh -c 'ulimit -n'`);
diag "RLIMIT_NOFILE=$nfd, consider increasing if test fails";
fail "RLIMIT_NOFILE=$nfd too small" if $nfd < 11;
$nfd -= 10;

my $req = "GET /t2/?q=m:testmessage\@example.com HTTP/1.0\r\n\r\n";
my @opt = (Peer => $unix, Type => Socket::SOCK_STREAM);
my $start_req = sub {
	my $c = IO::Socket::UNIX->new(@opt) or xbail "connect: $!";
	$c->autoflush(1);
	print $c $req;
	$c;
};

# make a search request/response to ensure xap_helper is started
read $start_req->(), my $buf, 16384;

my $xh_pid = $find_xh_pid->($td->{pid}) or
	xbail "can't find XH pid";
kill 'STOP', $xh_pid;
diag "starting $nfd requests...";
my @c = map { $start_req->() } (1..$nfd);
diag "all $nfd requested";
tick 1;
kill 'CONT', $xh_pid;
my %codes = (200 => 0, 503 => 0);
diag "reading $nfd responses";
for my $c (@c) {
	read $c, $buf, 16384;
	undef $c;
	if ($buf =~ m!\AHTTP/1\.[01] (\d+)!) {
		$codes{$1}++;
	} else {
		diag explain($buf);
		fail 'bad response';
	}
}
ok $codes{200}, 'got some 200 responses' or diag explain(\%codes);
ok $codes{503}, 'got some 503 errors' or diag explain(\%codes);
is $codes{200} + $codes{503}, $nfd,
	'only got 503 and 200 codes' or diag explain(\%codes);

$td->join('TERM');
done_testing;
