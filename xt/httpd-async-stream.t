#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Expensive test to validate compression and TLS.
use v5.12;
use autodie;
use PublicInbox::IO qw(write_file);
use IO::Uncompress::Gunzip qw(gunzip $GunzipError);
use PublicInbox::TestCommon;
use PublicInbox::DS qw(now);
use PublicInbox::Spawn qw(popen_rd);
use Digest::MD5;
use POSIX qw(_exit);
my $inboxdir = $ENV{GIANT_INBOX_DIR};
plan skip_all => "GIANT_INBOX_DIR not defined for $0" unless $inboxdir;
my $curl = require_cmd('curl');
my ($tmpdir, $for_destroy) = tmpdir();
require_mods(qw(DBD::SQLite));
my $JOBS = $ENV{TEST_JOBS} // 4;
my $endpoint = $ENV{TEST_ENDPOINT} // 'all.mbox.gz';
my $curl_opt = $ENV{TEST_CURL_OPT} // '';
diag "TEST_JOBS=$JOBS TEST_ENDPOINT=$endpoint TEST_CURL_OPT=$curl_opt";

# we set Host: to ensure stable results across test runs
my @CURL_OPT = (qw(-HHost:example.com -sSf), split(' ', $curl_opt));

my $make_local_server = sub {
	my ($http) = @_;
	my $pi_config = "$tmpdir/config";
	write_file '>', $pi_config, <<"";
[publicinbox "test"]
inboxdir = $inboxdir
address = test\@example.com

	my ($out, $err) = ("$tmpdir/out", "$tmpdir/err");
	for ($out, $err) { open my $fh, '>', $_ }

	# not using multiple workers, here, since we want to increase
	# the chance of tripping concurrency bugs within PublicInbox/HTTP*.pm
	my $cmd = [ '-httpd', "--stdout=$out", "--stderr=$err", '-W0' ];
	my $host_port = tcp_host_port($http);
	push @$cmd, "-lhttp://$host_port";
	my $url = "$host_port/test/$endpoint";
	print STDERR "# CMD ". join(' ', @$cmd). "\n";
	my $env = { PI_CONFIG => $pi_config };
	(start_script($cmd, $env, { 3 => $http }), $url)
};

my ($td, $url) = $make_local_server->(my $http = tcp_server());

my $s1 = tcp_connect($http);
my $rbuf = do { # pipeline while reading long response
	my $req = <<EOM;
GET /test/$endpoint HTTP/1.1\r
Host: example.com\r
\r
EOM
	is syswrite($s1, $req), length($req), 'initial long req';
	<$s1>;
};
like $rbuf, qr!\AHTTP/1\.1 200\b!, 'started reading 200 response';

my $do_get_all = sub {
	my ($job) = @_;
	local $SIG{__DIE__} = sub { print STDERR $job, ': ', @_; _exit(1) };
	my $dig = Digest::MD5->new;
	my ($buf, $nr);
	my $bytes = 0;
	my $t0 = now();
	my $rd = popen_rd([$curl, @CURL_OPT, $url]);
	while (1) {
		$nr = sysread($rd, $buf, 65536);
		last if !$nr;
		$dig->add($buf);
		$bytes += $nr;
	}
	my $res = $dig->hexdigest;
	my $elapsed = sprintf('%0.3f', now() - $t0);
	$rd->close or xbail "close curl failed: $! \$?=$?\n";
	print STDERR "# $job $$ ($?) $res (${elapsed}s) $bytes bytes\n";
	$res;
};

my (%pids, %res);
for my $job (1..$JOBS) {
	pipe(my $r, my $w);
	my $pid = fork;
	if ($pid == 0) {
		close $r;
		my $res = $do_get_all->($job);
		print $w $res;
		close $w;
		_exit(0);
	}
	close $w;
	$pids{$pid} = [ $job, $r ];
}

while (scalar keys %pids) {
	my $pid = waitpid(-1, 0) or next;
	my $child = delete $pids{$pid} or next;
	my ($job, $rpipe) = @$child;
	is($?, 0, "$job done");
	my $sum = do { local $/; <$rpipe> };
	push @{$res{$sum}}, $job;
}
is(scalar keys %res, 1, 'all got the same result');
{
	my $req = <<EOM;
GET /test/manifest.js.gz HTTP/1.1\r
Host: example.com\r
Connection: close\r
\r
EOM
	is syswrite($s1, $req), length($req),
		'pipeline another request while reading long response';
	diag 'reading remainder of slow response';
	my $res = do { local $/ = "\r\n\r\n"; <$s1> };
	like $res, qr/^Transfer-Encoding: chunked\r\n/sm, 'chunked response';
	{
		local $/ = "\r\n"; # get to final chunk
		while (defined(my $l = <$s1>)) { last if $l eq "0\r\n" }
	};
	is scalar(readline($s1)), "\r\n", 'got final CRLF from 1st response';
	diag "second response:";
	$res = do { local $/ = "\r\n\r\n"; <$s1> };
	like $res, qr!\AHTTP/1\.1 200 !, 'response for pipelined req';
	gunzip($s1 => \my $json) or xbail "gunzip $GunzipError";
	my $m = PublicInbox::Config::json()->decode($json);
	like $m->{'/test'}->{fingerprint}, qr/\A[0-9a-f]{40,}\z/,
		'acceptable fingerprint in response';
}
$td->kill;
$td->join;
is($?, 0, 'no error on -httpd exit');
done_testing;
