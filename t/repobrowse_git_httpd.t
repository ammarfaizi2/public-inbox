# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Integration test for public-inbox-httpd and (git) repobrowse
# since we may use some special APIs not available in other servers
use strict;
use warnings;
use Test::More;
foreach my $mod (qw(Danga::Socket HTTP::Parser::XS HTTP::Date HTTP::Status
		Plack::Test::ExternalServer)) {
	eval "require $mod";
	plan skip_all => "$mod missing for repobrowse_git_httpd.t" if $@;
}
my $test = require './t/repobrowse_common_git.perl';
{
	no warnings 'once';
	$Plack::Test::Impl = 'ExternalServer';
}
use File::Temp qw/tempdir/;
use Cwd qw/getcwd/;
use IO::Socket;
use Fcntl qw(F_SETFD);
use POSIX qw(dup2);
my $tmpdir = tempdir('repobrowse_git_httpd-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $err = "$tmpdir/stderr.log";
my $out = "$tmpdir/stdout.log";
my $httpd = 'blib/script/public-inbox-httpd';
my $psgi = getcwd() . '/' . $test->{psgi};
my %opts = (
	LocalAddr => '127.0.0.1',
	ReuseAddr => 1,
	Proto => 'tcp',
	Type => SOCK_STREAM,
	Listen => 1024,
);
my $sock = IO::Socket::INET->new(%opts);
my $host = $sock->sockhost;
my $port = $sock->sockport;
my $uri = "http://$host:$port/";
my $pid;
END { kill 'TERM', $pid if defined $pid };
my $spawn_httpd = sub {
	$pid = fork;
	if ($pid == 0) {
		# pretend to be systemd:
		dup2(fileno($sock), 3) or die "dup2 failed: $!\n";
		my $t = IO::Handle->new_from_fd(3, 'r');
		$t->fcntl(F_SETFD, 0);
		$ENV{REPOBROWSE_CONFIG} = $test->{repobrowse_config};
		$ENV{LISTEN_PID} = $$;
		$ENV{LISTEN_FDS} = 1;
		exec $httpd, '-W0', $psgi;
		# exec $httpd, '-W0', "--stdout=$out", "--stderr=$err", $psgi;
		die "FAIL: $!\n";
	}
	ok(defined $pid, 'forked httpd process successfully');
};

$spawn_httpd->();

{ # git clone tests
	my $url = $uri . 'test.git';
	is(system(qw(git clone -q --mirror), $url, "$tmpdir/smart.git"),
		0, 'smart clone successful');
	is(system('git', "--git-dir=$tmpdir/smart.git", 'fsck'), 0, 'fsck OK');
	is(system('git', "--git-dir=$test->{git_dir}",
		qw(config http.uploadpack 0)), 0, 'disabled smart HTTP');
	is(system('git', "--git-dir=$test->{git_dir}",
		qw(update-server-info)), 0, 'enable dumb HTTP');
	is(system(qw(git clone -q --mirror), $url, "$tmpdir/dumb.git"),
		0, 'dumb clone successful');
	is(system('git', "--git-dir=$tmpdir/dumb.git", 'fsck'),
		0, 'fsck dumb OK');
}

test_psgi(uri => $uri, client => sub {
	my ($cb) = @_;
	my $res = $cb->(GET($uri . 'test.git/info/refs'));
	is(200, $res->code, 'got info/refs');

	$res = $cb->(GET($uri . 'best.git/info/refs'));
	is(404, $res->code, 'bad request fails');

	$res = $cb->(GET($uri . 'test.git/patch'));
	is(200, $res->code, 'got patch');
	is('text/plain; charset=UTF-8', $res->header('Content-Type'),
		'got proper content-type with patch');

	# ignore signature from git-format-patch:
	my ($patch, undef) = split(/\n-- \n/s, $res->content);
	my ($exp, undef) = split(/\n-- \n/s,
		`cd "$test->{git_dir}" && \
		 git format-patch -1 -M --stdout HEAD`);
	is($patch, $exp, 'patch content matches expected');
});

{
	# allow reading description file
	my %conn = ( PeerAddr => $host, PeerPort => $port, Proto => 'tcp',
		Type => SOCK_STREAM);
	my $conn = IO::Socket::INET->new(%conn);
	ok($conn, "connected for description check");
	$conn->write("GET /test.git/description HTTP/1.0\r\n\r\n");
	ok($conn->read(my $buf, 8192), 'read response');
	my ($head, $body) = split(/\r\n\r\n/, $buf, 2);
	like($head, qr!\AHTTP/1\.0 200 !s, 'got 200 response for description');

	$conn = IO::Socket::INET->new(%conn);
	ok($conn, "connected for range check");
	$conn->write("GET /test.git/description HTTP/1.0\r\n" .
			"Range: bytes=5-\r\n\r\n");
	ok($conn->read($buf, 8192), 'read partial response');
	my ($h2, $b2) = split(/\r\n\r\n/, $buf, 2);
	like($h2, qr!\AHTTP/1\.0 206 !s, 'got 206 response for range');
	is($b2, substr($body, 5), 'substring matches on 206');
}

test_psgi(uri => $uri, client => sub {
	my ($cb) = @_;
	my $res = $cb->(GET($uri . 'test.git/snapshot/test-master.tar.gz'));
	is(200, $res->code, 'got gzipped tarball');
	my $got = "$tmpdir/got.tar.gz";
	my $exp = "$tmpdir/exp.tar.gz";
	open my $fh, '>', $got or die "open got.tar.gz: $!";
	print $fh $res->content;
	close $fh or die "close failed: $!";
	$res = undef;
	my $rc = system('git', "--git-dir=$test->{git_dir}",
			qw(archive --prefix=test-master/ --format=tar.gz),
			'-o', $exp, 'master');
	is(0, $rc, 'git-archive generated check correctly');
	is(0, system('cmp', $got, $exp), 'got expected gzipped tarball');

});

done_testing();
1;
