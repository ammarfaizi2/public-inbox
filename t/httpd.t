# Copyright (C) 2016-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;

foreach my $mod (qw(Plack::Util Plack::Builder HTTP::Date HTTP::Status)) {
	eval "require $mod";
	plan skip_all => "$mod missing for httpd.t" if $@;
}
use File::Temp qw/tempdir/;
use Socket qw(IPPROTO_TCP SOL_SOCKET);
require './t/common.perl';

# FIXME: too much setup
my $tmpdir = tempdir('pi-httpd-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $home = "$tmpdir/pi-home";
my $err = "$tmpdir/stderr.log";
my $out = "$tmpdir/stdout.log";
my $maindir = "$tmpdir/main.git";
my $group = 'test-httpd';
my $addr = $group . '@example.com';
my $cfgpfx = "publicinbox.$group";
my $httpd = 'blib/script/public-inbox-httpd';
my $init = 'blib/script/public-inbox-init';
my $sock = tcp_server();
my $pid;
use_ok 'PublicInbox::Git';
use_ok 'PublicInbox::Import';
use_ok 'Email::MIME';
END { kill 'TERM', $pid if defined $pid };
{
	local $ENV{HOME} = $home;
	ok(!system($init, $group, $maindir, 'http://example.com/', $addr),
		'init ran properly');

	# ensure successful message delivery
	{
		my $mime = Email::MIME->new(<<EOF);
From: Me <me\@example.com>
To: You <you\@example.com>
Cc: $addr
Message-Id: <nntp\@example.com>
Subject: hihi
Date: Thu, 01 Jan 1970 06:06:06 +0000

nntp
EOF

		my $git = PublicInbox::Git->new($maindir);
		my $im = PublicInbox::Import->new($git, 'test', $addr);
		$im->add($mime);
		$im->done($mime);
	}
	ok($sock, 'sock created');
	my $cmd = [ $httpd, '-W0', "--stdout=$out", "--stderr=$err" ];
	$pid = spawn_listener(undef, $cmd, [$sock]);
	my $host = $sock->sockhost;
	my $port = $sock->sockport;
	my $conn = tcp_connect($sock);
	ok($conn, 'connected');
	ok($conn->write("GET / HTTP/1.0\r\n\r\n"), 'wrote data to socket');
	{
		my $buf;
		ok($conn->read($buf, 4096), 'read some bytes');
		like($buf, qr!\AHTTP/1\.[01] 404\b!, 'got 404 response');
		is($conn->read($buf, 1), 0, "EOF");
	}

	is(system(qw(git clone -q --mirror),
			"http://$host:$port/$group", "$tmpdir/clone.git"),
		0, 'smart clone successful');

	# ensure dumb cloning works, too:
	is(system('git', "--git-dir=$maindir",
		qw(config http.uploadpack false)),
		0, 'disable http.uploadpack');
	is(system(qw(git clone -q --mirror),
			"http://$host:$port/$group", "$tmpdir/dumb.git"),
		0, 'clone successful');

	ok(kill('TERM', $pid), 'killed httpd');
	$pid = undef;
	waitpid(-1, 0);

	is(system('git', "--git-dir=$tmpdir/clone.git",
		  qw(fsck --no-verbose)), 0,
		'fsck on cloned directory successful');
}

SKIP: {
	skip 'TCP_DEFER_ACCEPT is Linux-only', 1 if $^O ne 'linux';
	my $var = Socket::TCP_DEFER_ACCEPT();
	defined(my $x = getsockopt($sock, IPPROTO_TCP, $var)) or die;
	ok(unpack('i', $x) > 0, 'TCP_DEFER_ACCEPT set');
};
SKIP: {
	skip 'SO_ACCEPTFILTER is FreeBSD-only', 1 if $^O ne 'freebsd';
	if (system('kldstat -m accf_http >/dev/null') != 0) {
		skip 'accf_http not loaded: kldload accf_http', 1;
	}
	require PublicInbox::Daemon;
	my $var = PublicInbox::Daemon::SO_ACCEPTFILTER();
	my $x = getsockopt($sock, SOL_SOCKET, $var);
	like($x, qr/\Ahttpready\0+\z/, 'got httpready accf for HTTP');
};

done_testing();

1;
