# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::Eml;
use PublicInbox::TestCommon;
use IO::Uncompress::Gunzip qw(gunzip);
my ($tmpdir, $for_destroy) = tmpdir();
my $maindir = "$tmpdir/main.git";
my $addr = 'test-public@example.com';
my $cfgpfx = "publicinbox.test";
my @mods = qw(HTTP::Request::Common Plack::Test URI::Escape Plack::Builder);
use_ok $_ foreach @mods;
use PublicInbox::Import;
use_ok 'PublicInbox::WWW';
use_ok 'PublicInbox::WwwText';
my $config = cfg_new $tmpdir, <<EOF;
[publicinbox "test"]
	address = $addr
	inboxdir = $maindir
EOF
PublicInbox::Import::init_bare($maindir);
my $www = PublicInbox::WWW->new($config);

test_psgi(sub { $www->call(@_) }, sub {
	my ($cb) = @_;
	my $gunzipped;
	my $req = GET('/test/_/text/help/');
	my $res = $cb->($req);
	my $content = $res->content;
	like($content, qr!<title>public-inbox help.*</title>!, 'default help');
	$req->header('Accept-Encoding' => 'gzip');
	$res = $cb->($req);
	is($res->header('Content-Encoding'), 'gzip', 'got gzip encoding');
	is($res->header('Content-Type'), 'text/html; charset=UTF-8',
		'got gzipped HTML');
	gunzip(\($res->content) => \$gunzipped);
	is($gunzipped, $content, 'gzipped content is correct');

	$req = GET('/test/_/text/help/raw');
	$res = $cb->($req);
	like $res->header('Content-Type'), qr!\Atext/plain\b!,
		'got text/plain Content-Type';
	$content = $res->content;
	like $content, qr!public-inbox help!, 'default help';

	$req->header('Accept-Encoding' => 'gzip');
	$res = $cb->($req);
	is($res->header('Content-Encoding'), 'gzip', 'got gzip encoding');
	like $res->header('Content-Type'), qr!\Atext/plain\b!,
		'got text/plain Content-Type w/ gzip';
	gunzip(\($res->content) => \$gunzipped);
	is $gunzipped, $content, 'gzipped content is correct';

	$req = GET('/test/_/text/config/raw');
	$res = $cb->($req);
	$content = $res->content;
	my $olen = $res->header('Content-Length');
	my $cfg = cfg_new $tmpdir, $content;
	is($cfg->{"$cfgpfx.address"}, $addr, 'got expected address in config');

	$req->header('Accept-Encoding' => 'gzip');
	$res = $cb->($req);
	is($res->header('Content-Encoding'), 'gzip', 'got gzip encoding');
	ok($res->header('Content-Length') < $olen, 'gzipped help is smaller');
	gunzip(\($res->content) => \$gunzipped);
	is($gunzipped, $content);
});

done_testing();
