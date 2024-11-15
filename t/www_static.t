#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use autodie;
use PublicInbox::TestCommon;
use PublicInbox::IO qw(write_file);
my $tmpdir = tmpdir();
require_mods qw(psgi);
require IO::Uncompress::Gunzip;
use File::Path qw(remove_tree);
use_ok 'PublicInbox::WwwStatic';

my $psgi_env = sub { # @_ is passed to WwwStatic->new
	my $ret = "$tmpdir/www_static.psgi";
	write_file '>', $ret, <<EOM;
use v5.12;
use Plack::Builder;
use PublicInbox::WwwStatic;
my \$ws = PublicInbox::WwwStatic->new(docroot => "$tmpdir" @_);
builder { sub { \$ws->call(shift) } }
EOM
	{ psgi_file => $ret, TMPDIR => "$tmpdir" };
};

my $client = sub {
	my $cb = shift;
	unlink "$tmpdir/index.html" if -f "$tmpdir/index.html";
	my $res = $cb->(GET('/'));
	is($res->code, 404, '404 on "/" by default');
	write_file '>', "$tmpdir/index.html", 'hi';
	$res = $cb->(GET('/'));
	is($res->code, 200, '200 with index.html');
	is($res->content, 'hi', 'default index.html returned');
	$res = $cb->(HEAD('/'));
	is($res->code, 200, '200 on HEAD /');
	is($res->content, '', 'no content');
	is($res->header('Content-Length'), '2', 'content-length set');
	like($res->header('Content-Type'), qr!^text/html\b!,
		'content-type is html');
};

my $env = $psgi_env->();
test_psgi(Plack::Util::load_psgi($env->{psgi_file}), $client);
test_httpd $env, $client;

$client = sub {
	my $cb = shift;
	write_file '>', "$tmpdir/index.html", 'hi';
	my $res = $cb->(GET('/'));
	my $updir = 'href="../">../</a>';
	is($res->code, 200, '200 with autoindex default');
	my $ls = $res->content;
	like($ls, qr/index\.html/, 'got listing with index.html');
	ok(index($ls, $updir) < 0, 'no updir at /');
	mkdir "$tmpdir/dir";
	rename "$tmpdir/index.html", "$tmpdir/dir/index.html";

	$res = $cb->(GET('/dir/'));
	is($res->code, 200, '200 with autoindex for dir/');
	$ls = $res->content;
	ok(index($ls, $updir) > 0, 'updir at /dir/');

	for my $up (qw(/../ .. /dir/.. /dir/../)) {
		is($cb->(GET($up))->code, 403, "`$up' traversal rejected");
	}

	$res = $cb->(GET('/dir'));
	is($res->code, 302, '302 w/o slash');
	like($res->header('Location'), qr!://[^/]+/dir/\z!,
		'redirected w/ slash');

	rename "$tmpdir/dir/index.html", "$tmpdir/dir/foo";
	link "$tmpdir/dir/foo", "$tmpdir/dir/foo.gz";
	$res = $cb->(GET('/dir/'));
	unlike($res->content, qr/>foo\.gz</,
		'.gz file hidden if mtime matches uncompressed');
	like($res->content, qr/>foo</, 'uncompressed foo shown');

	$res = $cb->(GET('/dir/foo/bar'));
	is($res->code, 404, 'using file as dir fails');

	unlink "$tmpdir/dir/foo";
	$res = $cb->(GET('/dir/'));
	like($res->content, qr/>foo\.gz</,
		'.gz shown when no uncompressed version exists');

	write_file '>', "$tmpdir/dir/foo", "uncompressed\n";
	utime 0, 0, "$tmpdir/dir/foo";
	$res = $cb->(GET('/dir/'));
	my $html = $res->content;
	like($html, qr/>foo</, 'uncompressed foo shown');
	like($html, qr/>foo\.gz</, 'gzipped foo shown on mtime mismatch');

	$res = $cb->(GET('/dir/foo'));
	is($res->content, "uncompressed\n",
		'got uncompressed on mtime mismatch');

	utime 0, 0, "$tmpdir/dir/foo.gz";
	my $get = GET('/dir/foo');
	$get->header('Accept-Encoding' => 'gzip');
	$res = $cb->($get);
	is($res->content, "hi", 'got compressed on mtime match');

	$get = GET('/dir/');
	$get->header('Accept-Encoding' => 'gzip');
	$res = $cb->($get);
	my $in = $res->content;
	my $out = '';
	IO::Uncompress::Gunzip::gunzip(\$in => \$out);
	like($out, qr/\A<html>/, 'got HTML start after gunzip');
	like($out, qr{</html>$}, 'got HTML end after gunzip');
	unlink "$tmpdir/dir/foo.gz";
	$get = GET('/dir/foo');

	require HTTP::Date;
	HTTP::Date->import('time2str');
	$get->header('If-Modified-Since' => time2str(0));
	$res = $cb->($get);
	is $res->code, 304, '304 on If-Modified-Since hit';
	$get->header('If-Modified-Since' => time2str(1));
	$res = $cb->($get);
	is $res->code, 200, '200 on If-Modified-Since miss';
SKIP: {
	# validating with curl ensures we didn't carry the same
	# misunderstandings across both the code being tested
	# and the test itself.
	$ENV{TEST_EXPENSIVE} or
		skip 'TEST_EXPENSIVE unset for validation w/curl', 1;
	my $uri = $ENV{PLACK_TEST_EXTERNALSERVER_URI} or
		skip 'curl test skipped w/o external server', 1;
	my $dst = "$tmpdir/foo.dst";
	my $dst2 = "$dst.2";
	$uri .= '/dir/foo';
	state $curl = require_cmd 'curl', 1;
	xsys_e $curl, '-gsSfR', '-o', $dst, $uri;
	xsys_e [ $curl, '-vgsSfR', '-o', $dst2, $uri, '-z', $dst ],
		undef, { 2 => \(my $curl_err) };
	like $curl_err, qr! HTTP/1\.[012] 304 !sm,
		'got 304 response w/ If-Modified-Since';
	is -s $dst2, undef, 'nothing downloaded on 304';
	utime 1, 1, "$tmpdir/dir/foo";
	xsys_e [ $curl, '-vgsSfR', '-o', $dst2, $uri, '-z', $dst ],
		undef, { 2 => \$curl_err };
	is -s $dst2, -s $dst, 'got 200 on If-Modified-Since mismatch';
	like $curl_err, qr! HTTP/1\.[012] 200 !sm,
		'got 200 response w/ If-Modified-Since';
} # SKIP
	remove_tree "$tmpdir/dir";
};

$env = $psgi_env->(', autoindex => 1, index => []');
test_psgi(Plack::Util::load_psgi($env->{psgi_file}), $client);
test_httpd $env, $client;

done_testing();
