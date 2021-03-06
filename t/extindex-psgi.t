#!perl -w
# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use v5.10.1;
use PublicInbox::TestCommon;
use PublicInbox::Config;
use File::Copy qw(cp);
use IO::Handle ();
require_git(2.6);
require_mods(qw(json DBD::SQLite Search::Xapian
		HTTP::Request::Common Plack::Test URI::Escape Plack::Builder));
use_ok($_) for (qw(HTTP::Request::Common Plack::Test));
use IO::Uncompress::Gunzip qw(gunzip);
require PublicInbox::WWW;
my ($ro_home, $cfg_path) = setup_public_inboxes;
my ($tmpdir, $for_destroy) = tmpdir;
my $home = "$tmpdir/home";
mkdir $home or BAIL_OUT $!;
mkdir "$home/.public-inbox" or BAIL_OUT $!;
my $pi_config = "$home/.public-inbox/config";
cp($cfg_path, $pi_config) or BAIL_OUT;
my $env = { HOME => $home };
run_script([qw(-extindex --all), "$tmpdir/eidx"], $env) or BAIL_OUT;
{
	open my $cfgfh, '>>', $pi_config or BAIL_OUT;
	$cfgfh->autoflush(1);
	print $cfgfh <<EOM or BAIL_OUT;
[extindex "all"]
	topdir = $tmpdir/eidx
	url = http://bogus.example.com/all
[publicinbox]
	wwwlisting = all
	grokManifest = all
EOM
}
my $www = PublicInbox::WWW->new(PublicInbox::Config->new($pi_config));
my $client = sub {
	my ($cb) = @_;
	my $res = $cb->(GET('/all/'));
	is($res->code, 200, '/all/ good');
	$res = $cb->(GET('/all/new.atom', Host => 'usethis.example.com'));
	like($res->content, qr!http://usethis\.example\.com/!s,
		'Host: header respected in Atom feed');
	unlike($res->content, qr!http://bogus\.example\.com/!s,
		'default URL ignored with different host header');

	$res = $cb->(GET('/all/_/text/config/'));
	is($res->code, 200, '/text/config HTML');
	$res = $cb->(GET('/all/_/text/config/raw'));
	is($res->code, 200, '/text/config raw');
	my $f = "$tmpdir/extindex.config";
	open my $fh, '>', $f or xbail $!;
	print $fh $res->content or xbail $!;
	close $fh or xbail $!;
	my $cfg = PublicInbox::Config->git_config_dump($f);
	is($?, 0, 'no errors from git-config parsing');
	ok($cfg->{'extindex.all.topdir'}, 'extindex.topdir defined');

	$res = $cb->(GET('/all/all.mbox.gz'));
	is($res->code, 200, 'all.mbox.gz');

	$res = $cb->(GET('/'));
	like($res->content, qr!\Qhttp://bogus.example.com/all\E!,
		'/all listed');
	$res = $cb->(GET('/?q='));
	is($res->code, 200, 'no query means all inboxes');
	$res = $cb->(GET('/?q=nonexistent'));
	is($res->code, 404, 'no inboxes matched');
	unlike($res->content, qr!no inboxes, yet!,
		'we have inboxes, just no matches');

	my $m = {};
	for my $pfx (qw(/t1 /t2), '') {
		$res = $cb->(GET($pfx.'/manifest.js.gz'));
		gunzip(\($res->content) => \(my $js));
		$m->{$pfx} = json_utf8->decode($js);
	}
	is_deeply([sort keys %{$m->{''}}],
		[ sort(keys %{$m->{'/t1'}}, keys %{$m->{'/t2'}}) ],
		't1 + t2 = all');
	is_deeply([ sort keys %{$m->{'/t2'}} ], [ '/t2/git/0.git' ],
		't2 manifest');
	is_deeply([ sort keys %{$m->{'/t1'}} ], [ '/t1' ],
		't2 manifest');
};
test_psgi(sub { $www->call(@_) }, $client);
%$env = (%$env, TMPDIR => $tmpdir, PI_CONFIG => $pi_config);
test_httpd($env, $client);

done_testing;
