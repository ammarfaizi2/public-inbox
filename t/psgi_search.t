# Copyright (C) 2017-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use Email::MIME;
use PublicInbox::Config;
use PublicInbox::Inbox;
use PublicInbox::InboxWritable;
use PublicInbox::WWW;
use bytes (); # only for bytes::length
use PublicInbox::TestCommon;
my @mods = qw(DBD::SQLite Search::Xapian HTTP::Request::Common Plack::Test
		URI::Escape Plack::Builder);
foreach my $mod (@mods) {
	eval "require $mod";
	plan skip_all => "$mod missing for psgi_search.t" if $@;
}

use_ok $_ foreach (@mods, qw(PublicInbox::SearchIdx));
my ($tmpdir, $for_destroy) = tmpdir();

my $ibx = PublicInbox::Inbox->new({
	inboxdir => $tmpdir,
	address => 'git@vger.kernel.org',
	name => 'test',
});
$ibx = PublicInbox::InboxWritable->new($ibx);
$ibx->init_inbox(1);
my $im = $ibx->importer(0);
my $digits = '10010260936330';
my $ua = 'Pine.LNX.4.10';
my $mid = "$ua.$digits.2460-100000\@penguin.transmeta.com";
my $data = <<"EOF";
Subject: test
Message-ID: <$mid>
From: Ævar Arnfjörð Bjarmason <avarab\@example>
To: git\@vger.kernel.org

EOF

my $mime = Email::MIME->new(\$data);
$im->add($mime);
$im->done;
PublicInbox::SearchIdx->new($ibx, 1)->index_sync;

my $cfgpfx = "publicinbox.test";
my $config = PublicInbox::Config->new(\<<EOF);
$cfgpfx.address=git\@vger.kernel.org
$cfgpfx.inboxdir=$tmpdir
EOF
my $www = PublicInbox::WWW->new($config);
test_psgi(sub { $www->call(@_) }, sub {
	my ($cb) = @_;
	my $res;
	$res = $cb->(GET('/test/?q=%C3%86var'));
	my $html = $res->content;
	like($html, qr/<title>&#198;var - /, 'HTML escaped in title');
	my @res = ($html =~ m/\?q=(.+var)\b/g);
	ok(scalar(@res), 'saw query strings');
	my %uniq = map { $_ => 1 } @res;
	is(1, scalar keys %uniq, 'all query values identical in HTML');
	is('%C3%86var', (keys %uniq)[0], 'matches original query');
	ok(index($html, 'by &#198;var Arnfj&#246;r&#240; Bjarmason') >= 0,
		"displayed Ævar's name properly in HTML");

	my $warn = [];
	local $SIG{__WARN__} = sub { push @$warn, @_ };
	$res = $cb->(GET('/test/?q=s:test&l=5e'));
	is($res->code, 200, 'successful search result');
	is_deeply([], $warn, 'no warnings from non-numeric comparison');

	$res = $cb->(POST('/test/?q=s:bogus&x=m'));
	is($res->code, 404, 'failed search result gives 404');
	is_deeply([], $warn, 'no warnings');

	my $mid_re = qr/\Q$mid\E/o;
	while (length($digits) > 8) {
		$res = $cb->(GET("/test/$ua.$digits/"));
		is($res->code, 300, 'partial match found while truncated');
		like($res->content, qr/\b1 partial match found\b/);
		like($res->content, $mid_re, 'found mid in response');
		chop($digits);
	}
});

done_testing();

1;
