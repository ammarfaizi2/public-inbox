#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use PublicInbox::Eml;
my @req = qw(URI::Escape DBD::SQLite);
require_git v2.6;
require_mods qw(DBD::SQLite psgi);
use_ok 'PublicInbox::WWW';
my $cfgtxt = '';
foreach my $i (1..2) {
	my $ibx = create_inbox "test-$i", version => 2, indexlevel => 'basic',
	sub {
		my ($im, $ibx) = @_;
		$im->add(PublicInbox::Eml->new(<<EOF)) or BAIL_OUT;
From: a\@example.com
To: $ibx->{-primary_address}
Subject: s$i
Message-ID: <a-mid-$i\@b>
Date: Fri, 02 Oct 1993 00:00:00 +0000

hello world
EOF
	};
	$cfgtxt .= <<EOM;
[publicinbox "test-$i"]
	address = $ibx->{-primary_address}
	inboxdir = $ibx->{inboxdir}
	url = http://example.com/$i
EOM
}
my $tmpdir = tmpdir;
my $www = PublicInbox::WWW->new(cfg_new($tmpdir, $cfgtxt));

test_psgi(sub { $www->call(@_) }, sub {
	my ($cb) = @_;
	foreach my $i (1..2) {
		foreach my $end ('', '/') {
			my $res = $cb->(GET("/a-mid-$i\@b$end"));
			is($res->code, 302, 'got 302');
			is($res->header('Location'),
				"http://example.com/$i/a-mid-$i\@b/",
				"redirected OK to $i");
		}
	}
	foreach my $x (qw(inv@lid inv@lid/ i/v/a l/i/d/)) {
		my $res = $cb->(GET("/$x"));
		is($res->code, 404, "404 on $x");
	}
});
done_testing;
