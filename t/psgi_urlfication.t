#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# corner cases to abuse URL-fication
use v5.12; use PublicInbox::TestCommon;
use autodie;
use PublicInbox::IO qw(write_file);
require_mods(qw(DBD::SQLite Xapian psgi -httpd));
require PublicInbox::Eml;
require PublicInbox::WWW;

my $ibx_a = create_inbox 'a', indexlevel => 'basic', sub {
	my ($im, $ibx) = @_;
	$im->add(PublicInbox::Eml->new(<<EOM)) or xbail;
Date: Fri, 02 Oct 1993 00:00:04 +0000
From: a\@example.com
Message-ID: <xpost-addr-urlfic\@tion>
To: <$ibx->{-primary_address}>
Cc: <;>, <"foo>">, <amp&amp\@wtf>, <gt>, "<", ">", <lt>,
	somethingelse\@example.com
EOM
};

my $ibx_b = create_inbox 'b', indexlevel => 'basic', sub {
	my ($im, $ibx) = @_;
	$im->add(PublicInbox::Eml->new(<<EOM)) or xbail;
Date: Fri, 02 Oct 1993 00:00:00 +0000
Message-ID: <wh\@tever>
From: b\@example.com
To: <$ibx->{-primary_address}>
Cc: <$ibx_a->{-primary_address}>
EOM
};

my $tmpdir = tmpdir;
my $cfgpath = "$tmpdir/public-inbox.config";

write_file '>', $cfgpath, <<EOM;
[publicinbox]
	nameIsUrl = true
[publicinbox "a"]
	inboxdir = $ibx_a->{inboxdir}
	address = $ibx_a->{-primary_address}
	address = somethingelse\@example.com
[publicinbox "b"]
	inboxdir = $ibx_b->{inboxdir}
	address = $ibx_b->{-primary_address}
	address = ";"
	address = &
	address = gt
	address = >
	address = <
EOM
my $cfg = PublicInbox::Config->new($cfgpath);
my $www = PublicInbox::WWW->new($cfg);
my $env = { TMPDIR => "$tmpdir", PI_CONFIG => $cfgpath };
my $client = sub {
	my ($cb) = @_;
	my $res = $cb->(GET('/a/xpost-addr-urlfic@tion/'));
	my $content = $res->content;
	for my $c ('&', ';', '<', '>') {
		unlike $content, qr/>$c</s, "no bare `$c' from URL-ification";
	}
	like $content, qr/>somethingelse\@example\.com</s,
		'address linkified';
};

test_psgi(sub { $www->call(@_) }, $client);
test_httpd $env, $client;

done_testing;
