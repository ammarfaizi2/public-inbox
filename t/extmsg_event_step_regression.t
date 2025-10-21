#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use PublicInbox::Eml;
use PublicInbox::IO qw(write_file);
require_mods qw(-httpd v2 psgi);
my $tmpdir = tmpdir;
my $one = PublicInbox::Eml->new(<<'EOM');
From: X <x@example.com>
Subject: subj
Date: Sat, 16 Jun 2007 17:33:53 +0100
Message-ID: <681fb71bcf6@example.com>
To: list@example.com
Mime-Version: 1.0
Content-Type: text/plain

abc
EOM
my $two = PublicInbox::Eml->new(<<'EOM');
From: X <x@example.com>
Subject: Re: subj
Date: Sat, 30 Jun 2007 18:56:36 +0100
Message-ID: <eb1c75afc92@example.com>
References: <681fb71bcf6@example.com>
    <de34bbcfcf7@example.com>
In-Reply-To: <de34bbcfcf7@example.com>
To: Y <y@example.com>
Cc: list@example.com
MIME-Version: 1.0
Content-Type: text/plain

def
EOM
my %ibx = map {
	$_ => create_inbox "v$_-event_step", version => $_,
			indexlevel => 'medium', sub {
		my ($im, $ibx) = @_;
		$im->add($one) or xbail 'add 1';
		$im->add($two) or xbail 'add 2';
	};
} (1, 2);
write_file '>', my $cfgpath = "$tmpdir/config", <<EOM;
[publicinbox "list"]
	address = list\@example.com
        url = http://example.com/list
	inboxdir = $ibx{1}->{inboxdir}
[publicinbox "list2"]
	address = list2\@example.com
        url = http://example.com/list2
	inboxdir = $ibx{2}->{inboxdir}
EOM
my $env = { TMPDIR => $tmpdir, PI_CONFIG => $cfgpath };
my $client = sub {
	my ($cb) = @_;
	my $res = $cb->(GET('/list/de34bbcfcf7@example.com/'));
	is $res->code, 300, 'v1 300';
	$res = $cb->(GET('/list2/de34bbcfcf7@example.com/'));
	is $res->code, 300, 'v2 300';
};

test_httpd($env, $client);
{
	require PublicInbox::WWW;
	my $cfg = PublicInbox::Config->new($cfgpath);
	my $www = PublicInbox::WWW->new($cfg);
	test_psgi(sub { $www->call(@_) }, $client);
}
done_testing;
