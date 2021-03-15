#!perl -w
# Copyright (C) 2016-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use v5.10.1;
use PublicInbox::TestCommon;
use PublicInbox::Eml;
use PublicInbox::Msgmap;
require_git(2.6);
require_mods(qw(DBD::SQLite Search::Xapian));
my $another = 'another-nntp.sqlite3';
my $altid = [ "serial:gmane:file=$another" ];
my $ibx = create_inbox 'v2', version => 2, indexlevel => 'medium',
			altid => $altid, sub {
	my ($im, $ibx) = @_;
	my $mm = PublicInbox::Msgmap->new_file("$ibx->{inboxdir}/$another", 1);
	$mm->mid_set(1234, 'a@example.com') == 1 or BAIL_OUT 'mid_set once';
	ok(0 == $mm->mid_set(1234, 'a@example.com'), 'mid_set not idempotent');
	ok(0 == $mm->mid_set(1, 'a@example.com'), 'mid_set fails with dup MID');
	$im->add(PublicInbox::Eml->new(<<'EOF')) or BAIL_OUT;
From: a@example.com
To: b@example.com
Subject: boo!
Message-ID: <a@example.com>

hello world gmane:666
EOF
};
my $mm = PublicInbox::Msgmap->new_file("$ibx->{inboxdir}/$another", 1);
ok(0 == $mm->mid_set(1234, 'a@example.com'), 'mid_set not idempotent');
ok(0 ==  $mm->mid_set(1, 'a@example.com'), 'mid_set fails with dup MID');
my $mset = $ibx->search->mset('gmane:1234');
my $msgs = $ibx->search->mset_to_smsg($ibx, $mset);
$msgs = [ map { $_->{mid} } @$msgs ];
is_deeply($msgs, ['a@example.com'], 'got one match');
$mset = $ibx->search->mset('gmane:666');
is($mset->size, 0, 'body did NOT match');

done_testing();
