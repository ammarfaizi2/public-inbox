#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use autodie;
use PublicInbox::TestCommon;
require_mods 'v2';
use PublicInbox::InboxIdle;
use PublicInbox::DS qw(now);
use PublicInbox::IO qw(write_file);
use PublicInbox::Eml;
my $tmpdir = tmpdir;
local $ENV{HOME} = "$tmpdir";
local $ENV{PI_CONFIG} = "$tmpdir/.public-inbox/config";
my $msg = <<'EOM';
From: a@example.com
To: v1@example.com, v2@example.com
Subject: test
Message-ID: <a@example>

hi
EOM

for my $v (2, 1) {
	run_script(['-init', "-V$v", "v$v", '-Lbasic', "$tmpdir/v$v",
		"https://example.com/v$v", "v$v\@example.com" ]) or
			xbail "init -V$v";
	write_file '>>', $ENV{PI_CONFIG}, "watch = maildir:$tmpdir/md$v";
	mkdir "$tmpdir/md$v/$_" for ('', qw(cur tmp new));
}
my $orig = "$tmpdir/md2/cur/initial:2,S";
write_file '>', $orig, $msg;

my $delivered = 0;
my $cb = sub {
	my ($ibx) = @_;
	diag "message delivered to `$ibx->{name}'";
	$delivered++;
};
my $ii = PublicInbox::InboxIdle->new(my $cfg = PublicInbox::Config->new);
my $obj = bless \$cb, 'PublicInbox::TestCommon::InboxWakeup';
$cfg->each_inbox(sub { $_[0]->subscribe_unlock('ident', $obj) });
my $exp = now + 10;
local @PublicInbox::DS::post_loop_do = (sub { $delivered < 1 || now > $exp});

my $rdr;
open $rdr->{2}, '>>', undef;
my $w = start_script(['-watch'], undef, $rdr);
diag 'waiting for -watch to import 2 existing messages..';
PublicInbox::DS::add_timer 10, \&PublicInbox::Config::noop;
PublicInbox::DS::event_loop;
is $delivered, 1, 'delivered initial message';

$exp = now + 10;
PublicInbox::DS::add_timer 10, \&PublicInbox::Config::noop;
local @PublicInbox::DS::post_loop_do = (sub { $delivered < 3 || now > $exp });
my $tmpmsg = "$tmpdir/md2/tmp/conflict:2,S";
write_file '>', $tmpmsg, $msg, "hi\n";
link $tmpmsg, "$tmpdir/md1/cur/conflict:2,S";
rename $tmpmsg, "$tmpdir/md2/cur/conflict:2,S";
PublicInbox::DS::event_loop;
is $delivered, 3, 'delivered new conflicting message to v2 and new to v1';

my $v1ibx = $cfg->lookup('v1@example.com');
my $v1msgs = $v1ibx->over->recent;
is scalar(@$v1msgs), 1, 'only one message in v1';
my $v1eml = PublicInbox::Eml->new($v1ibx->git->cat_file($v1msgs->[0]->{blob}));
my @mid = $v1eml->header_raw('Message-ID');
is_deeply \@mid, [ qw[<a@example>] ], 'only one Message-ID in v1 message';

my $v2msgs = $cfg->lookup('v2@example.com')->over->recent;
is scalar(@$v2msgs), 2, 'both messages in v2';

done_testing;
