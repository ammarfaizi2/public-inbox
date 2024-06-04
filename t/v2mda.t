#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.10.1;
use strict;
use Fcntl qw(SEEK_SET);
use Cwd;
use PublicInbox::TestCommon;
use PublicInbox::Eml;
use File::Path qw(remove_tree);
require_git(2.6);

my $V = 2;
require_mods(qw(DBD::SQLite Xapian));
use_ok 'PublicInbox::V2Writable';
my ($tmpdir, $for_destroy) = tmpdir();
my $ibx = {
	inboxdir => "$tmpdir/inbox",
	name => 'test-v2writable',
	address => [ 'test@example.com' ],
};
my $mime = PublicInbox::Eml->new(<<'EOF');
From: a@example.com
To: test@example.com
Subject: this is a subject
Date: Fri, 02 Oct 1993 00:00:00 +0000
Message-ID: <foo@bar>
List-ID: <test.example.com>

hello world
EOF
my $main_bin = getcwd()."/t/main-bin";
my $fail_bin = getcwd()."/t/fail-bin";
local $ENV{PI_DIR} = "$tmpdir/foo";
my $fail_path = "$fail_bin:blib/script:$ENV{PATH}";
local $ENV{PATH} = "$main_bin:blib/script:$ENV{PATH}";
my $faildir = "$tmpdir/fail";
local $ENV{PI_EMERGENCY} = $faildir;
ok(mkdir $faildir);
my @cmd = (qw(-init), "-V$V", $ibx->{name},
		$ibx->{inboxdir}, 'http://localhost/test',
		$ibx->{address}->[0]);
ok(run_script(\@cmd), 'initialized v2 inbox');

my $rdr = { 0 => \($mime->as_string) };
local $ENV{ORIGINAL_RECIPIENT} = 'test@example.com';
ok(run_script(['-mda'], undef, $rdr), 'mda delivered a message');

$ibx = PublicInbox::Inbox->new($ibx);
my $msgs = $ibx->over->recent;
is(scalar(@$msgs), 1, 'only got one message');
my $eml = $ibx->smsg_eml($msgs->[0]);
is($eml->as_string, $mime->as_string, 'injected message');

{
	my @new = glob("$faildir/new/*");
	is_deeply(\@new, [], 'nothing in faildir');
	local $ENV{PATH} = $fail_path;
	$mime->header_set('Message-ID', '<bar@foo>');
	$rdr->{0} = \($mime->as_string);
	ok(run_script(['-mda'], undef, $rdr), 'mda did not die on "spam"');
	@new = glob("$faildir/new/*");
	is(scalar(@new), 1, 'got a message in faildir');
	$msgs = $ibx->over->recent;
	is(scalar(@$msgs), 1, 'no new message');

	my $config = "$ENV{PI_DIR}/config";
	ok(-f $config, 'config exists');
	my $k = 'publicinboxmda.spamcheck';
	is(xsys('git', 'config', "--file=$config", $k, 'none'), 0,
		'disabled spamcheck for mda');

	ok(run_script(['-mda'], undef, $rdr), 'mda did not die');
	my @again = glob("$faildir/new/*");
	is_deeply(\@again, \@new, 'no new message in faildir');
	$msgs = $ibx->over->recent;
	is(scalar(@$msgs), 2, 'new message added OK');
}

{
	my $patch = 't/data/0001.patch';
	open my $fh, '<', $patch or die "failed to open $patch: $!\n";
	$rdr->{0} = \(do { local $/; <$fh> });
	ok(run_script(['-mda'], undef, $rdr), 'mda delivered a patch');
	my $post = $ibx->search->reopen->mset('dfpost:6e006fd7');
	is($post->size, 1, 'got one result for dfpost');
	my $pre = $ibx->search->mset('dfpre:090d998');
	is($pre->size, 1, 'got one result for dfpre');
	$pre = $ibx->search->mset_to_smsg($ibx, $pre);
	$post = $ibx->search->mset_to_smsg($ibx, $post);
	is($post->[0]->{blob}, $pre->[0]->{blob}, 'same message in both cases');

	# git patch-id --stable <t/data/0001.patch | awk '{print $1}'
	my $patchid = '91ee6b761fc7f47cad9f2b09b10489f313eb5b71';
	my $mset = $ibx->search->mset("patchid:$patchid");
	is($mset->size, 1, 'patchid search works');
}

{
	my @shards = grep(m!/[0-9]+\z!, glob("$ibx->{inboxdir}/xap*/*"));
	ok(remove_tree(@shards), 'rm shards to convert to indexlevel=basic');
	$ibx->do_cleanup;
	$rdr->{2} = \(my $err = '');
	$rdr->{0} = \<<'EOM';
From: a@example.com
To: test@example.com
Subject: this is a ham message for learn
Date: Fri, 02 Oct 1993 00:00:00 +0000
Message-ID: <ham@example>

yum
EOM
	my ($id, $prev);
	is($ibx->over->next_by_mid('ham@example', \$id, \$prev), undef,
		'no ham@example, yet');
	ok(run_script([qw(-learn ham)], undef, $rdr), '-learn runs on basic')
		or diag $err;
	my $smsg = $ibx->over->next_by_mid('ham@example', \$id, \$prev);
	ok($smsg, 'ham message learned w/ indexlevel=basic');
	@shards = grep(m!/[0-9]+\z!, glob("$ibx->{inboxdir}/xap*/*"));
	is_deeply(\@shards, [], 'not converted to medium/full after learn');

	$rdr->{0} = \<<'EOM';
From: a@example.com
To: test@example.com
Subject: this is a message for -mda to stay basic
Date: Fri, 02 Oct 1993 00:00:00 +0000
Message-ID: <mda-stays-basic@example>

yum
EOM
	ok run_script(['-mda'], undef, $rdr), '-learn runs on basic'
		or diag $err;
	@shards = grep m!/[0-9]+\z!, glob("$ibx->{inboxdir}/xap*/*");
	is_deeply \@shards, [], 'not converted to medium/full after -mda';
}

done_testing();
