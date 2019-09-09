# Copyright (C) 2018-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use PublicInbox::MIME;
use File::Temp qw/tempdir/;
use Fcntl qw(SEEK_SET);
use Cwd;
require './t/common.perl';
require_git(2.6);

my $V = 2;
foreach my $mod (qw(DBD::SQLite Search::Xapian)) {
	eval "require $mod";
	plan skip_all => "$mod missing for v2mda.t" if $@;
}
use_ok 'PublicInbox::V2Writable';
my $tmpdir = tempdir('pi-v2mda-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $ibx = {
	mainrepo => "$tmpdir/inbox",
	name => 'test-v2writable',
	address => [ 'test@example.com' ],
};
my $mime = PublicInbox::MIME->create(
	header => [
		From => 'a@example.com',
		To => 'test@example.com',
		Subject => 'this is a subject',
		Date => 'Fri, 02 Oct 1993 00:00:00 +0000',
		'Message-ID' => '<foo@bar>',
		'List-ID' => '<test.example.com>',
	],
	body => "hello world\n",
);

my $mda = "blib/script/public-inbox-mda";
ok(-f "blib/script/public-inbox-mda", '-mda exists');
my $main_bin = getcwd()."/t/main-bin";
my $fail_bin = getcwd()."/t/fail-bin";
local $ENV{PI_DIR} = "$tmpdir/foo";
my $fail_path = "$fail_bin:blib/script:$ENV{PATH}";
local $ENV{PATH} = "$main_bin:blib/script:$ENV{PATH}";
my $faildir = "$tmpdir/fail";
local $ENV{PI_EMERGENCY} = $faildir;
ok(mkdir $faildir);
my @cmd = (qw(public-inbox-init), "-V$V", $ibx->{name},
		$ibx->{mainrepo}, 'http://localhost/test',
		$ibx->{address}->[0]);
ok(PublicInbox::Import::run_die(\@cmd), 'initialized v2 inbox');

open my $tmp, '+>', undef or die "failed to open anonymous tempfile: $!";
ok($tmp->print($mime->as_string), 'wrote to temporary file');
ok($tmp->flush, 'flushed temporary file');
ok($tmp->sysseek(0, SEEK_SET), 'seeked');

my $rdr = { 0 => fileno($tmp) };
local $ENV{ORIGINAL_RECIPIENT} = 'test@example.com';
ok(PublicInbox::Import::run_die(['public-inbox-mda'], undef, $rdr),
	'mda delivered a message');

$ibx = PublicInbox::Inbox->new($ibx);

if ($V == 1) {
	my $cmd = [ 'public-inbox-index', "$tmpdir/inbox" ];
	ok(PublicInbox::Import::run_die($cmd, undef, $rdr), 'v1 indexed');
}
my $msgs = $ibx->search->query('');
is(scalar(@$msgs), 1, 'only got one message');
my $saved = $ibx->smsg_mime($msgs->[0]);
is($saved->{mime}->as_string, $mime->as_string, 'injected message');

{
	my @new = glob("$faildir/new/*");
	is_deeply(\@new, [], 'nothing in faildir');
	local $ENV{PATH} = $fail_path;
	$mime->header_set('Message-ID', '<bar@foo>');
	ok($tmp->sysseek(0, SEEK_SET) &&
			$tmp->truncate(0) &&
			$tmp->print($mime->as_string) &&
			$tmp->flush &&
			$tmp->sysseek(0, SEEK_SET),
		'rewound and rewrite temporary file');
	my $cmd = ['public-inbox-mda'];
	ok(PublicInbox::Import::run_die($cmd, undef, $rdr),
		'mda did not die on "spam"');
	@new = glob("$faildir/new/*");
	is(scalar(@new), 1, 'got a message in faildir');
	$msgs = $ibx->search->reopen->query('');
	is(scalar(@$msgs), 1, 'no new message');

	my $config = "$ENV{PI_DIR}/config";
	ok(-f $config, 'config exists');
	my $k = 'publicinboxmda.spamcheck';
	is(system('git', 'config', "--file=$config", $k, 'none'), 0,
		'disabled spamcheck for mda');
	ok($tmp->sysseek(0, SEEK_SET), 'rewound input file');

	ok(PublicInbox::Import::run_die($cmd, undef, $rdr), 'mda did not die');
	my @again = glob("$faildir/new/*");
	is_deeply(\@again, \@new, 'no new message in faildir');
	$msgs = $ibx->search->reopen->query('');
	is(scalar(@$msgs), 2, 'new message added OK');
}

{
	my $patch = 't/data/0001.patch';
	open my $fh, '<', $patch or die "failed to open $patch: $!\n";
	$rdr = { 0 => fileno($fh) };
	ok(PublicInbox::Import::run_die(['public-inbox-mda'], undef, $rdr),
		'mda delivered a patch');
	my $post = $ibx->search->reopen->query('dfpost:6e006fd7');
	is(scalar(@$post), 1, 'got one result for dfpost');
	my $pre = $ibx->search->query('dfpre:090d998');
	is(scalar(@$pre), 1, 'got one result for dfpre');
	is($post->[0]->{blob}, $pre->[0]->{blob}, 'same message in both cases');
}

done_testing();
