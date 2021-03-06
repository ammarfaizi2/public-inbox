#!perl -w
# Copyright (C) 2019-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use v5.10.1;
use PublicInbox::TestCommon;
use PublicInbox::Import;
use_ok 'PublicInbox::Admin';
my $v1 = create_inbox 'v1', -no_gc => 1, sub {};
my ($tmpdir, $for_destroy) = tmpdir();
my $git_dir = $v1->{inboxdir};
my ($res, $err, $v);
my $v2ibx;
SKIP: {
	require_mods(qw(DBD::SQLite), 5);
	require_git(2.6, 1) or skip 5, 'git too old';
	$v2ibx = create_inbox 'v2', indexlevel => 'basic', version => 2,
				-no_gc => 1, sub {
		my ($v2w, $ibx) = @_;
		$v2w->idx_init;
		$v2w->importer;
	};
};

*resolve_inboxdir = \&PublicInbox::Admin::resolve_inboxdir;

# v1
is(resolve_inboxdir($git_dir), $git_dir, 'top-level GIT_DIR resolved');
is(resolve_inboxdir("$git_dir/objects"), $git_dir, 'GIT_DIR/objects resolved');

ok(chdir($git_dir), 'chdir GIT_DIR works');
is(resolve_inboxdir(), $git_dir, 'resolve_inboxdir works in GIT_DIR');

ok(chdir("$git_dir/objects"), 'chdir GIT_DIR/objects works');
is(resolve_inboxdir(), $git_dir, 'resolve_inboxdir works in GIT_DIR');
$res = resolve_inboxdir(undef, \$v);
is($v, 1, 'version 1 detected');
is($res, $git_dir, 'detects directory along with version');

# $tmpdir could be inside a git working, directory, so we test '/'
SKIP: {
	my $no_vcs_dir = '/';
	# do people version-control "/"?
	skip "$no_vcs_dir is version controlled by git", 4 if -d '/.git';
	open my $null, '>', '/dev/null' or die "open /dev/null: $!";
	open my $olderr, '>&', \*STDERR or die "dup stderr: $!";

	ok(chdir($no_vcs_dir), 'chdir to a non-inbox');
	open STDERR, '>&', $null or die "redirect stderr to /dev/null: $!";
	$res = eval { resolve_inboxdir() };
	open STDERR, '>&', $olderr or die "restore stderr: $!";
	is($res, undef, 'fails inside non-version-controlled dir');

	ok(chdir($tmpdir), 'back to test-specific $tmpdir');
	open STDERR, '>&', $null or die "redirect stderr to /dev/null: $!";
	$res = eval { resolve_inboxdir($no_vcs_dir) };
	$err = $@;
	open STDERR, '>&', $olderr or die "restore stderr: $!";
	is($res, undef, 'fails on non-version-controlled dir');
	ok($err, '$@ set on failure');
}

# v2
if ($v2ibx) {
	my $v2_dir = $v2ibx->{inboxdir};
	is(resolve_inboxdir($v2_dir), $v2_dir,
		'resolve_inboxdir works on v2_dir');
	chdir($v2_dir) or BAIL_OUT "chdir v2_dir: $!";
	is(resolve_inboxdir(), $v2_dir, 'resolve_inboxdir works inside v2_dir');
	$res = resolve_inboxdir(undef, \$v);
	is($v, 2, 'version 2 detected');
	is($res, $v2_dir, 'detects directory along with version');

	# TODO: should work from inside Xapian dirs, and git dirs, here...
	my $objdir = "$v2_dir/git/0.git/objects";
	is($v2_dir, resolve_inboxdir($objdir, \$v), 'at $objdir');
	is($v, 2, 'version 2 detected at $objdir');
	chdir($objdir) or BAIL_OUT "chdir objdir: $!";
	is(resolve_inboxdir(undef, \$v), $v2_dir, 'inside $objdir');
	is($v, 2, 'version 2 detected inside $objdir');
}

chdir '/' or BAIL_OUT "chdir: $!";

my @pairs = (
	'1g' => 1024 ** 3,
	666 => 666,
	'1500K' => 1500 * 1024,
	'15m' => 15 * (1024 ** 2),
);

while (@pairs) {
	my ($in, $out) = splice(@pairs, 0, 2);
	my $orig = $in;
	ok(PublicInbox::Admin::parse_unsigned(\$in), "parse_unsigned $orig");
	is($in, $out, "got $orig => ($in == $out)");
}

for my $v ('', 'bogus', '1p', '1gig') {
	ok(!PublicInbox::Admin::parse_unsigned(\$v),
		"parse_unsigned rejects $v");
}

done_testing();
