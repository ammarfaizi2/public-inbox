#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use Cwd qw(getcwd);
use autodie qw(open close);
use PublicInbox::Import;
use PublicInbox::DS;

require_mods('PublicInbox::Gcf2');
use_ok 'PublicInbox::Gcf2Client';
my ($tmpdir, $for_destroy) = tmpdir();
my $git_a = "$tmpdir/a.git";
my $git_b = "$tmpdir/b.git";
PublicInbox::Import::init_bare($git_a);
PublicInbox::Import::init_bare($git_b);
my $fi_data = './t/git.fast-import-data';
my $rdr = {};
open $rdr->{0}, '<', $fi_data;
xsys([qw(git fast-import --quiet)], { GIT_DIR => $git_a }, $rdr);
is($?, 0, 'fast-import succeeded');

my $tree = 'fdbc43725f21f485051c17463b50185f4c3cf88c';
my $called = 0;
my $err_f = "$tmpdir/err";
{
	PublicInbox::DS->Reset;
	open my $err, '>>', $err_f;
	my $gcf2c = PublicInbox::Gcf2Client::new({ 2 => $err });
	$gcf2c->gcf2_async("$tree $git_a\n", sub {
		my ($bref, $oid, $type, $size, $arg) = @_;
		is($oid, $tree, 'got expected OID');
		is($size, 30, 'got expected length');
		is($type, 'tree', 'got tree type');
		is(length($$bref), 30, 'got a tree');
		is($arg, 'hi', 'arg passed');
		$called++;
	}, 'hi');
	$gcf2c->cat_async_step($gcf2c->{inflight});

	open $err, '<', $err_f;
	my $estr = do { local $/; <$err> };
	is($estr, '', 'nothing in stderr');

	my $trunc = substr($tree, 0, 39);
	$gcf2c->gcf2_async("$trunc $git_a\n", sub {
		my ($bref, $oid, $type, $size, $arg) = @_;
		is(undef, $bref, 'missing bref is undef');
		is($oid, $trunc, 'truncated OID printed');
		is($type, 'missing', 'type is "missing"');
		is($size, undef, 'size is undef');
		is($arg, 'bye', 'arg passed when missing');
		$called++;
	}, 'bye');
	$gcf2c->cat_async_step($gcf2c->{inflight});

	open $err, '<', $err_f;
	$estr = do { local $/; <$err> };
	like($estr, qr/retrying/, 'warned about retry');

	# try failed alternates lookup
	PublicInbox::DS->Reset;
	open $err, '>', $err_f;
	$gcf2c = PublicInbox::Gcf2Client::new({ 2 => $err });
	$gcf2c->gcf2_async("$tree $git_b\n", sub {
		my ($bref, $oid, $type, $size, $arg) = @_;
		is(undef, $bref, 'missing bref from alt is undef');
		$called++;
	});
	$gcf2c->cat_async_step($gcf2c->{inflight});
	open $err, '<', $err_f;
	$estr = do { local $/; <$err> };
	like($estr, qr/retrying/, 'warned about retry before alt update');

	# now try successful alternates lookup
	open my $alt, '>>', "$git_b/objects/info/alternates";
	print $alt "$git_a/objects\n";
	close $alt;
	my $expect = xqx(['git', "--git-dir=$git_a", qw(cat-file tree), $tree]);
	$gcf2c->gcf2_async("$tree $git_a\n", sub {
		my ($bref, $oid, $type, $size, $arg) = @_;
		is($oid, $tree, 'oid match on alternates retry');
		is($$bref, $expect, 'tree content matched');
		$called++;
	});
	$gcf2c->cat_async_step($gcf2c->{inflight});
}
is($called, 4, 'gcf2_async callbacks hit');
done_testing;
