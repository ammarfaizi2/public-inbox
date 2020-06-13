# Copyright (C) 2015-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use PublicInbox::TestCommon;
my ($dir, $for_destroy) = tmpdir();
use PublicInbox::Spawn qw(popen_rd);
use PublicInbox::Import;

use_ok 'PublicInbox::Git';

{
	PublicInbox::Import::init_bare($dir);
	my $fi_data = './t/git.fast-import-data';
	open my $fh, '<', $fi_data or die
		"fast-import data readable (or run test at top level: $!";
	my $rdr = { 0 => $fh };
	xsys([qw(git fast-import --quiet)], { GIT_DIR => $dir }, $rdr);
	is($?, 0, 'fast-import succeeded');
}

{
	my $gcf = PublicInbox::Git->new($dir);
	is($gcf->modified, 749520000, 'modified time detected from commit');
	my $f = 'HEAD:foo.txt';
	my @x = $gcf->check($f);
	is(scalar @x, 3, 'returned 3 element array for existing file');
	like($x[0], qr/\A[a-f0-9]{40}\z/, 'returns obj ID in 1st element');
	is('blob', $x[1], 'returns obj type in 2nd element');
	like($x[2], qr/\A\d+\z/, 'returns obj size in 3rd element');

	my $raw = $gcf->cat_file($f);
	is($x[2], length($$raw), 'length matches');

	is(${$gcf->cat_file($f)}, $$raw, 'not broken after failures');
	is(${$gcf->cat_file($f)}, $$raw, 'not broken after partial read');

	my $oid = $x[0];
	my $arg = { 'foo' => 'bar' };
	my $res = [];
	my $missing = [];
	$gcf->cat_async($oid, sub {
		my ($bref, $oid_hex, $type, $size, $arg) = @_;
		$res = [ @_ ];
	}, $arg);
	$gcf->cat_async('non-existent', sub {
		my ($bref, $oid_hex, $type, $size, $arg) = @_;
		$missing = [ @_ ];
	}, $arg);
	$gcf->cat_async_wait;
	my ($bref, $oid_hex, $type, $size, $arg_res) = @$res;
	is_deeply([$oid_hex, $type, $size], \@x, 'got expected header');
	is($arg_res, $arg, 'arg passed to cat_async');
	is_deeply($raw, $bref, 'blob result matches');
	is_deeply($missing, [ undef, 'non-existent', 'missing', undef, $arg],
		'non-existent blob gives expected result');
}

if (1) {
	# need a big file, use the AGPL-3.0 :p
	my $big_data = './COPYING';
	ok(-r $big_data, 'COPYING readable');
	my $size = -s $big_data;
	ok($size > 8192, 'file is big enough');
	open my $fh, '<', $big_data or die;
	my $cmd = [ 'git', "--git-dir=$dir", qw(hash-object -w --stdin) ];
	my $buf = xqx($cmd, { GIT_DIR => $dir }, { 0 => $fh });
	is(0, $?, 'hashed object successfully');
	chomp $buf;

	my $gcf = PublicInbox::Git->new($dir);
	my $rsize;
	my $x = $gcf->cat_file($buf, \$rsize);
	is($rsize, $size, 'got correct size ref on big file');
	is(length($$x), $size, 'read correct number of bytes');

	my $ref = $gcf->qx(qw(cat-file blob), $buf);
	my @ref = $gcf->qx(qw(cat-file blob), $buf);
	my $nl = scalar @ref;
	ok($nl > 1, "qx returned array length of $nl");

	$gcf->qx(qw(repack -adq));
	ok($gcf->packed_bytes > 0, 'packed size is positive');
}

if ('alternates reloaded') {
	my ($alt, $alt_obj) = tmpdir();
	my $hash_obj = [ 'git', "--git-dir=$alt", qw(hash-object -w --stdin) ];
	PublicInbox::Import::init_bare($alt);
	open my $fh, '<', "$alt/config" or die "open failed: $!\n";
	chomp(my $remote = xqx($hash_obj, undef, { 0 => $fh }));
	my $gcf = PublicInbox::Git->new($dir);
	is($gcf->cat_file($remote), undef, "remote file not found");
	open $fh, '>>', "$dir/objects/info/alternates" or
			die "open failed: $!\n";
	print $fh "$alt/objects\n" or die "print failed: $!\n";
	close $fh or die "close failed: $!";
	my $found = $gcf->cat_file($remote);
	open $fh, '<', "$alt/config" or die "open failed: $!\n";
	my $config = eval { local $/; <$fh> };
	is($$found, $config, 'alternates reloaded');

	# with the async interface
	my ($async_alt, $async_dir_obj) = tmpdir();
	PublicInbox::Import::init_bare($async_alt);
	my @exist = map { chomp; [ split / / ] } (xqx(['git', "--git-dir=$dir",
			qw(cat-file --batch-all-objects --batch-check)]));
	my $results = [];
	my $cb = sub {
		my ($bref, $oid, $type, $size) = @_;
		push @$results, [ $oid, $type, $size ];
	};
	for my $i (0..5) {
		$gcf->cat_async($exist[$i]->[0], $cb, $results);
		next if $i != 3;

		# stick a new alternate into a running async pipeline
		$hash_obj->[1] = "--git-dir=$async_alt";
		$remote = xqx($hash_obj, undef, { 0 => \'async' });
		chomp $remote;
		open $fh, '>>', "$dir/objects/info/alternates" or
				die "open failed: $!\n";
		print $fh "$async_alt/objects\n" or die "print failed: $!\n";
		close $fh or die "close failed: $!";
		# trigger cat_async_retry:
		$gcf->cat_async($remote, $cb, $results);
	}
	$gcf->cat_async_wait;
	my $expect = [ @exist[0..3], [ $remote, 'blob', 5 ], @exist[4..5] ];
	is_deeply($results, $expect, 'got expected results');

	ok(!$gcf->cleanup, 'cleanup can expire');
	ok(!$gcf->cleanup, 'cleanup idempotent');

	my $t = $gcf->modified;
	ok($t <= time, 'repo not modified in the future');
	isnt($t, 0, 'repo not modified in 1970')
}

use_ok 'PublicInbox::Git', qw(git_unquote git_quote);
my $s;
is("foo\nbar", git_unquote($s = '"foo\\nbar"'), 'unquoted newline');
is("Eléanor", git_unquote($s = '"El\\303\\251anor"'), 'unquoted octal');
is(git_unquote($s = '"I\"m"'), 'I"m', 'unquoted dq');
is(git_unquote($s = '"I\\m"'), 'I\\m', 'unquoted backslash');

is(git_quote($s = "Eléanor"), '"El\\303\\251anor"', 'quoted octal');
is(git_quote($s = "hello\"world"), '"hello\"world"', 'quoted dq');
is(git_quote($s = "hello\\world"), '"hello\\\\world"', 'quoted backslash');
is(git_quote($s = "hello\nworld"), '"hello\\nworld"', 'quoted LF');

done_testing();
