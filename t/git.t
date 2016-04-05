# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)
use strict;
use warnings;
use Test::More;
use File::Temp qw/tempdir/;
my $dir = tempdir('pi-git-XXXXXX', TMPDIR => 1, CLEANUP => 1);
use Cwd qw/getcwd/;

use_ok 'PublicInbox::Git';
{
	is(system(qw(git init -q --bare), $dir), 0, 'created git directory');
	my @cmd = ('git', "--git-dir=$dir", 'fast-import', '--quiet');

	my $fi_data = getcwd().'/t/git.fast-import-data';
	ok(-r $fi_data, "fast-import data readable (or run test at top level)");
	my $pid = fork;
	defined $pid or die "fork failed: $!\n";
	if ($pid == 0) {
		open STDIN, '<', $fi_data or die "open $fi_data: $!\n";
		exec @cmd;
		die "failed exec: ",join(' ', @cmd),": $!\n";
	}
	waitpid $pid, 0;
	is($?, 0, 'fast-import succeeded');
}

{
	my $gcf = PublicInbox::Git->new($dir);
	my $f = 'HEAD:foo.txt';
	my @x = $gcf->check($f);
	is(scalar @x, 3, 'returned 3 element array for existing file');
	like($x[0], qr/\A[a-f0-9]{40}\z/, 'returns obj ID in 1st element');
	is('blob', $x[1], 'returns obj type in 2nd element');
	like($x[2], qr/\A\d+\z/, 'returns obj size in 3rd element');

	my $raw = $gcf->cat_file($f);
	is($x[2], length($$raw), 'length matches');

	{
		my $size;
		my $rv = $gcf->cat_file($f, sub {
			my ($in, $left) = @_;
			$size = $$left;
			'nothing'
		});
		is($rv, 'nothing', 'returned from callback without reading');
		is($size, $x[2], 'set size for callback correctly');
	}

	eval { $gcf->cat_file($f, sub { die 'OMG' }) };
	like($@, qr/\bOMG\b/, 'died in callback propagated');
	is(${$gcf->cat_file($f)}, $$raw, 'not broken after failures');

	{
		my ($buf, $r);
		my $rv = $gcf->cat_file($f, sub {
			my ($in, $left) = @_;
			$r = read($in, $buf, 2);
			$$left -= $r;
			'blah'
		});
		is($r, 2, 'only read 2 bytes');
		is($buf, '--', 'partial read succeeded');
		is($rv, 'blah', 'return value propagated');
	}
	is(${$gcf->cat_file($f)}, $$raw, 'not broken after partial read');
}

if (1) {
	use POSIX qw(dup2);
	my @cmd = ('git', "--git-dir=$dir", qw(hash-object -w --stdin));

	# need a big file, use the AGPL-3.0 :p
	my $big_data = getcwd().'/COPYING';
	ok(-r $big_data, 'COPYING readable');
	my $size = -s $big_data;
	ok($size > 8192, 'file is big enough');

	my ($r, $w);
	ok(pipe($r, $w), 'created pipe');

	my $pid = fork;
	defined $pid or die "fork failed: $!\n";
	if ($pid == 0) {
		close $r;
		open STDIN, '<', $big_data or die "open $big_data: $!\n";
		dup2(fileno($w), 1);
		exec @cmd;
		die "failed exec: ",join(' ', @cmd),": $!\n";
	}
	close $w;
	my $n = read $r, my $buf, 41;
	waitpid $pid, 0;
	is(0, $?, 'hashed object successfully');
	chomp $buf;

	my $gcf = PublicInbox::Git->new($dir);
	my $rsize;
	is($gcf->cat_file($buf, sub {
		$rsize = ${$_[1]};
		'x';
	}), 'x', 'checked input');
	is($rsize, $size, 'got correct size on big file');

	my $x = $gcf->cat_file($buf, \$rsize);
	is($rsize, $size, 'got correct size ref on big file');
	is(length($$x), $size, 'read correct number of bytes');

	my $rline;
	$gcf->cat_file($buf, sub {
		my ($in, $left) = @_;
		$rline = <$in>;
		$$left -= length($rline);
	});
	{
		open my $fh, '<', $big_data or die "open failed: $!\n";
		is($rline, <$fh>, 'first line matches');
	};

	my $all;
	$gcf->cat_file($buf, sub {
		my ($in, $left) = @_;
		my $x = read($in, $all, $$left);
		$$left -= $x;
	});
	{
		open my $fh, '<', $big_data or die "open failed: $!\n";
		local $/;
		is($all, <$fh>, 'entire read matches');
	};

	my $ref = $gcf->qx(qw(cat-file blob), $buf);
	is($all, $ref, 'qx read giant single string');

	my @ref = $gcf->qx(qw(cat-file blob), $buf);
	is($all, join('', @ref), 'qx returned array when wanted');
	my $nl = scalar @ref;
	ok($nl > 1, "qx returned array length of $nl");
}

{
	my $git = PublicInbox::Git->new($dir);

	my $err = $git->popen([qw(cat-file blob non-existent)], undef,
				{ 2 => $git->err_begin });
	my @out = <$err>;
	my $close_ret = close $err;
	my $close_err = $?;
	is(join('', @out), '', 'no output on stdout on error');
	isnt($close_err, 0, 'close set $? on bad command');
	ok(!$close_ret, 'close returned error on bad command');
	isnt($git->err, '', 'got stderr output');

	$err = $git->popen([qw(tag -l)], undef, { 2 => $git->err_begin });
	@out = <$err>;
	$close_ret = close $err;
	$close_err = $?;
	is(join('', @out), '', 'no output on stdout on error');
	ok(!$close_err, 'close clobbered $? on empty output');
	ok($close_ret, 'close returned error on empty output');
	is($git->err, '', 'no stderr output');
}

done_testing();
