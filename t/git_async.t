# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
$SIG{PIPE} = 'IGNORE';
foreach my $mod (qw(Danga::Socket)) {
	eval "require $mod";
	plan skip_all => "$mod missing for git_async.t" if $@;
}
use File::Temp qw/tempdir/;
use Cwd qw/getcwd/;
my $tmpdir = tempdir('git_async-XXXXXX', TMPDIR => 1, CLEANUP => 1);
use_ok 'PublicInbox::Git';
my $dir = "$tmpdir/git.git";
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
	my $f = 'HEAD:foo.txt';
	my @args;
	my $n = 0;
	my $git = PublicInbox::Git->new($dir);
	Danga::Socket->SetPostLoopCallback(sub {
		my ($fdmap) = @_;
		foreach (values %$fdmap) {
			return 1 if ref($_) =~ /::GitAsync/;
		}
		0
	});
	$git->check_async_ds($f, sub {
		$n++;
		@args = @_;
		$git = undef;
	});
	Danga::Socket->EventLoop;
	my @exp = PublicInbox::Git->new($dir)->check($f);
	my $exp = [ \@exp ];
	is_deeply(\@args, $exp, 'matches regular check');
	is($n, 1, 'callback only called once');
	$git = PublicInbox::Git->new($dir);
	$n = 0;
	my $max = 100;
	my $missing = 'm';
	my $m = 0;
	for my $i (0..$max) {
		my $k = "HEAD:m$i";
		$git->check_async_ds($k, sub {
			my ($info) = @_;
			++$n;
			++$m if $info->[1] eq 'missing' && $info->[0] eq $k;
		});
		if ($git->{async_c}->{wr}->{write_buf_size}) {
			diag("async_check capped at $i");
			$max = $i;
			last;
		}
	}
	is($m, $n, 'everything expected missing is missing');
	$git->check_async_ds($f, sub { $git = undef });
	Danga::Socket->EventLoop;

	$git = PublicInbox::Git->new($dir);
	my $info;
	my $str = '';
	my @missing;
	$git->cat_async_ds('HEAD:miss', sub {
		my ($miss) = @_;
		push @missing, $miss;
	});
	$git->cat_async_ds($f, sub {
		my $res = $_[0];
		if (ref($res) eq 'ARRAY') {
			is($info, undef, 'info unset, setting..');
			$info = $res;
		} elsif (ref($res) eq 'SCALAR') {
			$str .= $$res;
			if (length($str) >= $info->[2]) {
				is($info->[2], length($str), 'length match');
				$git = undef
			}
		}
	});
	Danga::Socket->EventLoop;
	is_deeply(\@missing, [['HEAD:miss', 'missing']], 'missing cat OK');
	is($git, undef, 'git undefined');
	$git = PublicInbox::Git->new($dir);
	my $sref = $git->cat_file($f);
	is($str, $$sref, 'matches synchronous version');
	$git = undef;
	Danga::Socket->RunTimers;
}

{
	my $git = PublicInbox::Git->new($dir);
	foreach my $s (qw(check_async_compat cat_async_compat)) {
		my @missing;
		$git->check_async_compat('HED:miss1ng', sub {
			my ($miss) = @_;
			push @missing, $miss;
		});
		is_deeply(\@missing, [['HED:miss1ng', 'missing']],
			"missing $s OK");
	}
	my @info;
	my $str = '';
	$git->cat_async_compat('HEAD:foo.txt', sub {
		my $ref = $_[0];
		my $t = ref $ref;
		if ($t eq 'ARRAY') {
			push @info, $ref;
		} elsif ($t eq 'SCALAR') {
			$str .= $$ref;
		} else {
			fail "fail type: $t";
		}
	});
	is_deeply(\@info, [ [ 'bf4f17855632367a160bef055fc8ba4675d10e6b',
			       'blob', 18 ]], 'info matches compat');
	is($str, "-----\nhello\nworld\n", 'data matches compat');
}

done_testing();

1;
