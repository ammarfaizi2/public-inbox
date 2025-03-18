#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use Benchmark qw(:all);
use PublicInbox::TestCommon;
require_mods 'PublicInbox::Lg2';
require_git v2.19;
require PublicInbox::Gcf2Client;
my $git_dir = $ENV{GIANT_GIT_DIR} //
	plan skip_all => "GIANT_GIT_DIR not defined for $0";
my $git = PublicInbox::Git->new($git_dir);
my @cat = qw[cat-file --buffer --batch-check=%(objectname)
	--batch-all-objects --unordered];
my $nr = $ENV{NR} || 100;
diag "NR=$nr";
my $n = 0;
my $count = sub { ++$n };

my $gcf2c = PublicInbox::Gcf2Client::new();
my $repo = " $git_dir\n";
my ($lg2_total, $git_total);
my $lg2_async = timeit($nr, sub {
	my $cat = $git->popen(@cat);
	while (<$cat>) {
		chomp;
		$gcf2c->gcf2_async($_.$repo, $count);
	}
	$cat->close or xbail "cat: $?";
	$gcf2c->event_step while PublicInbox::Git::cat_active($gcf2c);
	$lg2_total += $n;
	$n = 0;
});

my $git_async = timeit($nr, sub {
	my $cat = $git->popen(@cat);
	while (<$cat>) {
		chomp;
		$git->cat_async($_, $count);
	}
	$cat->close or xbail "cat: $?";
	$git->async_wait_all;
	$git_total += $n;
	$n = 0;
});

diag 'git '.timestr($git_async);
diag 'lg2 '.timestr($lg2_async);
is $lg2_total, $git_total, 'libgit2 and git saw same number of requests';

done_testing;
