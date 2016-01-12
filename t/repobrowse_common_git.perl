#!/usr/bin/perl -w
# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use Data::Dumper;
use File::Temp qw/tempdir/;
use Cwd qw/getcwd/;
my @mods = qw(HTTP::Request::Common Plack::Request Plack::Test URI::Escape);
foreach my $mod (@mods) {
	eval "require $mod";
	plan skip_all => "$mod missing for $0" if $@;
}

sub dechunk ($) {
	my ($res) = @_;
	my $s = $res->content;
	if (lc($res->header('Transfer-Encoding')) eq 'chunked') {
		my $rv = '';
		while ($s =~ s/\A([a-f0-9]+)\r\n//i) { # no comment support :x
			my $n = hex($1) or last;
			$rv .= substr($s, 0, $n);
			$s = substr($s, $n);
			$s =~ s/\A\r\n// or die "broken parsing in $s\n";
		}
		$s =~ s/\A\r\n// or die "broken end parsing in $s\n";
		$s = $rv;
	}
	$s;
}

use_ok $_ foreach @mods;
my $git_dir = tempdir(CLEANUP => 1);
my $psgi = "examples/repobrowse.psgi";
my $app;
ok(-f $psgi, 'psgi example for repobrowse.psgi found');
{
	is(system(qw(git init -q --bare), $git_dir), 0, 'created git directory');
	my @cmd = ('git', "--git-dir=$git_dir", 'fast-import', '--quiet');
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
	my $repo_config = "$git_dir/pi_repo_config";
	my $fh;
	ok((open $fh, '>', $repo_config and
		print $fh '[repo "test.git"]', "\n",
			"\t", "path = $git_dir", "\n" and
		close $fh), 'created repo config');
	local $ENV{PI_REPO_CONFIG} = $repo_config;
	ok($app = require $psgi, 'loaded PSGI app');
}

# return value
bless {
	psgi => $psgi,
	git_dir => $git_dir,
	app => $app,
}, 'Repobrowse::TestGit';
