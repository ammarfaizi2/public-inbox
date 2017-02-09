# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /patch/ endpoint for git repositories
# usage: /repo.git/patch?id=COMMIT_ID
package PublicInbox::RepoGitPatch;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Qspawn;

# try to be educational and show the command-line used in the signature
my @CMD = qw(format-patch -M --stdout);
my $sig = '--signature=git '.join(' ', @CMD);

sub call_git_patch {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $env = $req->{env};
	my $q = PublicInbox::RepoGitQuery->new($env);
	my $id = $q->{id};
	$id =~ /\A[\w-]+([~\^][~\^\d])*\z/ or $id = 'HEAD';

	# limit scope, don't take extra args to avoid wasting server
	# resources buffering:
	my $range = "$id~1..$id^0";
	my $cmd = $git->cmd(@CMD, $sig." $range", $range, '--');
	my $expath = $req->{expath};
	push @$cmd, $expath if $expath ne '';

	my $qsp = PublicInbox::Qspawn->new($cmd);
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		my $h = ['Content-Type', 'text/plain; charset=UTF-8'];
		$r ? [ 200, $h ] : [ 500, $h, [ "format-patch error\n" ] ];
	});
}

1;
