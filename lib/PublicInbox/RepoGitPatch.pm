# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /patch/ endpoint for git repositories
# usage: /repo.git/patch/COMMIT_ID
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
	my $repo = $req->{-repo};
	my $git = $repo->{git};
	my $env = $req->{env};
	my $tip = $repo->tip;
	$tip =~ /\A[\w-]+([~\^][~\^\d])*\z/;

	# limit scope, don't take extra args to avoid wasting server
	# resources buffering:
	my $range = "$tip~1..$tip^0";
	my $cmd = $git->cmd(@CMD, $sig." $range", $range, '--');
	my $expath = $req->{expath};
	push @$cmd, $expath if $expath ne '';

	my $qsp = PublicInbox::Qspawn->new($cmd);
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		$r ? $self->rt(200, 'plain') :
			$self->rt(500, 'plain', "format-patch error\n");
	});
}

1;
