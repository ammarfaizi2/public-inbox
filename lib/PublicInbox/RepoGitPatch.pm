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

sub fmt_patch ($$$) {
	my ($self, $req, $res) = @_;
	my $git = $req->{-repo}->{git};
	my $tip = $req->{tip};
	my $env = $req->{env};

	# limit scope, don't take extra args to avoid wasting server
	# resources buffering:
	my $range = "$tip~1..$tip^0";
	my $cmd = $git->cmd(@CMD, $sig." $range", $range, '--');
	my $expath = $req->{expath};
	push @$cmd, $expath if $expath ne '';
	$env->{'qspawn.response'} = $res;

	my $qsp = PublicInbox::Qspawn->new($cmd);
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		$r ? $self->rt(200, 'plain') :
			$self->rt(500, 'plain', "format-patch error\n");
	});
}

sub call_git_patch {
	my ($self, $req) = @_;
	sub {
		my ($res) = @_;
		my $repo = $req->{-repo};
		my $tip = $req->{tip};
		my $obj = $tip || $repo->tip;
		$repo->{git}->check_async($req->{env}, $obj.'^{commit}', sub {
			my ($info) = @_;
			my ($hex, $type, undef) = @$info;
			if (!defined $type || $type ne 'commit') {
				return $res->($self->rt(400, 'plain',
						"$obj is not a commit\n"));
			}
			return fmt_patch($self, $req, $res) if $obj eq $hex;
			$res->($self->r(302, $req, $tip ? "../$hex" : $hex));
		});
	}
}

1;
