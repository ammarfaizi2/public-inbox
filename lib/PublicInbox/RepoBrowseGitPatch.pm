# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /patch/ endpoint for git repositories
# usage: /repo.git/patch?id=COMMIT_ID
package PublicInbox::RepoBrowseGitPatch;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);

# try to be educational and show the command-line used in the signature
my @CMD = qw(format-patch -M --stdout);
my $sig = '--signature=git '.join(' ', @CMD);

sub call_git_patch {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id =~ /\A[\w-]+([~\^][~\^\d])*\z/ or $id = 'HEAD';

	# limit scope, don't take extra args to avoid wasting server
	# resources buffering:
	my $range = "$id~1..$id^0";
	my @cmd = (@CMD, $sig." $range", $range, '--');
	if (defined(my $expath = $req->{expath})) {
		push @cmd, $expath;
	}
	my $fp = $git->popen(@cmd);
	my ($buf, $n);

	$n = read($fp, $buf, 8192);
	return unless (defined $n && $n > 0);
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type' => 'text/plain']]);
		$fh->write($buf);
		while (1) {
			$n = read($fp, $buf, 8192);
			last unless (defined $n && $n > 0);
			$fh->write($buf);
		}
		$fh->close;
	}
}

1;
