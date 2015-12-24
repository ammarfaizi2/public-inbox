# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /commit/ endpoint for git repositories
package PublicInbox::RepoBrowseGitPatch;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);
use PublicInbox::Git;

sub call_git_patch {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $dir = $repo_info->{path};
	my $git = $repo_info->{git} ||= PublicInbox::Git->new($dir);

	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';
	my $fp = $git->popen(qw(format-patch -M -1 --stdout --encoding=utf-8
				--signature=public-inbox.org), $id);
	my ($buf, $n);

	$n = read($fp, $buf, 8192);
	return unless (defined $n && $n > 0);
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type'=>'text/plain']]);
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
