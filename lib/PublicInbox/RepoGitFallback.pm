# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)

# when no endpoints match, fallback to this and serve a static file
# This can serve Smart HTTP in the future.
package PublicInbox::RepoGitFallback;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::GitHTTPBackend;

# overrides PublicInbox::RepoBase::call
sub call {
	my ($self, undef, $req) = @_;
	my $expath = $req->{expath};
	return if index($expath, '..') >= 0; # prevent path traversal
	my $git = $req->{repo_info}->{git};
	PublicInbox::GitHTTPBackend::serve($req->{env}, $git, $expath);
}

1;
