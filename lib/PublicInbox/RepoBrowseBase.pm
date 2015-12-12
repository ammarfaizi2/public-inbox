# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoBrowseBase;
use strict;
use warnings;
require PublicInbox::RepoBrowseQuery;
require PublicInbox::Hval;

sub new { bless {}, shift }

sub call {
	my ($self, $req) = @_;
	my $vcs = $req->{repo_info}->{vcs};
	my $rv = eval {
		no strict 'refs';
		my $sub = 'call_'.$vcs;
		$self->$sub($req);
	};
	$@ ? [ 500, ['Content-Type'=>'text/plain'], [] ] : $rv;
}

1;
