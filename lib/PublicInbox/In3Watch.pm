# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# duck-type compatible with Linux::Inotify2::Watch for pure Perl
# PublicInbox::Inotify3 for our needs, only
package PublicInbox::In3Watch;
use v5.12;

sub mask { $_[0]->[1] }
sub name { $_[0]->[2] }

sub cancel {
	my ($self) = @_;
	my ($wd, $in3) = @$self[0, 3];
	$in3 or return 1; # already canceled
	pop @$self;
	$in3->rm_watch($wd);
}

1;
