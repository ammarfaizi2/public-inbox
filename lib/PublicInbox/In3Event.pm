# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# duck-type compatible with Linux::Inotify2::Event for pure Perl
# PublicInbox::Inotify3 w/o callback support
package PublicInbox::In3Event;
use v5.12;

sub w { $_[0]->[2] } # PublicInbox::In3Watch
sub mask { $_[0]->[0] }
sub name { $_[0]->[1] }

sub fullname {
	my ($name, $wname) = ($_[0]->[1], $_[0]->[2]->name);
	length($name) ? "$wname/$name" : $wname;
}

my $buf = '';
while (my ($sym, $mask) = each %PublicInbox::Inotify3::events) {
	$buf .= "sub $sym { \$_[0]->[0] & $mask }\n";
}
eval $buf;

1;
