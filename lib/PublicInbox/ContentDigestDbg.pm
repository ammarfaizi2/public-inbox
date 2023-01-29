# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::ContentDigestDbg; # cf. PublicInbox::ContentDigest
use v5.12;
use Data::Dumper;
use PublicInbox::SHA;

sub new { bless { dig => PublicInbox::SHA->new(256), fh => $_[1] }, __PACKAGE__ }

sub add {
	$_[0]->{dig}->add($_[1]);
	print { $_[0]->{fh} } Dumper([split(/^/sm, $_[1])]) or die "print $!";
}

sub hexdigest { $_[0]->{dig}->hexdigest; }

1;
