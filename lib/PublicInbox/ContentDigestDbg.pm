# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::ContentDigestDbg; # cf. PublicInbox::ContentDigest
use v5.12;
use Data::Dumper;
use PublicInbox::SHA;
$Data::Dumper::Useqq = $Data::Dumper::Terse = 1;

sub new { bless [ PublicInbox::SHA->new(256), $_[1] ], __PACKAGE__ }

sub add {
	$_[0]->[0]->add($_[1]);
	my @dbg = split(/^/sm, $_[1]);
	if (@dbg && $dbg[0] =~ /\A(To|Cc)\0/) { # fold excessively long lines
		@dbg = map { split(/,/s, $_) } @dbg;
	}
	print { $_[0]->[1] } Dumper(\@dbg) or die "print $!";
}

sub hexdigest { $_[0]->[0]->hexdigest }

1;
