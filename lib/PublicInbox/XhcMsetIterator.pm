# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# mocks Xapian::MsetIterator, there's many of these allocated at once
package PublicInbox::XhcMsetIterator;
use v5.12;

sub make ($) {
	chomp($_[0]);
	my @self = map { $_ + 0 } split /\0/, $_[0]; # docid, pct, rank
	# we don't store $xdb in self[4] since we avoid $it->get_document
	# in favor of $xdb->get_document($it->get_docid)
	bless \@self, __PACKAGE__;
}

sub get_docid { $_[0]->[0] }
sub get_percent { $_[0]->[1] }
sub get_rank { $_[0]->[2] }

1;
