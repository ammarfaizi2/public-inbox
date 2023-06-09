# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# compatibility code for old Perl and standard modules, mainly
# List::Util but maybe other stuff
package PublicInbox::Compat;
use v5.12;
use parent qw(Exporter);
require List::Util;

our @EXPORT_OK = qw(uniqstr);

# uniqstr is in List::Util 1.45+, which means Perl 5.26+;
# so maybe 2030 for us since we need to support enterprise distros.
# We can use uniqstr everywhere in our codebase and don't need
# to account for special cases of `uniqnum' nor `uniq' in List::Util
# even if they make more sense in some contexts
no warnings 'once';
*uniqstr = List::Util->can('uniqstr') // sub (@) {
	my %seen;
	grep { !$seen{$_}++ } @_;
};

1;
