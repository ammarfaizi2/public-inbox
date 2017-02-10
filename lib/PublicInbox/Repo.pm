# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Represents a code repository, analoguos to the PublicInbox::Inbox
# class for represpenting an inbox git repository.
package PublicInbox::Repo;
use strict;
use warnings;

sub new {
	my ($class, $opts) = @_;
	bless $opts, $class;
}

1;
