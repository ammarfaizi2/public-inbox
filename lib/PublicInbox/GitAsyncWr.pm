# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# internal class used by PublicInbox::Git + Danga::Socket
# This writes to the input pipe of "git cat-file --batch/--batch-check"
package PublicInbox::GitAsyncWr;
use strict;
use warnings;
use base qw(Danga::Socket);

sub new {
	my ($class, $io) = @_;
	my $self = fields::new($class);
	IO::Handle::blocking($io, 0);
	$self->SUPER::new($io);
}

# we only care about write + event_write

sub event_hup { $_[0]->close }
sub event_err { $_[0]->close }

1;
