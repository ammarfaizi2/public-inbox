# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Waits for initial `git log -p' output for PublicInbox::CodeSearchIdx.
# The initial output from `git log -p' can take a while to generate,
# CodeSearchIdx can process prune work while it's happening.  Once
# `git log -p' starts generating output, it should be able to keep
# up with Xapian indexing, so we still rely on blocking reads to simplify
# cidx_read_log_p
package PublicInbox::CidxLogP;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT);

sub new {
	my ($cls, $rd, $cidx, $git, $roots) = @_;
	my $self = bless { cidx => $cidx, git => $git, roots => $roots }, $cls;
	$self->SUPER::new($rd, EPOLLIN|EPOLLONESHOT);
}

sub event_step {
	my ($self) = @_;
	my $rd = $self->{sock} // return warn('BUG?: no {sock}');
	$self->close; # EPOLL_CTL_DEL
	delete($self->{cidx})->cidx_read_log_p($self, $rd);
}

1;
