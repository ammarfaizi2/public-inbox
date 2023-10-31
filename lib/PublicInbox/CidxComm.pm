# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Waits for initial comm(1) output for PublicInbox::CodeSearchIdx.
# The initial output from `comm' can take a while to generate because
# it needs to wait on:
# `git cat-file --batch-all-objects --batch-check --unordered | sort'
# We still rely on blocking reads, here, since comm should be fast once
# it's seeing input.  (`--unordered | sort' is intentional for HDDs)
package PublicInbox::CidxComm;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT);

sub new {
	my ($cls, $rd, $cidx) = @_;
	my $self = bless { cidx => $cidx }, $cls;
	$self->SUPER::new($rd, EPOLLIN|EPOLLONESHOT);
}

sub event_step {
	my ($self) = @_;
	my $rd = $self->{sock} // return warn('BUG?: no {sock}');
	$self->close; # EPOLL_CTL_DEL
	delete($self->{cidx})->cidx_read_comm($rd);
}

1;
