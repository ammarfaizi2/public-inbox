# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::EOFpipe;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT $F_SETPIPE_SZ);

sub new {
	my (undef, $rd, @cb_args) = @_;
	my $self = bless { cb_args => \@cb_args }, __PACKAGE__;
	# 4096: page size
	fcntl($rd, $F_SETPIPE_SZ, 4096) if $F_SETPIPE_SZ;
	$self->SUPER::new($rd, EPOLLIN|EPOLLONESHOT);
}

sub event_step {
	my ($self) = @_;
	if ($self->do_read(my $buf, 1) == 0) { # auto-closed
		my ($cb, @args) = @{delete $self->{cb_args}};
		$cb->(@args);
	}
}

1;
