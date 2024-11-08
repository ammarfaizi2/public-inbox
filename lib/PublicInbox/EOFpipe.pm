# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::EOFpipe;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN $F_SETPIPE_SZ);
use Errno qw(EAGAIN);

sub new {
	my (undef, $rd, @cb_args) = @_;
	my $self = bless { cb_args => \@cb_args }, __PACKAGE__;
	# 4096: page size
	fcntl($rd, $F_SETPIPE_SZ, 4096) if $F_SETPIPE_SZ;
	$rd->blocking(0); # avoid EINTR
	$self->SUPER::new($rd, EPOLLIN); # level trigger for spurious wakeup
}

sub event_step {
	my ($self) = @_;
	my $r = sysread $self->{sock}, my $buf, 1;
	if (!defined $r) {
		warn "W: EOFpipe read: $!\n" if $! != EAGAIN;
	} elsif ($r == 0) {
		$self->close;
		my ($cb, @args) = @{delete $self->{cb_args}};
		$cb->(@args);
	} else {
		warn "BUG? EOFpipe read $r bytes, expected 0\n";
	}
}

1;
