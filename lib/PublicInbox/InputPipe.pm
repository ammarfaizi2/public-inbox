# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# for reading pipes, sockets, and TTYs off the DS event loop
package PublicInbox::InputPipe;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN);

sub consume {
	my ($in, $cb, @args) = @_;
	my $self = bless { cb => $cb, args => \@args }, __PACKAGE__;
	eval { $self->SUPER::new($in, EPOLLIN) };
	if ($@) { # regular file (but not w/ select|IO::Poll backends)
		$self->{-need_rq} = 1;
		$self->requeue;
	} elsif (-p $in || -S _) { # O_NONBLOCK for sockets and pipes
		$in->blocking(0);
	}
	$self;
}

sub close { # idempotent
	my ($self) = @_;
	$self->{-need_rq} ? delete($self->{sock}) : $self->SUPER::close
}

sub event_step {
	my ($self) = @_;
	my $r = sysread($self->{sock} // return, my $rbuf, 65536);
	eval {
		if ($r) {
			$self->{cb}->($self, @{$self->{args}}, $rbuf);
			$self->requeue if $self->{-need_rq};
		} elsif (defined($r)) { # EOF
			$self->{cb}->($self, @{$self->{args}}, '');
			$self->close
		} elsif ($!{EAGAIN}) { # rely on EPOLLIN
		} elsif ($!{EINTR}) { # rely on EPOLLIN for sockets/pipes
			$self->requeue if $self->{-need_rq};
		} else { # another error
			$self->{cb}->($self, @{$self->{args}}, undef);
			$self->close;
		}
	};
	if ($@) {
		warn "E: $@";
		$self->close;
	}
}

1;
