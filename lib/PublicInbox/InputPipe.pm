# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# for reading pipes, sockets, and TTYs off the DS event loop
package PublicInbox::InputPipe;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN);
use POSIX ();
use Carp qw(croak carp);

# I'm not sure what I'm doing w.r.t terminals.
# FIXME needs non-interactive tests
sub unblock_tty ($) {
	my ($self) = @_;
	my $fd = fileno($self->{sock});
	my $t = POSIX::Termios->new;
	$t->getattr($fd) or croak("tcgetattr($fd): $!");
	return if $t->getlflag & POSIX::ICANON; # line-oriented, good

	# make noncanonical mode TTYs behave like a O_NONBLOCK pipe.
	# O_NONBLOCK itself isn't well-defined, here, so rely on VMIN + VTIME
	my ($vmin, $vtime) = ($t->getcc(POSIX::VMIN), $t->getcc(POSIX::VTIME));
	return if $vmin == 1 && $vtime == 0;

	$t->setcc(POSIX::VMIN, 1); # 1 byte minimum
	$t->setcc(POSIX::VTIME, 0); # no timeout
	$t->setattr($fd, POSIX::TCSANOW) or croak("tcsetattr($fd): $!");

	$t->setcc(POSIX::VMIN, $vmin);
	$t->setcc(POSIX::VTIME, $vtime);
	$self->{restore_termios} = $t;
}

sub consume {
	my ($in, $cb, @args) = @_;
	my $self = bless { cb => $cb, args => \@args }, __PACKAGE__;
	eval { $self->SUPER::new($in, EPOLLIN) };
	if ($@) { # regular file (but not w/ select|IO::Poll backends)
		$self->{-need_rq} = 1;
		$self->requeue;
	} elsif (do { no warnings 'unopened'; !stat($in) }) { # ProcessIONBF
	} elsif (-p _ || -S _) { # O_NONBLOCK for sockets and pipes
		$in->blocking(0);
	} elsif (-t $in) { # isatty(3) can't use `_' stat cache
		unblock_tty($self);
	}
	$self;
}

sub close { # idempotent
	my ($self) = @_;
	if (my $t = delete($self->{restore_termios})) {
		my $fd = fileno($self->{sock} // return);
		$t->setattr($fd, POSIX::TCSANOW) or carp("tcsetattr($fd): $!")
	}
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
		} elsif ($!{EINTR}) { # rely on EPOLLIN for sockets/pipes/tty
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
