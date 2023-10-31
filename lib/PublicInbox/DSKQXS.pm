# Copyright (C) all contributors <meta@public-inbox.org>
# Licensed the same as Danga::Socket (and Perl5)
# License: GPL-1.0+ or Artistic-1.0-Perl
#  <https://www.gnu.org/licenses/gpl-1.0.txt>
#  <https://dev.perl.org/licenses/artistic.html>
#
# kqueue support via IO::KQueue XS module.  This makes kqueue look
# like epoll to simplify the code in DS.pm.  This is NOT meant to be
# an all encompassing emulation of epoll via IO::KQueue, but just to
# support cases public-inbox-nntpd/httpd care about.
#
# It also implements signalfd(2) emulation via "tie".
package PublicInbox::DSKQXS;
use v5.12;
use Symbol qw(gensym);
use IO::KQueue;
use Errno qw(EAGAIN);
use PublicInbox::Syscall qw(EPOLLONESHOT EPOLLIN EPOLLOUT EPOLLET);

sub EV_DISPATCH () { 0x0080 }

# map EPOLL* bits to kqueue EV_* flags for EV_SET
sub kq_flag ($$) {
	my ($bit, $ev) = @_;
	if ($ev & $bit) {
		my $fl = EV_ENABLE;
		$fl |= EV_CLEAR if $fl & EPOLLET;

		# EV_DISPATCH matches EPOLLONESHOT semantics more closely
		# than EV_ONESHOT, in that EV_ADD is not required to
		# re-enable a disabled watch.
		($ev & EPOLLONESHOT) ? ($fl | EV_DISPATCH) : $fl;
	} else {
		EV_DISABLE;
	}
}

sub new {
	my ($class) = @_;
	bless { kq => IO::KQueue->new, owner_pid => $$ }, $class;
}

# returns a new instance which behaves like signalfd on Linux.
# It's wasteful in that it uses another FD, but it simplifies
# our epoll-oriented code.
sub signalfd {
	my ($class, $signo) = @_;
	my $sym = gensym;
	tie *$sym, $class, $signo; # calls TIEHANDLE
	$sym
}

sub TIEHANDLE { # similar to signalfd()
	my ($class, $signo) = @_;
	my $self = $class->new;
	my $kq = $self->{kq};
	$kq->EV_SET($_, EVFILT_SIGNAL, EV_ADD) for @$signo;
	$self;
}

sub READ { # called by sysread() for signalfd compatibility
	my ($self, undef, $len, $off) = @_; # $_[1] = buf
	die "bad args for signalfd read" if ($len % 128) // defined($off);
	my $sigbuf = $self->{sigbuf} //= [];
	my $nr = $len / 128;
	my $r = 0;
	$_[1] = '';
	while (1) {
		while ($nr--) {
			my $signo = shift(@$sigbuf) or last;
			# caller only cares about signalfd_siginfo.ssi_signo:
			$_[1] .= pack('L', $signo) . ("\0" x 124);
			$r += 128;
		}
		return $r if $r;
		my @events = eval { $self->{kq}->kevent(0) };
		# workaround https://rt.cpan.org/Ticket/Display.html?id=116615
		if ($@) {
			next if $@ =~ /Interrupted system call/;
			die;
		}
		if (!scalar(@events)) {
			$! = EAGAIN;
			return;
		}

		# Grab the kevent.ident (signal number).  The kevent.data
		# field shows coalesced signals, and maybe we'll use it
		# in the future...
		@$sigbuf = map { $_->[0] } @events;
	}
}

# for fileno() calls in PublicInbox::DS
sub FILENO { ${$_[0]->{kq}} }

sub _ep_mod_add ($$$$) {
	my ($kq, $fd, $ev, $add) = @_;
	$kq->EV_SET($fd, EVFILT_READ, $add|kq_flag(EPOLLIN, $ev));

	# we call this blindly for read-only FDs such as tied
	# DSKQXS (signalfd emulation) and Listeners
	eval { $kq->EV_SET($fd, EVFILT_WRITE, $add|kq_flag(EPOLLOUT, $ev)) };
	0;
}

sub ep_add { _ep_mod_add($_[0]->{kq}, fileno($_[1]), $_[2], EV_ADD) };
sub ep_mod { _ep_mod_add($_[0]->{kq}, fileno($_[1]), $_[2], 0) };

sub ep_del {
	my ($self, $io, $ev) = @_;
	my $kq = $_[0]->{kq} // return; # called in cleanup
	my $fd = fileno($io);
	$kq->EV_SET($fd, EVFILT_READ, EV_DISABLE);
	eval { $kq->EV_SET($fd, EVFILT_WRITE, EV_DISABLE) };
	0;
}

sub ep_wait {
	my ($self, $timeout_msec, $events) = @_;
	# n.b.: IO::KQueue is hard-coded to return up to 1000 events
	@$events = eval { $self->{kq}->kevent($timeout_msec) };
	if (my $err = $@) {
		# workaround https://rt.cpan.org/Ticket/Display.html?id=116615
		if ($err =~ /Interrupted system call/) {
			@$events = ();
		} else {
			die $err;
		}
	}
	# caller only cares for $events[$i]->[0]
	$_ = $_->[0] for @$events;
}

# kqueue is close-on-fork (not exec), so we must not close it
# in forked processes:
sub DESTROY {
	my ($self) = @_;
	my $kq = delete $self->{kq} or return;
	if (delete($self->{owner_pid}) == $$) {
		POSIX::close($$kq);
	}
}

1;
