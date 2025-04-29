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
package PublicInbox::DSKQXS;
use v5.12;
use Symbol qw(gensym);
use IO::KQueue;
use Errno qw(EAGAIN);
use PublicInbox::Syscall qw(EPOLLONESHOT EPOLLIN EPOLLOUT EPOLLET %SIGNUM);
use POSIX ();

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
	my $fgen = $PublicInbox::OnDestroy::fork_gen;
	bless { kq => IO::KQueue->new, fgen => $fgen }, $class;
}

sub _ep_mod_add ($$$$) {
	my ($kq, $fd, $ev, $add) = @_;
	$kq->EV_SET($fd, EVFILT_READ, $add|kq_flag(EPOLLIN, $ev));

	# we call this blindly for read-only FDs
	eval { $kq->EV_SET($fd, EVFILT_WRITE, $add|kq_flag(EPOLLOUT, $ev)) };
	0;
}

sub ep_add { _ep_mod_add($_[0]->{kq}, fileno($_[1]), $_[2], EV_ADD) };
sub ep_mod { _ep_mod_add($_[0]->{kq}, fileno($_[1]), $_[2], 0) };

sub ep_del {
	my ($self, $io, $ev) = @_;
	my $kq = $_[0]->{kq} // return; # called in cleanup
	my $fd = fileno($io);
	eval { $kq->EV_SET($fd, EVFILT_READ, EV_DISABLE) };
	eval { $kq->EV_SET($fd, EVFILT_WRITE, EV_DISABLE) };
	0;
}

# there's nothing like the sigmask arg for pselect/ppoll/epoll_pwait, we
# use EVFILT_SIGNAL to allow certain signals to wake us up from kevent
# but let Perl invoke %SIG handlers (see $peek_sigs in ep_wait)
sub prepare_signals {
	my ($self, $sig, $sigset) = @_; # $sig => \%SIG like hashmap
	my $kq = $self->{kq};
	for (keys %$sig) {
		my $num = $SIGNUM{$_} // POSIX->can("SIG$_")->();
		$kq->EV_SET($num, EVFILT_SIGNAL, EV_ADD);
	}
	# Unlike Linux signalfd, EVFILT_SIGNAL can't handle
	# signals received before the filter is created,
	# so we peek at signals here:
	my $restore = PublicInbox::DS::allow_sigs(keys %$sig);
	select undef, undef, undef, 0; # check sigs
	# $restore (on_destroy fires)
}

sub ep_wait {
	my ($self, $timeout_msec, $events, $sigmask) = @_;
	# n.b.: IO::KQueue is hard-coded to return up to 1000 events
	my $peek_sigs;
	@$events = eval { $self->{kq}->kevent($timeout_msec) };
	if (my $err = $@) {
		# workaround https://rt.cpan.org/Ticket/Display.html?id=116615
		if ($err =~ /Interrupted system call/) {
			$peek_sigs = 1;
			@$events = ();
		} else {
			die $err;
		}
	}
	# caller only cares for $events[$i]->[0]
	@$events = map {
		if ($_->[KQ_FILTER] == EVFILT_SIGNAL) {
			$peek_sigs = 1;
			()
		} else {
			$_->[0]
		}
	} @$events;
	if ($peek_sigs && $sigmask) {
		my $orig = POSIX::SigSet->new;
		PublicInbox::DS::sig_setmask($sigmask, $orig);
		select undef, undef, undef, 0; # Perl invokes %SIG handlers here
		PublicInbox::DS::sig_setmask($orig);
	}
	@$events;
}

# kqueue is close-on-fork (not exec), so we must not close it
# in forked processes:
sub DESTROY {
	my ($self) = @_;
	my $kq = delete $self->{kq} or return;
	delete($self->{fgen}) == $PublicInbox::OnDestroy::fork_gen and
		POSIX::close($$kq);
}

1;
