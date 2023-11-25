# Copyright (C) all contributors <meta@public-inbox.org>
# Licensed the same as Danga::Socket (and Perl5)
# License: GPL-1.0+ or Artistic-1.0-Perl
#  <https://www.gnu.org/licenses/gpl-1.0.txt>
#  <https://dev.perl.org/licenses/artistic.html>
#
# poll(2) via IO::Poll core module.  This makes poll look
# like epoll to simplify the code in DS.pm.  This is NOT meant to be
# an all encompassing emulation of epoll via IO::Poll, but just to
# support cases public-inbox-nntpd/httpd care about.
package PublicInbox::DSPoll;
use v5.12;
use IO::Poll;
use PublicInbox::Syscall qw(EPOLLONESHOT EPOLLIN EPOLLOUT);
use Carp qw(carp);
use Errno ();

sub new { bless {}, __PACKAGE__ } # fd => events

sub ep_wait {
	my ($self, $timeout_msec, $events) = @_;
	my (@pset, $n, $fd, $revents, $nval);
	while (my ($fd, $events) = each %$self) {
		my $pevents = $events & EPOLLIN ? POLLIN : 0;
		$pevents |= $events & EPOLLOUT ? POLLOUT : 0;
		push(@pset, $fd, $pevents);
	}
	@$events = ();
	$n = IO::Poll::_poll($timeout_msec, @pset) or return; # timeout expired
	return if $n < 0 && $! == Errno::EINTR; # caller recalculates timeout
	die "poll: $!" if $n < 0;
	while (defined($fd = shift @pset)) {
		$revents = shift @pset or next; # no event
		if ($revents & POLLNVAL) {
			carp "E: FD=$fd invalid in poll";
			delete $self->{$fd};
			$nval = 1;
		} else {
			delete $self->{$fd} if $self->{$fd} & EPOLLONESHOT;
			push @$events, $fd;
		}
	}
	if ($nval && !@$events) {
		$! = Errno::EBADF;
		die "poll: $!";
	}
}

sub ep_del { delete($_[0]->{fileno($_[1])}); 0 }
sub ep_add { $_[0]->{fileno($_[1])} = $_[2]; 0 }

no warnings 'once';
*ep_mod = \&ep_add;

1;
