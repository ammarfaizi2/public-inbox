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

sub new { bless { poll => IO::Poll->new }, __PACKAGE__ } # fd => events

sub ep_wait {
	my ($self, $maxevents, $timeout_msec, $events) = @_;
	$self->{poll}->poll($timeout_msec/1000) > 0 or return (@$events = ());
	my @io = $self->{poll}->handles(POLLIN|POLLOUT);
	@$events = map { fileno($_) } @io;
	for (@$events) {
		my $io = shift @io;
		$self->{poll}->remove($io) if delete($self->{oneshot}->{$_});
	}
}

sub ep_del {
	my ($self, $io) = @_;
	delete $self->{oneshot}->{fileno($io)};
	$self->{poll}->remove($io);
	0;
}

sub ep_add {
	my ($self, $io, $ev) = @_;
	$self->{oneshot}->{fileno($io)} = 1 if $ev & EPOLLONESHOT;
	$self->{poll}->mask($io, ($ev & EPOLLIN ? POLLIN : 0) |
				($ev & EPOLLOUT ? POLLOUT : 0));
	0;
}

no warnings 'once';
*ep_mod = \&ep_add;

1;
