# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# This makes select(2) look like epoll to simplify the code in DS.pm.
# Unlike IO::Select, it does NOT hold references to IO handles.
# This is NOT meant to be an all encompassing emulation of epoll
# via select, but only to support cases we care about.
package PublicInbox::Select;
use v5.12;
use PublicInbox::Syscall qw(EPOLLONESHOT EPOLLIN EPOLLOUT);

sub new { bless {}, __PACKAGE__ } # fd => events

sub ep_wait {
	my ($self, $msec, $events) = @_;
	my ($rvec, $wvec) = ('', ''); # we don't use EPOLLERR
	while (my ($fd, $ev) = each %$self) {
		vec($rvec, $fd, 1) = 1 if $ev & EPOLLIN;
		vec($wvec, $fd, 1) = 1 if $ev & EPOLLOUT;
	}
	@$events = ();
	my $n = select($rvec, $wvec, undef, $msec < 0 ? undef : ($msec/1000));
	return if $n == 0;
	die "select: $!" if $n < 0;
	while (my ($fd, $ev) = each %$self) {
		if (vec($rvec, $fd, 1) || vec($wvec, $fd, 1)) {
			delete($self->{$fd}) if $ev & EPOLLONESHOT;
			push @$events, $fd;
		}
	}
	$n == scalar(@$events) or
		warn "BUG? select() returned $n, but got ".scalar(@$events);
}

sub ep_del { delete($_[0]->{fileno($_[1])}); 0 }
sub ep_add { $_[0]->{fileno($_[1])} = $_[2]; 0 }

no warnings 'once';
*ep_mod = \&ep_add;

1;
