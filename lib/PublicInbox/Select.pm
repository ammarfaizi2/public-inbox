# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# This makes select(2) look like epoll to simplify the code in DS.pm.
# Unlike IO::Select, it does NOT hold references to IO handles.
# This is NOT meant to be an all encompassing emulation of epoll
# via select, but only to support cases we care about.
package PublicInbox::Select;
use v5.12;
use PublicInbox::Syscall qw(EPOLLONESHOT EPOLLIN EPOLLOUT %SIGNUM);
use Errno;
use POSIX ();

sub new { bless {}, __PACKAGE__ } # fd => events

sub ep_wait {
	my ($self, $msec, $events) = @_;
	my ($rvec, $wvec) = ('', ''); # we don't use EPOLLERR
	while (my ($fd, $ev) = each %$self) {
		vec($rvec, $fd, 1) = 1 if $ev & EPOLLIN;
		vec($wvec, $fd, 1) = 1 if $ev & EPOLLOUT;
	}
	@$events = ();
	# no pselect(2), wake up every 1s to let Perl dispatch %SIG handlers
	my $to = $msec < 0 ? 1 : ($msec/1000);
	$to = 1 if $to > 1;
	my $n = select $rvec, $wvec, undef, $to or return; # timeout expired
	return if $n < 0 && $! == Errno::EINTR; # caller recalculates timeout
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

sub prepare_signals {
	my ($self, $sig, $sigset) = @_;
	for (keys %$sig) { # like %SIG
		my $num = $SIGNUM{$_} // POSIX->can("SIG$_")->();
		$sigset->delset($num);
	}
	PublicInbox::DS::sig_setmask($sigset);
}

no warnings 'once';
*ep_mod = \&ep_add;

1;
