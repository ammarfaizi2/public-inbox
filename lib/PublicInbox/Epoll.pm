# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# OO API for epoll
package PublicInbox::Epoll;
use v5.12;
use PublicInbox::Syscall qw(epoll_create epoll_ctl epoll_pwait
	EPOLL_CTL_ADD EPOLL_CTL_MOD EPOLL_CTL_DEL %SIGNUM);
use Fcntl qw(F_SETFD FD_CLOEXEC);
use POSIX ();
use autodie qw(open fcntl);

sub new {
	open(my $fh, '+<&=', epoll_create());
	fcntl($fh, F_SETFD, FD_CLOEXEC);
	bless \$fh, __PACKAGE__;
}

sub ep_add { epoll_ctl(fileno(${$_[0]}), EPOLL_CTL_ADD, fileno($_[1]), $_[2]) }
sub ep_mod { epoll_ctl(fileno(${$_[0]}), EPOLL_CTL_MOD, fileno($_[1]), $_[2]) }
sub ep_del { epoll_ctl(fileno(${$_[0]}), EPOLL_CTL_DEL, fileno($_[1]), 0) }

# n.b. maxevents=1000 is the historical default.  maxevents=1 (yes, one)
# is more fair under load with multiple worker processes sharing one listener
# ($self, \@events, $timeout, $sigset) = @_;
sub ep_wait { epoll_pwait(fileno(${$_[0]}), 1000, @_[1..3]) }

# prepare sigset for epoll_pwait
sub prepare_signals {
	my (undef, $sig, $sigset) = @_;
	for (keys %$sig) { # like %SIG
		my $num = $SIGNUM{$_} // POSIX->can("SIG$_")->();
		$sigset->delset($num);
	}
}

1;
