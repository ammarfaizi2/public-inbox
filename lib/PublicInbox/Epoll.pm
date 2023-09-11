# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# OO API for epoll
package PublicInbox::Epoll;
use v5.12;
use PublicInbox::Syscall qw(epoll_create epoll_ctl epoll_wait
	EPOLL_CTL_ADD EPOLL_CTL_MOD EPOLL_CTL_DEL);
use Fcntl qw(F_SETFD FD_CLOEXEC);
use autodie qw(open fcntl);

sub new {
	open(my $fh, '+<&=', epoll_create());
	fcntl($fh, F_SETFD, FD_CLOEXEC);
	bless \$fh, __PACKAGE__;
}

sub ep_add { epoll_ctl(fileno(${$_[0]}), EPOLL_CTL_ADD, fileno($_[1]), $_[2]) }
sub ep_mod { epoll_ctl(fileno(${$_[0]}), EPOLL_CTL_MOD, fileno($_[1]), $_[2]) }
sub ep_del { epoll_ctl(fileno(${$_[0]}), EPOLL_CTL_DEL, fileno($_[1]), 0) }
sub ep_wait { epoll_wait(fileno(${$_[0]}), @_[1, 2, 3]) }

1;
