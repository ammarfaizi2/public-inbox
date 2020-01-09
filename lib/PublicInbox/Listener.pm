# Copyright (C) 2015-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Used by -nntpd for listen sockets
package PublicInbox::Listener;
use strict;
use warnings;
use base 'PublicInbox::DS';
use Socket qw(SOL_SOCKET SO_KEEPALIVE IPPROTO_TCP TCP_NODELAY);
use fields qw(post_accept);
use IO::Handle;
use PublicInbox::Syscall qw(EPOLLIN EPOLLEXCLUSIVE EPOLLET);
use Errno qw(EAGAIN ECONNABORTED EPERM);

# Warn on transient errors, mostly resource limitations.
# EINTR would indicate the failure to set NonBlocking in systemd or similar
my %ERR_WARN = map {;
	eval("Errno::$_()") => $_
} qw(EMFILE ENFILE ENOBUFS ENOMEM EINTR);

sub new ($$$) {
	my ($class, $s, $cb) = @_;
	setsockopt($s, SOL_SOCKET, SO_KEEPALIVE, 1);
	setsockopt($s, IPPROTO_TCP, TCP_NODELAY, 1); # ignore errors on non-TCP
	listen($s, 1024);
	my $self = fields::new($class);
	$self->SUPER::new($s, EPOLLIN|EPOLLET|EPOLLEXCLUSIVE);
	$self->{post_accept} = $cb;
	$self
}

sub event_step {
	my ($self) = @_;
	my $sock = $self->{sock} or return;

	# no loop here, we want to fairly distribute clients
	# between multiple processes sharing the same socket
	# XXX our event loop needs better granularity for
	# a single accept() here to be, umm..., acceptable
	# on high-traffic sites.
	if (my $addr = accept(my $c, $sock)) {
		IO::Handle::blocking($c, 0); # no accept4 :<
		eval { $self->{post_accept}->($c, $addr, $sock) };
		warn "E: $@\n" if $@;
		$self->requeue;
	} elsif ($! == EAGAIN || $! == ECONNABORTED || $! == EPERM) {
		# EAGAIN is common and likely
		# ECONNABORTED is common with bad connections
		# EPERM happens if firewall rules prevent a connection
		# on Linux (and everything that emulates Linux).
		# Firewall rules are sometimes intentional, so we don't
		# warn on EPERM to avoid being too noisy...
		return;
	} elsif (my $sym = $ERR_WARN{int($!)}) {
		warn "W: accept(): $! ($sym)\n";
	} else {
		warn "BUG?: accept(): $!\n";
	}
}

1;
