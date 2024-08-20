# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Wraps a signalfd (or similar) for PublicInbox::DS
# fields: (sig: hashref similar to %SIG, but signal numbers as keys)
package PublicInbox::Sigfd;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(signalfd EPOLLIN EPOLLET %SIGNUM);
use POSIX ();
use autodie qw(kill open);
my @num2name;

# returns a coderef to unblock signals if neither signalfd or kqueue
# are available.
sub new {
	my ($class, $sig) = @_;
	my @signo;
	for my $name (keys %$sig) {
		my $num = $SIGNUM{$name} // POSIX->can("SIG$name")->();
		push @signo, $num;
		$num2name[$num] //= $name;
	}
	my $self = bless {}, $class;
	my $io;
	my $fd = signalfd(\@signo);
	if (defined $fd && $fd >= 0) {
		open $io, '+<&=', $fd;
	} elsif (eval { require PublicInbox::DSKQXS }) {
		$io = PublicInbox::DSKQXS->signalfd(\@signo);
		$self->{kq_sigs} = [ keys %$sig ];
	} else {
		return; # wake up every second to check for signals
	}
	$self->SUPER::new($io, EPOLLIN | EPOLLET);
	$self;
}

# PublicInbox::Daemon in master main loop (blocking)
sub wait_once ($) {
	my ($self) = @_;
	# 128 == sizeof(struct signalfd_siginfo)
	my $r = sysread($self->{sock}, my $buf, 128 * 64);
	if ($self->{kq_sigs}) {
		# kqueue doesn't consume signals the same way signalfd does,
		# so the OS + Perl can make calls for us:
		my $restore = PublicInbox::DS::allow_sigs @{$self->{kq_sigs}};
		select undef, undef, undef, 0; # checks signals
	} elsif (defined($r)) { # Linux signalfd
		my $nr = $r / 128 - 1; # $nr may be -1
		for my $off (0..$nr) {
			# the first uint32_t of signalfd_siginfo: ssi_signo
			my $num = unpack('L', substr($buf, 128 * $off, 4));
			my $name = $num2name[$num];
			my $cb = $SIG{$name} || 'IGNORE';
			if ($cb eq 'DEFAULT') {
				my $restore = PublicInbox::DS::allow_sigs $name;
				kill $name, $$;
				select undef, undef, undef, 0; # checks signals
				# $restore fires
			} elsif (ref $cb) {
				$cb->($name);
			} # undef
		}
	}
	$r;
}

# called by PublicInbox::DS in epoll_wait loop
sub event_step {
	while (wait_once($_[0])) {} # non-blocking
}

1;
