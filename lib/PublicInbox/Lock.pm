# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Base class for per-inbox locking, subclassed by several
# only uses {lock_path} and {lockfh} fields
package PublicInbox::Lock;
use v5.12;
use Fcntl qw(LOCK_UN LOCK_EX O_RDWR O_CREAT);
use Carp qw(croak);
use PublicInbox::OnDestroy;
use Errno qw(EINTR);
use autodie qw(close sysopen syswrite);

sub xflock ($$) {
	until (flock($_[0], $_[1])) { return if $! != EINTR }
	1;
}

sub new { bless { lock_path => $_[1] }, $_[0] }

# we only acquire the flock if creating or reindexing;
# PublicInbox::Import already has the lock on its own.
sub lock_acquire {
	my ($self) = @_;
	my $fn = $self->{lock_path};
	croak 'already locked '.($fn // '(undef)') if $self->{lockfh};
	$fn // return;
	sysopen(my $fh, $fn, O_RDWR|O_CREAT);
	xflock($fh, LOCK_EX) or croak "LOCK_EX $fn: $!";
	$self->{lockfh} = $fh;
}

sub lock_release {
	my ($self, $wake) = @_;
	my $fn = $self->{lock_path} // return;
	my $fh = delete $self->{lockfh} or croak "not locked: $fn";
	syswrite($fh, '.') if $wake;
	xflock($fh, LOCK_UN) or croak "LOCK_UN $fn: $!";
	close $fh; # may detect errors
}

# caller must use return value
sub lock_for_scope {
	my ($self, @single_pid) = @_;
	lock_acquire($self) or return; # lock_path not set
	PublicInbox::OnDestroy->new(@single_pid, \&lock_release, $self);
}

sub lock_acquire_fast {
	my $fh = $_[0]->{lockfh} or return lock_acquire($_[0]);
	xflock($fh, LOCK_EX) or croak "LOCK_EX $_[0]->{lock_path}: $!";
}

sub lock_release_fast {
	xflock($_[0]->{lockfh} // return, LOCK_UN) or
		croak "LOCK_UN $_[0]->{lock_path}: $!"
}

# caller must use return value
sub lock_for_scope_fast {
	my ($self, @single_pid) = @_;
	lock_acquire_fast($self) or return; # lock_path not set
	PublicInbox::OnDestroy->new(@single_pid, \&lock_release_fast, $self);
}

1;
