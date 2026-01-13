# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Implements most Linux::Inotify2 functionality we need in pure Perl
# Anonymous sub support isn't supported since it's expensive in the
# best case and likely leaky in some Perls (e.g. 5.16.3, 5.40.x)
package PublicInbox::Inotify3;
use v5.12;
use autodie qw(open);
use PublicInbox::Syscall ();
use Carp;
use Scalar::Util ();

# this fails if undefined no unsupported platforms
use constant $PublicInbox::Syscall::INOTIFY;
our %events;

BEGIN {
# extracted from devel/sysdefs-list output, these should be arch-independent
# for Linux and also FreeBSD 15+
%events = (
	IN_ACCESS => 0x1,
	IN_ALL_EVENTS => 0xfff,
	IN_ATTRIB => 0x4,
	IN_CLOSE => 0x18,
	IN_CLOSE_NOWRITE => 0x10,
	IN_CLOSE_WRITE => 0x8,
	IN_CREATE => 0x100,
	IN_DELETE => 0x200,
	IN_DELETE_SELF => 0x400,
	IN_DONT_FOLLOW => 0x2000000,
	IN_EXCL_UNLINK => 0x4000000,
	IN_IGNORED => 0x8000,
	IN_ISDIR => 0x40000000,
	IN_MASK_ADD => 0x20000000,
	IN_MODIFY => 0x2,
	IN_MOVE => 0xc0,
	IN_MOVED_FROM => 0x40,
	IN_MOVED_TO => 0x80,
	IN_MOVE_SELF => 0x800,
	IN_ONESHOT => 0x80000000,
	IN_ONLYDIR => 0x1000000,
	IN_OPEN => 0x20,
	IN_Q_OVERFLOW => 0x4000,
	IN_UNMOUNT => 0x2000,
);
} # /BEGIN
use constant \%events;
require PublicInbox::In3Event; # uses %events
require PublicInbox::In3Watch; # uses SYS_inotify_rm_watch

use constant autocancel =>
	(IN_IGNORED|IN_UNMOUNT|IN_ONESHOT|IN_DELETE_SELF);

if (defined $PublicInbox::Syscall::INOTIFY->{SYS_inotify_init1}) {
	eval <<'EOS' or die $@;

sub new {
	open my $fh, "+<&=", syscall SYS_inotify_init1, IN_CLOEXEC;
	bless { fh => $fh }, __PACKAGE__;
}

sub inotify_add_watch ($$$) { syscall SYS_inotify_add_watch, @_ }
1;
EOS
} elsif (defined $PublicInbox::Syscall::INOTIFY->{SYS___specialfd}) {
	eval <<'EOS' or die $@;
sub new {
	my $args = pack "L", IN_CLOEXEC; # struct specialfd_inotify
	open my $fh, "+<&=", syscall SYS___specialfd, SPECIALFD_INOTIFY,
					$args, length $args;
	bless { fh => $fh }, __PACKAGE__;
}
sub inotify_add_watch ($$$) {
	syscall SYS_inotify_add_watch_at, $_[0], AT_FDCWD, $_[1], $_[2];
}
1;
EOS
}
sub read {
	my ($self) = @_;
	my (@ret, $wd, $mask, $len, $name, $size, $buf);
	my $r = sysread($self->{fh}, my $rbuf, 8192);
	if ($r) {
		while ($r) {
			($wd, $mask, undef, $len) = unpack('lLLL', $rbuf);
			$size = 16 + $len; # 16: sizeof(struct inotify_event)
			substr($rbuf, 0, 16, '');
			$name = $len ? unpack('Z*', substr($rbuf, 0, $len, ''))
					: undef;
			$r -= $size;
			next if $self->{ignore}->{$wd};
			my $ev = bless [$mask, $name], 'PublicInbox::In3Event';
			push @ret, $ev;
			if (my $w = $self->{w}->{$wd}) {
				$ev->[2] = $w;
				$w->cancel if $ev->mask & autocancel;
			} elsif ($mask & IN_Q_OVERFLOW) {
				carp 'E: IN_Q_OVERFLOW, too busy? (non-fatal)'
			} else {
				carp "BUG? wd:$wd unknown (non-fatal)";
			}
		}
	} elsif (defined($r) || ($!{EAGAIN} || $!{EINTR})) {
	} else {
		croak "inotify read: $!";
	}
	delete $self->{ignore};
	@ret;
}

sub fileno { CORE::fileno($_[0]->{fh}) }

sub fh { $_[0]->{fh} }

sub blocking { shift->{fh}->blocking(@_) }

sub watch {
	my ($self, $name, $mask, $cb) = @_;
	croak "E: $cb not supported" if $cb; # too much memory
	my $wd = inotify_add_watch($self->fileno, $name, $mask);
	return if $wd < 0;
	my $w = bless [ $wd, $mask, $name, $self ], 'PublicInbox::In3Watch';
	$self->{w}->{$wd} = $w;
	Scalar::Util::weaken($w->[3]); # ugh
	$w;
}

sub rm_watch {
	my ($self, $wd) = @_;
	delete $self->{w}->{$wd};
	$self->{ignore}->{$wd} = 1; # is this needed?
	syscall(SYS_inotify_rm_watch, $self->fileno, $wd) < 0 ? undef : 1;
}

1;
