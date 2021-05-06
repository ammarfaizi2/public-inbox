# This is a fork of the (for now) unmaintained Sys::Syscall 0.25,
# specifically the Debian libsys-syscall-perl 0.25-6 version to
# fix upstream regressions in 0.25.
#
# This license differs from the rest of public-inbox
#
# This module is Copyright (c) 2005 Six Apart, Ltd.
# Copyright (C) 2019-2021 all contributors <meta@public-inbox.org>
#
# All rights reserved.
#
# You may distribute under the terms of either the GNU General Public
# License or the Artistic License, as specified in the Perl README file.
package PublicInbox::Syscall;
use strict;
use parent qw(Exporter);
use POSIX qw(ENOSYS O_NONBLOCK);
use Config;

# $VERSION = '0.25'; # Sys::Syscall version
our @EXPORT_OK = qw(epoll_ctl epoll_create epoll_wait
                  EPOLLIN EPOLLOUT EPOLLET
                  EPOLL_CTL_ADD EPOLL_CTL_DEL EPOLL_CTL_MOD
                  EPOLLONESHOT EPOLLEXCLUSIVE
                  signalfd SFD_NONBLOCK);
our %EXPORT_TAGS = (epoll => [qw(epoll_ctl epoll_create epoll_wait
                             EPOLLIN EPOLLOUT
                             EPOLL_CTL_ADD EPOLL_CTL_DEL EPOLL_CTL_MOD
                             EPOLLONESHOT EPOLLEXCLUSIVE)],
                );

use constant {
	EPOLLIN => 1,
	EPOLLOUT => 4,
	# EPOLLERR => 8,
	# EPOLLHUP => 16,
	# EPOLLRDBAND => 128,
	EPOLLEXCLUSIVE => (1 << 28),
	EPOLLONESHOT => (1 << 30),
	EPOLLET => (1 << 31),
	EPOLL_CTL_ADD => 1,
	EPOLL_CTL_DEL => 2,
	EPOLL_CTL_MOD => 3,
};

our $loaded_syscall = 0;

sub _load_syscall {
    # props to Gaal for this!
    return if $loaded_syscall++;
    my $clean = sub {
        delete @INC{qw<syscall.ph asm/unistd.ph bits/syscall.ph
                        _h2ph_pre.ph sys/syscall.ph>};
    };
    $clean->(); # don't trust modules before us
    my $rv = eval { require 'syscall.ph'; 1 } || eval { require 'sys/syscall.ph'; 1 };
    $clean->(); # don't require modules after us trust us
    $rv;
}


our (
     $SYS_epoll_create,
     $SYS_epoll_ctl,
     $SYS_epoll_wait,
     $SYS_signalfd4,
     );

my $SFD_CLOEXEC = 02000000; # Perl does not expose O_CLOEXEC
sub SFD_NONBLOCK () { O_NONBLOCK }
our $no_deprecated = 0;

if ($^O eq "linux") {
    my $machine = (POSIX::uname())[-1];
    # whether the machine requires 64-bit numbers to be on 8-byte
    # boundaries.
    my $u64_mod_8 = 0;

    # if we're running on an x86_64 kernel, but a 32-bit process,
    # we need to use the x32 or i386 syscall numbers.
    if ($machine eq "x86_64" && $Config{ptrsize} == 4) {
        $machine = $Config{cppsymbols} =~ /\b__ILP32__=1\b/ ? 'x32' : 'i386';
    }

    # Similarly for mips64 vs mips
    if ($machine eq "mips64" && $Config{ptrsize} == 4) {
        $machine = "mips";
    }

    if ($machine =~ m/^i[3456]86$/) {
        $SYS_epoll_create = 254;
        $SYS_epoll_ctl    = 255;
        $SYS_epoll_wait   = 256;
        $SYS_signalfd4 = 327;
    } elsif ($machine eq "x86_64") {
        $SYS_epoll_create = 213;
        $SYS_epoll_ctl    = 233;
        $SYS_epoll_wait   = 232;
        $SYS_signalfd4 = 289;
    } elsif ($machine eq 'x32') {
        $SYS_epoll_create = 1073742037;
        $SYS_epoll_ctl = 1073742057;
        $SYS_epoll_wait = 1073742056;
        $SYS_signalfd4 = 1073742113;
    } elsif ($machine eq 'sparc64') {
	$SYS_epoll_create = 193;
	$SYS_epoll_ctl = 194;
	$SYS_epoll_wait = 195;
	$u64_mod_8 = 1;
	$SYS_signalfd4 = 317;
	$SFD_CLOEXEC = 020000000;
    } elsif ($machine =~ m/^parisc/) {
        $SYS_epoll_create = 224;
        $SYS_epoll_ctl    = 225;
        $SYS_epoll_wait   = 226;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 309;
    } elsif ($machine =~ m/^ppc64/) {
        $SYS_epoll_create = 236;
        $SYS_epoll_ctl    = 237;
        $SYS_epoll_wait   = 238;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 313;
    } elsif ($machine eq "ppc") {
        $SYS_epoll_create = 236;
        $SYS_epoll_ctl    = 237;
        $SYS_epoll_wait   = 238;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 313;
    } elsif ($machine =~ m/^s390/) {
        $SYS_epoll_create = 249;
        $SYS_epoll_ctl    = 250;
        $SYS_epoll_wait   = 251;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 322;
    } elsif ($machine eq "ia64") {
        $SYS_epoll_create = 1243;
        $SYS_epoll_ctl    = 1244;
        $SYS_epoll_wait   = 1245;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 289;
    } elsif ($machine eq "alpha") {
        # natural alignment, ints are 32-bits
        $SYS_epoll_create = 407;
        $SYS_epoll_ctl    = 408;
        $SYS_epoll_wait   = 409;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 484;
	$SFD_CLOEXEC = 010000000;
    } elsif ($machine eq "aarch64") {
        $SYS_epoll_create = 20;  # (sys_epoll_create1)
        $SYS_epoll_ctl    = 21;
        $SYS_epoll_wait   = 22;  # (sys_epoll_pwait)
        $u64_mod_8        = 1;
        $no_deprecated    = 1;
        $SYS_signalfd4 = 74;
    } elsif ($machine =~ m/arm(v\d+)?.*l/) {
        # ARM OABI
        $SYS_epoll_create = 250;
        $SYS_epoll_ctl    = 251;
        $SYS_epoll_wait   = 252;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 355;
    } elsif ($machine =~ m/^mips64/) {
        $SYS_epoll_create = 5207;
        $SYS_epoll_ctl    = 5208;
        $SYS_epoll_wait   = 5209;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 5283;
    } elsif ($machine =~ m/^mips/) {
        $SYS_epoll_create = 4248;
        $SYS_epoll_ctl    = 4249;
        $SYS_epoll_wait   = 4250;
        $u64_mod_8        = 1;
        $SYS_signalfd4 = 4324;
    } else {
        # as a last resort, try using the *.ph files which may not
        # exist or may be wrong
        _load_syscall();
        $SYS_epoll_create = eval { &SYS_epoll_create; } || 0;
        $SYS_epoll_ctl    = eval { &SYS_epoll_ctl;    } || 0;
        $SYS_epoll_wait   = eval { &SYS_epoll_wait;   } || 0;

	# Note: do NOT add new syscalls to depend on *.ph, here.
	# Better to miss syscalls (so we can fallback to IO::Poll)
	# than to use wrong ones, since the names are not stable
	# (at least not on FreeBSD), if the actual numbers are.
    }

    if ($u64_mod_8) {
        *epoll_wait = \&epoll_wait_mod8;
        *epoll_ctl = \&epoll_ctl_mod8;
    } else {
        *epoll_wait = \&epoll_wait_mod4;
        *epoll_ctl = \&epoll_ctl_mod4;
    }
}
# use Inline::C for *BSD-only or general POSIX stuff.
# Linux guarantees stable syscall numbering, BSDs only offer a stable libc

############################################################################
# epoll functions
############################################################################

sub epoll_defined { $SYS_epoll_create ? 1 : 0; }

sub epoll_create {
	syscall($SYS_epoll_create, $no_deprecated ? 0 : 100);
}

# epoll_ctl wrapper
# ARGS: (epfd, op, fd, events_mask)
sub epoll_ctl_mod4 {
    syscall($SYS_epoll_ctl, $_[0]+0, $_[1]+0, $_[2]+0, pack("LLL", $_[3], $_[2], 0));
}
sub epoll_ctl_mod8 {
    syscall($SYS_epoll_ctl, $_[0]+0, $_[1]+0, $_[2]+0, pack("LLLL", $_[3], 0, $_[2], 0));
}

# epoll_wait wrapper
# ARGS: (epfd, maxevents, timeout (milliseconds), arrayref)
#  arrayref: values modified to be [$fd, $event]
our $epoll_wait_events = '';
our $epoll_wait_size = 0;
sub epoll_wait_mod4 {
	my ($epfd, $maxevents, $timeout_msec, $events) = @_;
	# resize our static buffer if maxevents bigger than we've ever done
	if ($maxevents > $epoll_wait_size) {
		$epoll_wait_size = $maxevents;
		vec($epoll_wait_events, $maxevents * 12 * 8 - 1, 1) = 0;
	}
	@$events = ();
	my $ct = syscall($SYS_epoll_wait, $epfd, $epoll_wait_events,
			$maxevents, $timeout_msec);
	for (0..$ct - 1) {
		# 12-byte struct epoll_event
		# 4 bytes uint32_t events mask (skipped, useless to us)
		# 8 bytes: epoll_data_t union (first 4 bytes are the fd)
		# So we skip the first 4 bytes and take the middle 4:
		$events->[$_] = unpack('L', substr($epoll_wait_events,
							12 * $_ + 4, 4));
	}
}

sub epoll_wait_mod8 {
	my ($epfd, $maxevents, $timeout_msec, $events) = @_;

	# resize our static buffer if maxevents bigger than we've ever done
	if ($maxevents > $epoll_wait_size) {
		$epoll_wait_size = $maxevents;
		vec($epoll_wait_events, $maxevents * 16 * 8 - 1, 1) = 0;
	}
	@$events = ();
	my $ct = syscall($SYS_epoll_wait, $epfd, $epoll_wait_events,
			$maxevents, $timeout_msec,
			$no_deprecated ? undef : ());
	for (0..$ct - 1) {
		# 16-byte struct epoll_event
		# 4 bytes uint32_t events mask (skipped, useless to us)
		# 4 bytes padding (skipped, useless)
		# 8 bytes epoll_data_t union (first 4 bytes are the fd)
		# So skip the first 8 bytes, take 4, and ignore the last 4:
		$events->[$_] = unpack('L', substr($epoll_wait_events,
							16 * $_ + 8, 4));
	}
}

sub signalfd ($$$) {
	my ($fd, $signos, $flags) = @_;
	if ($SYS_signalfd4) {
		my $set = POSIX::SigSet->new(@$signos);
		syscall($SYS_signalfd4, $fd, "$$set",
			# $Config{sig_count} is NSIG, so this is NSIG/8:
			int($Config{sig_count}/8),
			$flags|$SFD_CLOEXEC);
	} else {
		$! = ENOSYS;
		undef;
	}
}

1;

=head1 WARRANTY

This is free software. IT COMES WITHOUT WARRANTY OF ANY KIND.

=head1 AUTHORS

Brad Fitzpatrick <brad@danga.com>
