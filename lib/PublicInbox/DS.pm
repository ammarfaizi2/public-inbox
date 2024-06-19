# This library is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This license differs from the rest of public-inbox
#
# This is a fork of the unmaintained Danga::Socket (1.61) with
# significant changes.  See Documentation/technical/ds.txt in our
# source for details.
#
# Do not expect this to be a stable API like Danga::Socket,
# but it will evolve to suite our needs and to take advantage of
# newer Linux and *BSD features.
# Bugs encountered were reported to bug-Danga-Socket@rt.cpan.org,
# fixed in Danga::Socket 1.62 and visible at:
# https://rt.cpan.org/Public/Dist/Display.html?Name=Danga-Socket
#
# fields:
# sock: underlying socket
# rbuf: scalarref, usually undef
# wbuf: arrayref of coderefs or tmpio (autovivified))
#        (tmpio = [ GLOB, offset, [ length ] ])
package PublicInbox::DS;
use strict;
use v5.10.1;
use parent qw(Exporter);
use bytes qw(length substr); # FIXME(?): needed for PublicInbox::NNTP
use POSIX qw(WNOHANG sigprocmask SIG_SETMASK SIG_UNBLOCK);
use Fcntl qw(SEEK_SET :DEFAULT);
use Time::HiRes qw(clock_gettime CLOCK_MONOTONIC);
use Scalar::Util qw(blessed);
use PublicInbox::Syscall qw(%SIGNUM
	EPOLLIN EPOLLOUT EPOLLONESHOT EPOLLEXCLUSIVE);
use PublicInbox::Tmpfile;
use PublicInbox::Select;
use PublicInbox::OnDestroy;
use Errno qw(EAGAIN EINVAL ECHILD);
use Carp qw(carp croak);
our @EXPORT_OK = qw(now msg_more awaitpid add_timer add_uniq_timer);

my $nextq; # queue for next_tick
my $reap_armed;
my @active; # FDs (or objs) returned by epoll/kqueue
our (%AWAIT_PIDS, # pid => [ $callback, @args ]
	$cur_runq, # only set inside next_tick
     @FD_MAP, # fd (num) -> PublicInbox::DS object
     $Poller, # global Select, Epoll, DSPoll, or DSKQXS ref

     @post_loop_do,              # subref + args to call at the end of each loop

     $loop_timeout,               # timeout of event loop in milliseconds
     @Timers,                    # timers
     %UniqTimer,
     $in_loop,
     );

Reset();

# clobber everything explicitly to avoid DESTROY ordering problems w/ DBI
END { Reset() }

#####################################################################
### C L A S S   M E T H O D S
#####################################################################

=head2 C<< CLASS->Reset() >>

Reset all state

=cut
sub Reset {
	$Poller = bless [], 'PublicInbox::DummyPoller';
	do {
		$in_loop = undef; # first in case DESTROY callbacks use this
		# clobbering $Poller may call DSKQXS::DESTROY,
		# we must always have this set to something to avoid
		# needing branches before ep_del/ep_mod calls (via ->close).
		@FD_MAP = ();
		@Timers = ();
		%UniqTimer = ();
		@post_loop_do = ();

		# we may be called from an *atfork_child inside next_tick:
		@$cur_runq = () if $cur_runq;
		@active = ();
		$nextq = undef; # may call ep_del
		%AWAIT_PIDS = ();
	} while (@Timers || $nextq || keys(%AWAIT_PIDS) ||
		@active || @FD_MAP ||
		@post_loop_do || keys(%UniqTimer) ||
		scalar(@{$cur_runq // []})); # do not vivify cur_runq

	$reap_armed = undef;
	$loop_timeout = -1;  # no timeout by default
	$Poller = PublicInbox::Select->new;
}

sub _add_named_timer {
	my ($name, $secs, $coderef, @args) = @_;
	my $fire_time = now() + $secs;
	my $timer = [$fire_time, $name, $coderef, @args];

	if (!@Timers || $fire_time >= $Timers[-1][0]) {
		push @Timers, $timer;
		return $timer;
	}

	# Now, where do we insert?  (NOTE: this appears slow, algorithm-wise,
	# but it was compared against calendar queues, heaps, naive push/sort,
	# and a bunch of other versions, and found to be fastest with a large
	# variety of datasets.)
	for (my $i = 0; $i < @Timers; $i++) {
		if ($Timers[$i][0] > $fire_time) {
			splice(@Timers, $i, 0, $timer);
			return $timer;
		}
	}
	die "Shouldn't get here.";
}

sub add_timer { _add_named_timer(undef, @_) }

sub add_uniq_timer { # ($name, $secs, $coderef, @args) = @_;
	$UniqTimer{$_[0]} //= _add_named_timer(@_);
}

# caller sets return value to $Poller
sub _InitPoller () {
	my @try = ($^O eq 'linux' ? 'Epoll' : 'DSKQXS');
	my $cls;
	for (@try, 'DSPoll') {
		$cls = "PublicInbox::$_";
		last if eval "require $cls";
	}
	$cls->new;
}

sub now () { clock_gettime(CLOCK_MONOTONIC) }

sub next_tick () {
	$cur_runq = $nextq or return;
	$nextq = undef;
	while (my $obj = shift @$cur_runq) {
		# avoid "ref" on blessed refs to workaround a Perl 5.16.3 leak:
		# https://rt.perl.org/Public/Bug/Display.html?id=114340
		blessed($obj) ? $obj->event_step : $obj->();
	}
	1;
}

# runs timers and returns milliseconds for next one, or next event loop
sub RunTimers {
	my $ran = next_tick();

	return ($nextq || $ran ? 0 : $loop_timeout) unless @Timers;

	my $now = now();

	# Run expired timers
	while (@Timers && $Timers[0][0] <= $now) {
		my $to_run = shift(@Timers);
		delete $UniqTimer{$to_run->[1] // ''};
		$to_run->[2]->(@$to_run[3..$#$to_run]);
		$ran = 1;
	}

	# timers may enqueue into nextq:
	return 0 if $nextq || $ran;

	return $loop_timeout unless @Timers;

	# convert time to an even number of milliseconds, adding 1
	# extra, otherwise floating point fun can occur and we'll
	# call RunTimers like 20-30 times, each returning a timeout
	# of 0.0000212 seconds
	my $t = int(($Timers[0][0] - $now) * 1000) + 1;

	# -1 is an infinite timeout, so prefer a real timeout
	($loop_timeout < 0 || $loop_timeout >= $t) ? $t : $loop_timeout
}

sub sig_setmask { sigprocmask(SIG_SETMASK, @_) or die "sigprocmask: $!" }

# ensure we detect bugs, HW problems and user rlimits
our @UNBLOCKABLE = (POSIX::SIGABRT, POSIX::SIGBUS, POSIX::SIGFPE,
	POSIX::SIGILL, POSIX::SIGSEGV, POSIX::SIGXCPU, POSIX::SIGXFSZ);

sub block_signals { # anything in @_ stays unblocked
	my $newset = POSIX::SigSet->new;
	$newset->fillset or die "fillset: $!";
	for (@_, @UNBLOCKABLE) { $newset->delset($_) or die "delset($_): $!" }
	my $oldset = POSIX::SigSet->new;
	sig_setmask($newset, $oldset);
	$oldset;
}

sub await_cb ($;@) {
	my ($pid, @cb_args) = @_;
	my $cb = shift @cb_args or return;
	eval { $cb->($pid, @cb_args) };
	warn "E: awaitpid($pid): $@" if $@;
}

# This relies on our Perl process being single-threaded, or at least
# no threads spawning and waiting on processes (``, system(), etc...)
# Threads are officially discouraged by the Perl5 team, and I expect
# that to remain the case.
sub reap_pids {
	$reap_armed = undef;
	while (1) {
		my $pid = waitpid(-1, WNOHANG) or return;
		if (defined(my $cb_args = delete $AWAIT_PIDS{$pid})) {
			await_cb($pid, @$cb_args) if $cb_args;
		} elsif ($pid == -1 && $! == ECHILD) {
			return requeue(\&dflush); # force @post_loop_do to run
		} elsif ($pid > 0) {
			warn "W: reaped unknown PID=$pid: \$?=$?\n";
		} else { # does this happen?
			return warn("W: waitpid(-1, WNOHANG) => $pid ($!)");
		}
	}
}

# reentrant SIGCHLD handler (since reap_pids is not reentrant)
sub enqueue_reap () { $reap_armed //= requeue(\&reap_pids) }

sub in_loop () { $in_loop }

# use inside @post_loop_do, returns number of busy clients
sub close_non_busy () {
	my $n = 0;
	for my $s (grep defined, @FD_MAP) {
		# close as much as possible, early as possible
		($s->busy ? ++$n : $s->close) if $s->can('busy');
	}
	$n;
}

# Internal function: run the post-event callback, send read events
# for pushed-back data, and close pending connections.  returns 1
# if event loop should continue, or 0 to shut it all down.
sub PostEventLoop () {
	# by default we keep running, unless a postloop callback cancels it
	@post_loop_do ? $post_loop_do[0]->(@post_loop_do[1..$#post_loop_do]) : 1
}

sub sigset_prep ($$$) {
	my ($sig, $init, $each) = @_; # $sig: { signame => whatever }
	my $ret = POSIX::SigSet->new;
	$ret->$init or die "$init: $!";
	for my $s (keys %$sig) {
		my $num = $SIGNUM{$s} // POSIX->can("SIG$s")->();
		$ret->$each($num) or die "$each ($s => $num): $!";
	}
	for (@UNBLOCKABLE) { $ret->$each($_) or die "$each ($_): $!" }
	$ret;
}

sub allowset ($) { sigset_prep $_[0], 'fillset', 'delset' }
sub unblockset ($) { sigset_prep $_[0], 'emptyset', 'addset' }

# Start processing IO events. In most daemon programs this never exits. See
# C<post_loop_do> for how to exit the loop.
sub event_loop (;$$) {
	my ($sig, $oldset) = @_;
	$Poller //= _InitPoller();
	require PublicInbox::Sigfd if $sig;
	my $sigfd = $sig ? PublicInbox::Sigfd->new($sig) : undef;
	if ($sigfd && $sigfd->{is_kq}) {
		my $tmp = allowset($sig);
		local @SIG{keys %$sig} = values(%$sig);
		sig_setmask($tmp, my $old = POSIX::SigSet->new);
		# Unlike Linux signalfd, EVFILT_SIGNAL can't handle
		# signals received before the filter is created,
		# so we peek at signals here.
		sig_setmask($old);
	}
	local @SIG{keys %$sig} = values(%$sig) if $sig && !$sigfd;
	local $SIG{PIPE} = 'IGNORE';
	if (!$sigfd && $sig) {
		# wake up every second to accept signals if we don't
		# have signalfd or IO::KQueue:
		sig_setmask($oldset) if $oldset;
		sigprocmask(SIG_UNBLOCK, unblockset($sig)) or
			die "SIG_UNBLOCK: $!";
		$loop_timeout = 1000;
	}
	$_[0] = $sigfd = $sig = undef; # $_[0] == sig
	local $in_loop = 1;
	do {
		my $timeout = RunTimers();

		# grab whatever FDs are ready
		$Poller->ep_wait($timeout, \@active);

		# map all FDs to their associated Perl object
		@active = @FD_MAP[@active];

		while (my $obj = shift @active) {
			$obj->event_step;
		}
	} while (PostEventLoop());
}

#####################################################################
### PublicInbox::DS-the-object code
#####################################################################

=head2 OBJECT METHODS

=head2 C<< CLASS->new( $socket ) >>

Create a new PublicInbox::DS subclass object for the given I<socket> which will
react to events on it during the C<event_loop>.

This is normally (always?) called from your subclass via:

  $class->SUPER::new($socket);

=cut
sub new {
    my ($self, $sock, $ev) = @_;
    $self->{sock} = $sock;
    my $fd = fileno($sock);

    $Poller //= _InitPoller();
retry:
    if ($Poller->ep_add($sock, $ev)) {
        if ($! == EINVAL && ($ev & EPOLLEXCLUSIVE)) {
            $ev &= ~EPOLLEXCLUSIVE;
            goto retry;
        }
        die "EPOLL_CTL_ADD $self/$sock/$fd: $!";
    }
    defined($FD_MAP[$fd]) and
		croak("BUG: FD:$fd in use by $FD_MAP[$fd] (for $self/$sock)");

    $FD_MAP[$fd] = $self;
}

# for IMAP, NNTP, and POP3 which greet clients upon connect
sub greet {
	my ($self, $sock) = @_;
	my $ev = EPOLLIN;
	my $wbuf;
	if ($sock->can('accept_SSL') && !$sock->accept_SSL) {
		return if $! != EAGAIN || !($ev = PublicInbox::TLS::epollbit());
		$wbuf = [ \&accept_tls_step, $self->can('do_greet')];
	}
	new($self, $sock, $ev | EPOLLONESHOT);
	if ($wbuf) {
		$self->{wbuf} = $wbuf;
	} else {
		$self->do_greet;
	}
	$self;
}

sub requeue ($) { push @$nextq, $_[0] } # autovivifies

# drop the IO::Handle ref, true if successful, false if not (or already dropped)
# (this is closer to CORE::close than Danga::Socket::close)
sub close {
	my ($self) = @_;
	my $sock = delete $self->{sock} or return;

	# we need to clear our write buffer, as there may
	# be self-referential closures (sub { $client->close })
	# preventing the object from being destroyed
	delete $self->{wbuf};
	$FD_MAP[fileno($sock)] = undef;

	!$Poller->ep_del($sock); # stop getting notifications
}

# portable, non-thread-safe sendfile emulation (no pread, yet)
sub send_tmpio ($$) {
    my ($sock, $tmpio) = @_;

    sysseek($tmpio->[0], $tmpio->[1], SEEK_SET) or return;
    my $n = $tmpio->[2] // 65536;
    $n = 65536 if $n > 65536;
    defined(my $to_write = sysread($tmpio->[0], my $buf, $n)) or return;
    my $written = 0;
    while ($to_write > 0) {
        if (defined(my $w = syswrite($sock, $buf, $to_write, $written))) {
            $written += $w;
            $to_write -= $w;
        } else {
            return if $written == 0;
            last;
        }
    }
    $tmpio->[1] += $written; # offset
    $tmpio->[2] -= $written if defined($tmpio->[2]); # length
    $written;
}

sub epbit ($$) { # (sock, default)
	$_[0]->can('stop_SSL') ? PublicInbox::TLS::epollbit() : $_[1];
}

# returns 1 if done, 0 if incomplete
sub flush_write ($) {
    my ($self) = @_;
    my $sock = $self->{sock} or return;
    my $wbuf = $self->{wbuf} or return 1;

next_buf:
    while (my $bref = $wbuf->[0]) {
        if (ref($bref) ne 'CODE') {
            while ($sock) {
                my $w = send_tmpio($sock, $bref); # bref is tmpio
                if (defined $w) {
                    if ($w == 0) {
                        shift @$wbuf;
                        goto next_buf;
                    }
                } elsif ($! == EAGAIN && (my $ev = epbit($sock, EPOLLOUT))) {
                    epwait($sock, $ev | EPOLLONESHOT);
                    return 0;
                } else {
                    return $self->close;
                }
            }
        } else { #(ref($bref) eq 'CODE') {
            shift @$wbuf;
            my $before = scalar(@$wbuf);
            $bref->($self);

            # bref may be enqueueing more CODE to call (see accept_tls_step)
            return 0 if (scalar(@$wbuf) > $before);
        }
    } # while @$wbuf

    delete $self->{wbuf};
    1; # all done
}

sub rbuf_idle ($$) {
    my ($self, $rbuf) = @_;
    if ($$rbuf eq '') { # who knows how long till we can read again
        delete $self->{rbuf};
    } else {
        $self->{rbuf} = $rbuf;
    }
}

# returns true if bytes are read, false otherwise
sub do_read ($$$;$) {
	my ($self, $rbuf, $len, $off) = @_;
	my ($ev, $r, $s);
	$r = sysread($s = $self->{sock}, $$rbuf, $len, $off // 0) and return $r;

	if (!defined($r) && $! == EAGAIN && ($ev = epbit($s, EPOLLIN))) {
		epwait($s, $ev | EPOLLONESHOT);
		rbuf_idle($self, $rbuf);
	} else {
		$self->close;
	}
	undef;
}

# drop the socket if we hit unrecoverable errors on our system which
# require BOFH attention: ENOSPC, EFBIG, EIO, EMFILE, ENFILE...
sub drop {
	my $self = shift;
	carp(@_);
	$self->close;
	undef;
}

sub tmpio ($$$) {
	my ($self, $bref, $off) = @_;
	my $fh = tmpfile 'wbuf', $self->{sock}, 1 or
		return drop($self, "tmpfile $!");
	$fh->autoflush(1);
	my $len = length($$bref) - $off;
	my $n = syswrite($fh, $$bref, $len, $off) //
		return drop($self, "write ($len): $!");
	$n == $len or return drop($self, "wrote $n < $len bytes");
	[ $fh, 0 ] # [1] = offset, [2] = length, not set by us
}

=head2 C<< $obj->write( $data ) >>

Write the specified data to the underlying handle.  I<data> may be scalar,
scalar ref, code ref (to run when there).
Returns 1 if writes all went through, or 0 if there are writes in queue. If
it returns 1, caller should stop waiting for 'writable' events)

=cut
sub write {
    my ($self, $data) = @_;

    # nobody should be writing to closed sockets, but caller code can
    # do two writes within an event, have the first fail and
    # disconnect the other side (whose destructor then closes the
    # calling object, but it's still in a method), and then the
    # now-dead object does its second write.  that is this case.  we
    # just lie and say it worked.  it'll be dead soon and won't be
    # hurt by this lie.
    my $sock = $self->{sock} or return 1;
    my $ref = ref $data;
    my $bref = $ref ? $data : \$data;
    my $wbuf = $self->{wbuf};
    if ($wbuf && scalar(@$wbuf)) { # already buffering, can't write more...
        if ($ref eq 'CODE') {
            push @$wbuf, $bref;
        } else {
            my $tmpio = $wbuf->[-1];
            if (ref($tmpio) eq 'ARRAY' && !defined($tmpio->[2])) {
                # append to tmp file buffer
                $tmpio->[0]->print($$bref) or return drop($self, "print: $!");
            } else {
                my $tmpio = tmpio($self, $bref, 0) or return 0;
                push @$wbuf, $tmpio;
            }
        }
        return 0;
    } elsif ($ref eq 'CODE') {
        $bref->($self);
        return 1;
    } else {
        my $to_write = length($$bref);
        my $written = syswrite($sock, $$bref, $to_write);

        if (defined $written) {
            return 1 if $written == $to_write;
            requeue($self); # runs: event_step -> flush_write
        } elsif ($! == EAGAIN) {
            my $ev = epbit($sock, EPOLLOUT) or return $self->close;
            epwait($sock, $ev | EPOLLONESHOT);
            $written = 0;
        } else {
            return $self->close;
        }

        # deal with EAGAIN or partial write:
        my $tmpio = tmpio($self, $bref, $written) or return 0;

        # wbuf may be an empty array if we're being called inside
        # ->flush_write via CODE bref:
        push @{$self->{wbuf}}, $tmpio; # autovivifies
        return 0;
    }
}

use constant MSG_MORE => ($^O eq 'linux') ? 0x8000 : 0;

sub msg_more ($$) {
    my $self = $_[0];
    my $sock = $self->{sock} or return 1;
    my $wbuf = $self->{wbuf};

    if (MSG_MORE && (!defined($wbuf) || !scalar(@$wbuf)) &&
		!$sock->can('stop_SSL')) {
        my $n = send($sock, $_[1], MSG_MORE);
        if (defined $n) {
            my $nlen = length($_[1]) - $n;
            return 1 if $nlen == 0; # all done!
            # queue up the unwritten substring:
            my $tmpio = tmpio($self, \($_[1]), $n) or return 0;
            push @{$self->{wbuf}}, $tmpio; # autovivifies
            epwait($sock, EPOLLOUT|EPOLLONESHOT);
            return 0;
        }
    }

    # don't redispatch into NNTPdeflate::write
    PublicInbox::DS::write($self, \($_[1]));
}

sub epwait ($$) {
	my ($io, $ev) = @_;
	$Poller->ep_mod($io, $ev) and croak("EPOLL_CTL_MOD($io): $!");
}

# return true if complete, false if incomplete (or failure)
sub accept_tls_step ($) {
    my ($self) = @_;
    my $sock = $self->{sock} or return;
    return 1 if $sock->accept_SSL;
    return $self->close if $! != EAGAIN;
    my $ev = PublicInbox::TLS::epollbit() or return $self->close;
    epwait($sock, $ev | EPOLLONESHOT);
    unshift(@{$self->{wbuf}}, \&accept_tls_step); # autovivifies
    0;
}

# return value irrelevant
sub shutdn_tls_step ($) {
    my ($self) = @_;
    my $sock = $self->{sock} or return;
    return $self->close if $sock->stop_SSL(SSL_fast_shutdown => 1);
    return $self->close if $! != EAGAIN;
    my $ev = PublicInbox::TLS::epollbit() or return $self->close;
    epwait($sock, $ev | EPOLLONESHOT);
    unshift(@{$self->{wbuf}}, \&shutdn_tls_step); # autovivifies
}

# don't bother with shutdown($sock, 2), we don't fork+exec w/o CLOEXEC
# or fork w/o exec, so no inadvertent socket sharing
sub shutdn ($) {
	my ($self) = @_;
	my $sock = $self->{sock} or return;
	$sock->can('stop_SSL') ? shutdn_tls_step($self) : $self->close;
}

sub dflush {} # overridden by DSdeflate
sub compressed {} # overridden by DSdeflate
sub long_response_done {} # overridden by Net::NNTP

sub long_step {
	my ($self) = @_;
	# wbuf is unset or empty, here; {long} may add to it
	my ($fd, $cb, $t0, @args) = @{$self->{long_cb}};
	my $more = eval { $cb->($self, @args) };
	if ($@ || !$self->{sock}) { # something bad happened...
		delete $self->{long_cb};
		my $elapsed = now() - $t0;
		$@ and warn("$@ during long response[$fd] - ",
				sprintf('%0.6f', $elapsed),"\n");
		$self->out(" deferred[$fd] aborted - %0.6f", $elapsed);
		$self->close;
	} elsif ($more) { # $self->{wbuf}:
		# control passed to ibx_async_cat if $more == \undef
		requeue_once($self) if !ref($more);
	} else { # all done!
		delete $self->{long_cb};
		$self->long_response_done;
		my $elapsed = now() - $t0;
		$self->out(" deferred[$fd] done - %0.6f", $elapsed);
		my $wbuf = $self->{wbuf}; # do NOT autovivify
		requeue($self) unless $wbuf && @$wbuf;
	}
}

sub requeue_once {
	my ($self) = @_;
	# COMPRESS users all share the same DEFLATE context.
	# Flush it here to ensure clients don't see each other's data
	$self->dflush;

	# no recursion, schedule another call ASAP,
	# but only after all pending writes are done.
	# autovivify wbuf.  wbuf may be populated by $cb,
	# no need to rearm if so: (push returns new size of array)
	$self->requeue if push(@{$self->{wbuf}}, \&long_step) == 1;
}

sub long_response ($$;@) {
	my ($self, $cb, @args) = @_; # cb returns true if more, false if done
	my $sock = $self->{sock} or return;
	# make sure we disable reading during a long response,
	# clients should not be sending us stuff and making us do more
	# work while we are stream a response to them
	$self->{long_cb} = [ fileno($sock), $cb, now(), @args ];
	long_step($self); # kick off!
	undef;
}

sub awaitpid {
	my ($pid, @cb_args) = @_; # @cb_args = ($cb, @args), $cb may be undef
	$AWAIT_PIDS{$pid} = \@cb_args if @cb_args;
	# provide synchronous API
	if (defined(wantarray) || (!$in_loop && !@cb_args)) {
		my $ret = waitpid($pid, 0);
		if ($ret == $pid) {
			my $cb_args = delete $AWAIT_PIDS{$pid};
			@cb_args = @$cb_args if !@cb_args && $cb_args;
			await_cb($pid, @cb_args);
		} else {
			carp "waitpid($pid) => $ret ($!)";
			delete $AWAIT_PIDS{$pid};
		}
		return $ret;
	} elsif ($in_loop) { # We could've just missed our SIGCHLD, cover it, here:
		enqueue_reap();
	}
}

# for persistent child process
sub fork_persist () {
	my $seed = rand(0xffffffff);
	my $pid = PublicInbox::OnDestroy::fork_tmp;
	if ($pid == 0) {
		srand($seed);
		eval { Net::SSLeay::randomize() }; # may not be loaded
		Reset();
	}
	$pid;
}

package PublicInbox::DummyPoller; # only used during Reset
use v5.12;

sub ep_del {}
no warnings 'once';
*ep_add = \&ep_del;
*ep_mod = \&ep_del;

1;

=head1 AUTHORS (Danga::Socket)

Brad Fitzpatrick <brad@danga.com> - author

Michael Granger <ged@danga.com> - docs, testing

Mark Smith <junior@danga.com> - contributor, heavy user, testing

Matt Sergeant <matt@sergeant.org> - kqueue support, docs, timers, other bits
