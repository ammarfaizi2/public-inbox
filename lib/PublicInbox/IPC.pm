# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# base class for remote IPC calls and workqueues, requires Storable or Sereal
# - ipc_do and ipc_worker_* is for a single worker/producer and uses pipes
# - wq_io_do and wq_worker* is for a single producer and multiple workers,
#   using SOCK_SEQPACKET for work distribution
# use ipc_do when you need work done on a certain process
# use wq_io_do when your work can be done on any idle worker
package PublicInbox::IPC;
use strict;
use v5.10.1;
use parent qw(Exporter);
use Carp qw(croak);
use PublicInbox::DS qw(dwaitpid);
use PublicInbox::Spawn;
use PublicInbox::OnDestroy;
use PublicInbox::WQWorker;
use Socket qw(AF_UNIX MSG_EOR SOCK_STREAM);
my $SEQPACKET = eval { Socket::SOCK_SEQPACKET() }; # portable enough?
our @EXPORT_OK = qw(ipc_freeze ipc_thaw);
my $WQ_MAX_WORKERS = 4096;
my ($enc, $dec);
# ->imports at BEGIN turns sereal_*_with_object into custom ops on 5.14+
# and eliminate method call overhead
BEGIN {
	eval {
		require Sereal::Encoder;
		require Sereal::Decoder;
		Sereal::Encoder->import('sereal_encode_with_object');
		Sereal::Decoder->import('sereal_decode_with_object');
		($enc, $dec) = (Sereal::Encoder->new, Sereal::Decoder->new);
	};
};

if ($enc && $dec) { # should be custom ops
	*ipc_freeze = sub ($) { sereal_encode_with_object $enc, $_[0] };
	*ipc_thaw = sub ($) { sereal_decode_with_object $dec, $_[0], my $ret };
} else {
	require Storable;
	*ipc_freeze = \&Storable::freeze;
	*ipc_thaw = \&Storable::thaw;
}

my $recv_cmd = PublicInbox::Spawn->can('recv_cmd4');
my $send_cmd = PublicInbox::Spawn->can('send_cmd4') // do {
	require PublicInbox::CmdIPC4;
	$recv_cmd //= PublicInbox::CmdIPC4->can('recv_cmd4');
	PublicInbox::CmdIPC4->can('send_cmd4');
};

sub _get_rec ($) {
	my ($r) = @_;
	defined(my $len = <$r>) or return;
	chop($len) eq "\n" or croak "no LF byte in $len";
	defined(my $n = read($r, my $buf, $len)) or croak "read error: $!";
	$n == $len or croak "short read: $n != $len";
	ipc_thaw($buf);
}

sub _send_rec ($$) {
	my ($w, $ref) = @_;
	my $buf = ipc_freeze($ref);
	print $w length($buf), "\n", $buf or croak "print: $!";
}

sub ipc_return ($$$) {
	my ($w, $ret, $exc) = @_;
	_send_rec($w, $exc ? bless(\$exc, 'PublicInbox::IPC::Die') : $ret);
}

sub ipc_worker_loop ($$$) {
	my ($self, $r_req, $w_res) = @_;
	my ($rec, $wantarray, $sub, @args);
	local $/ = "\n";
	while ($rec = _get_rec($r_req)) {
		($wantarray, $sub, @args) = @$rec;
		# no waiting if client doesn't care,
		# this is the overwhelmingly likely case
		if (!defined($wantarray)) {
			eval { $self->$sub(@args) };
			warn "$$ die: $@ (from nowait $sub)\n" if $@;
		} elsif ($wantarray) {
			my @ret = eval { $self->$sub(@args) };
			ipc_return($w_res, \@ret, $@);
		} else { # '' => wantscalar
			my $ret = eval { $self->$sub(@args) };
			ipc_return($w_res, \$ret, $@);
		}
	}
}

# starts a worker if Sereal or Storable is installed
sub ipc_worker_spawn {
	my ($self, $ident, $oldset, $fields) = @_;
	return if ($self->{-ipc_ppid} // -1) == $$; # idempotent
	delete(@$self{qw(-ipc_req -ipc_res -ipc_ppid -ipc_pid)});
	pipe(my ($r_req, $w_req)) or die "pipe: $!";
	pipe(my ($r_res, $w_res)) or die "pipe: $!";
	my $sigset = $oldset // PublicInbox::DS::block_signals();
	$self->ipc_atfork_prepare;
	my $seed = rand(0xffffffff);
	my $pid = fork // die "fork: $!";
	if ($pid == 0) {
		srand($seed);
		eval { PublicInbox::DS->Reset };
		delete @$self{qw(-wq_s1 -wq_s2 -wq_workers -wq_ppid)};
		$w_req = $r_res = undef;
		$w_res->autoflush(1);
		$SIG{$_} = 'IGNORE' for (qw(TERM INT QUIT));
		local $0 = $ident;
		# ensure we properly exit even if warn() dies:
		my $end = PublicInbox::OnDestroy->new($$, sub { exit(!!$@) });
		eval {
			$fields //= {};
			local @$self{keys %$fields} = values(%$fields);
			my $on_destroy = $self->ipc_atfork_child;
			local %SIG = %SIG;
			PublicInbox::DS::sig_setmask($sigset);
			ipc_worker_loop($self, $r_req, $w_res);
		};
		warn "worker $ident PID:$$ died: $@\n" if $@;
		undef $end; # trigger exit
	}
	PublicInbox::DS::sig_setmask($sigset) unless $oldset;
	$r_req = $w_res = undef;
	$w_req->autoflush(1);
	$self->{-ipc_req} = $w_req;
	$self->{-ipc_res} = $r_res;
	$self->{-ipc_ppid} = $$;
	$self->{-ipc_pid} = $pid;
}

sub ipc_worker_reap { # dwaitpid callback
	my ($args, $pid) = @_;
	return if !$?;
	# TERM(15) is our default exit signal, PIPE(13) is likely w/ pager
	my $s = $? & 127;
	warn "PID:$pid died with \$?=$?\n" if $s != 15 && $s != 13;
}

sub wq_wait_old {
	my ($self, $cb, @args) = @_;
	my $pids = delete $self->{"-wq_old_pids.$$"} or return;
	dwaitpid($_, $cb // \&ipc_worker_reap, [$self, @args]) for @$pids;
}

# for base class, override in sub classes
sub ipc_atfork_prepare {}

sub wq_atexit_child {}

sub ipc_atfork_child {
	my ($self) = @_;
	my $io = delete($self->{-ipc_atfork_child_close}) or return;
	close($_) for @$io;
	undef;
}

# idempotent, can be called regardless of whether worker is active or not
sub ipc_worker_stop {
	my ($self, $args) = @_;
	my ($pid, $ppid) = delete(@$self{qw(-ipc_pid -ipc_ppid)});
	my ($w_req, $r_res) = delete(@$self{qw(-ipc_req -ipc_res)});
	if (!$w_req && !$r_res) {
		die "unexpected PID:$pid without IPC pipes" if $pid;
		return; # idempotent
	}
	die 'no PID with IPC pipes' unless $pid;
	$w_req = $r_res = undef;

	return if $$ != $ppid;
	dwaitpid($pid, \&ipc_worker_reap, [$self, $args]);
}

# use this if we have multiple readers reading curl or "pigz -dc"
# and writing to the same store
sub ipc_lock_init {
	my ($self, $f) = @_;
	$f // die 'BUG: no filename given';
	require PublicInbox::Lock;
	$self->{-ipc_lock} //= bless { lock_path => $f }, 'PublicInbox::Lock'
}

# call $self->$sub(@args), on a worker if ipc_worker_spawn was used
sub ipc_do {
	my ($self, $sub, @args) = @_;
	if (my $w_req = $self->{-ipc_req}) { # run in worker
		my $ipc_lock = $self->{-ipc_lock};
		my $lock = $ipc_lock ? $ipc_lock->lock_for_scope : undef;
		if (defined(wantarray)) {
			my $r_res = $self->{-ipc_res} or die 'no ipc_res';
			_send_rec($w_req, [ wantarray, $sub, @args ]);
			my $ret = _get_rec($r_res) // die "no response on $sub";
			die $$ret if ref($ret) eq 'PublicInbox::IPC::Die';
			wantarray ? @$ret : $$ret;
		} else { # likely, fire-and-forget into pipe
			_send_rec($w_req, [ undef , $sub, @args ]);
		}
	} else { # run locally
		$self->$sub(@args);
	}
}

# needed when there's multiple IPC workers and the parent forking
# causes newer siblings to inherit older siblings sockets
sub ipc_sibling_atfork_child {
	my ($self) = @_;
	my ($pid, undef) = delete(@$self{qw(-ipc_pid -ipc_ppid)});
	delete(@$self{qw(-ipc_req -ipc_res)});
	$pid == $$ and die "BUG: $$ ipc_atfork_child called on itself";
}

sub recv_and_run {
	my ($self, $s2, $len, $full_stream) = @_;
	my @fds = $recv_cmd->($s2, my $buf, $len);
	return if scalar(@fds) && !defined($fds[0]);
	my $n = length($buf) or return 0;
	my $nfd = 0;
	for my $fd (@fds) {
		if (open(my $cmdfh, '+<&=', $fd)) {
			$self->{$nfd++} = $cmdfh;
			$cmdfh->autoflush(1);
		} else {
			die "$$ open(+<&=$fd) (FD:$nfd): $!";
		}
	}
	while ($full_stream && $n < $len) {
		my $r = sysread($s2, $buf, $len - $n, $n) // croak "read: $!";
		croak "read EOF after $n/$len bytes" if $r == 0;
		$n = length($buf);
	}
	# Sereal dies on truncated data, Storable returns undef
	my $args = ipc_thaw($buf) // die "thaw error on buffer of size: $n";
	undef $buf;
	my $sub = shift @$args;
	eval { $self->$sub(@$args) };
	warn "$$ wq_worker: $@" if $@;
	delete @$self{0..($nfd-1)};
	$n;
}

sub wq_worker_loop ($) {
	my ($self, $bcast_a) = @_;
	my $wqw = PublicInbox::WQWorker->new($self);
	PublicInbox::WQWorker->new($self, '-wq_bcast2');
	PublicInbox::DS->SetPostLoopCallback(sub { $wqw->{sock} });
	PublicInbox::DS->EventLoop;
	PublicInbox::DS->Reset;
}

sub do_sock_stream { # via wq_io_do, for big requests
	my ($self, $len) = @_;
	recv_and_run($self, delete $self->{0}, $len, 1);
}

sub wq_broadcast {
	my ($self, $sub, @args) = @_;
	if (my $wkr = $self->{-wq_workers}) {
		for my $bcast1 (values %$wkr) {
			my $buf = ipc_freeze([$sub, @args]);
			send($bcast1, $buf, MSG_EOR) // croak "send: $!";
			# XXX shouldn't have to deal with EMSGSIZE here...
		}
	} else {
		eval { $self->$sub(@args) };
		warn "wq_broadcast: $@" if $@;
	}
}

sub wq_io_do { # always async
	my ($self, $sub, $ios, @args) = @_;
	if (my $s1 = $self->{-wq_s1}) { # run in worker
		my $fds = [ map { fileno($_) } @$ios ];
		my $buf = ipc_freeze([$sub, @args]);
		my $n = $send_cmd->($s1, $fds, $buf, MSG_EOR);
		return if defined($n); # likely
		croak "sendmsg: $! (check RLIMIT_NOFILE)" if $!{ETOOMANYREFS};
		croak "sendmsg: $!" if !$!{EMSGSIZE};
		socketpair(my $r, my $w, AF_UNIX, SOCK_STREAM, 0) or
			croak "socketpair: $!";
		$n = $send_cmd->($s1, [ fileno($r) ],
				ipc_freeze(['do_sock_stream', length($buf)]),
				MSG_EOR) // croak "sendmsg: $!";
		undef $r;
		$n = $send_cmd->($w, $fds, $buf, 0) // croak "sendmsg: $!";
		while ($n < length($buf)) {
			my $x = syswrite($w, $buf, length($buf) - $n, $n) //
					croak "syswrite: $!";
			croak "syswrite wrote 0 bytes" if $x == 0;
			$n += $x;
		}
	} else {
		@$self{0..$#$ios} = @$ios;
		eval { $self->$sub(@args) };
		warn "wq_io_do: $@" if $@;
		delete @$self{0..$#$ios}; # don't close
	}
}

sub _wq_worker_start ($$$) {
	my ($self, $oldset, $fields) = @_;
	my ($bcast1, $bcast2);
	socketpair($bcast1, $bcast2, AF_UNIX, $SEQPACKET, 0) or
						die "socketpair: $!";
	my $seed = rand(0xffffffff);
	my $pid = fork // die "fork: $!";
	if ($pid == 0) {
		srand($seed);
		undef $bcast1;
		eval { PublicInbox::DS->Reset };
		delete @$self{qw(-wq_s1 -wq_ppid)};
		$self->{-wq_worker_nr} =
				keys %{delete($self->{-wq_workers}) // {}};
		$SIG{$_} = 'IGNORE' for (qw(PIPE));
		$SIG{$_} = 'DEFAULT' for (qw(TTOU TTIN TERM QUIT INT CHLD));
		local $0 = "$self->{-wq_ident} $self->{-wq_worker_nr}";
		# ensure we properly exit even if warn() dies:
		my $end = PublicInbox::OnDestroy->new($$, sub { exit(!!$@) });
		eval {
			$fields //= {};
			local @$self{keys %$fields} = values(%$fields);
			my $on_destroy = $self->ipc_atfork_child;
			local %SIG = %SIG;
			PublicInbox::DS::sig_setmask($oldset);
			$self->{-wq_bcast2} = $bcast2;
			wq_worker_loop($self);
		};
		warn "worker $self->{-wq_ident} PID:$$ died: $@" if $@;
		undef $end; # trigger exit
	} else {
		$self->{-wq_workers}->{$pid} = $bcast1;
	}
}

# starts workqueue workers if Sereal or Storable is installed
sub wq_workers_start {
	my ($self, $ident, $nr_workers, $oldset, $fields) = @_;
	($send_cmd && $recv_cmd && defined($SEQPACKET)) or return;
	return if $self->{-wq_s1}; # idempotent
	$self->{-wq_s1} = $self->{-wq_s2} = undef;
	socketpair($self->{-wq_s1}, $self->{-wq_s2}, AF_UNIX, $SEQPACKET, 0) or
		die "socketpair: $!";
	$self->ipc_atfork_prepare;
	$nr_workers //= $self->{-wq_nr_workers};
	$nr_workers = $WQ_MAX_WORKERS if $nr_workers > $WQ_MAX_WORKERS;
	my $sigset = $oldset // PublicInbox::DS::block_signals();
	$self->{-wq_workers} = {};
	$self->{-wq_ident} = $ident;
	_wq_worker_start($self, $sigset, $fields) for (1..$nr_workers);
	PublicInbox::DS::sig_setmask($sigset) unless $oldset;
	$self->{-wq_ppid} = $$;
}

sub wq_worker_incr { # SIGTTIN handler
	my ($self, $oldset, $fields) = @_;
	$self->{-wq_s2} or return;
	die "-wq_nr_workers locked" if defined $self->{-wq_nr_workers};
	return if wq_workers($self) >= $WQ_MAX_WORKERS;
	$self->ipc_atfork_prepare;
	my $sigset = $oldset // PublicInbox::DS::block_signals();
	_wq_worker_start($self, $sigset, $fields);
	PublicInbox::DS::sig_setmask($sigset) unless $oldset;
}

sub wq_exit { # wakes up wq_worker_decr_wait
	send($_[0]->{-wq_s2}, $$, MSG_EOR) // die "$$ send: $!";
	exit;
}

sub wq_worker_decr { # SIGTTOU handler, kills first idle worker
	my ($self) = @_;
	return unless wq_workers($self);
	die "-wq_nr_workers locked" if defined $self->{-wq_nr_workers};
	$self->wq_io_do('wq_exit');
	# caller must call wq_worker_decr_wait in main loop
}

sub wq_worker_decr_wait {
	my ($self, $timeout, $cb, @args) = @_;
	return if $self->{-wq_ppid} != $$; # can't reap siblings or parents
	die "-wq_nr_workers locked" if defined $self->{-wq_nr_workers};
	my $s1 = $self->{-wq_s1} // croak 'BUG: no wq_s1';
	vec(my $rin = '', fileno($s1), 1) = 1;
	select(my $rout = $rin, undef, undef, $timeout) or
		croak 'timed out waiting for wq_exit';
	recv($s1, my $pid, 64, 0) // croak "recv: $!";
	my $workers = $self->{-wq_workers} // croak 'BUG: no wq_workers';
	delete $workers->{$pid} // croak "BUG: PID:$pid invalid";
	dwaitpid($pid, $cb // \&ipc_worker_reap, [ $self, @args ]);
}

# set or retrieve number of workers
sub wq_workers {
	my ($self, $nr, $cb, @args) = @_;
	my $cur = $self->{-wq_workers} or return;
	if (defined $nr) {
		while (scalar(keys(%$cur)) > $nr) {
			$self->wq_worker_decr;
			$self->wq_worker_decr_wait(undef, $cb, @args);
		}
		$self->wq_worker_incr while scalar(keys(%$cur)) < $nr;
	}
	scalar(keys(%$cur));
}

sub wq_close {
	my ($self, $nohang, $cb, @args) = @_;
	delete @$self{qw(-wq_s1 -wq_s2)} or return;
	my $ppid = delete $self->{-wq_ppid} or return;
	my $workers = delete $self->{-wq_workers} // die 'BUG: no wq_workers';
	return if $ppid != $$; # can't reap siblings or parents
	my @pids = map { $_ + 0 } keys %$workers;
	if ($nohang) {
		push @{$self->{"-wq_old_pids.$$"}}, @pids;
	} else {
		$cb //= \&ipc_worker_reap;
		unshift @args, $self;
		dwaitpid($_, $cb, \@args) for @pids;
	}
}

sub wq_kill_old {
	my ($self, $sig) = @_;
	my $pids = $self->{"-wq_old_pids.$$"} or return;
	kill($sig // 'TERM', @$pids);
}

sub wq_kill {
	my ($self, $sig) = @_;
	my $workers = $self->{-wq_workers} or return;
	kill($sig // 'TERM', keys %$workers);
}

sub WQ_MAX_WORKERS { $WQ_MAX_WORKERS }

sub DESTROY {
	my ($self) = @_;
	my $ppid = $self->{-wq_ppid};
	wq_kill($self) if $ppid && $ppid == $$;
	my $err = $?;
	wq_close($self);
	wq_wait_old($self);
	ipc_worker_stop($self);
	$? = $err if $err;
}

sub detect_nproc () {
	# _SC_NPROCESSORS_ONLN = 84 on both Linux glibc and musl
	return POSIX::sysconf(84) if $^O eq 'linux';
	return POSIX::sysconf(58) if $^O eq 'freebsd';
	# TODO: more OSes

	# getconf(1) is POSIX, but *NPROCESSORS* vars are not
	for (qw(_NPROCESSORS_ONLN NPROCESSORS_ONLN)) {
		`getconf $_ 2>/dev/null` =~ /^(\d+)$/ and return $1;
	}
	for my $nproc (qw(nproc gnproc)) { # GNU coreutils nproc
		`$nproc 2>/dev/null` =~ /^(\d+)$/ and return $1;
	}

	# should we bother with `sysctl hw.ncpu`?  Those only give
	# us total processor count, not online processor count.
	undef
}

1;
