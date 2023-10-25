# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Like most Perl modules in public-inbox, this is internal and
# NOT subject to any stability guarantees!  It is only documented
# for other hackers.
#
# This is used to limit the number of processes spawned by the
# PSGI server, so it acts like a semaphore and queues up extra
# commands to be run if currently at the limit.  Multiple "limiters"
# may be configured which give inboxes different channels to
# operate in.  This can be useful to ensure smaller inboxes can
# be cloned while cloning of large inboxes is maxed out.
#
# This does not depend on the PublicInbox::DS::event_loop or any
# other external scheduling mechanism, you just need to call
# start() and finish() appropriately. However, public-inbox-httpd
# (which uses PublicInbox::DS)  will be able to schedule this
# based on readability of stdout from the spawned process.
# See GitHTTPBackend.pm and SolverGit.pm for usage examples.
# It does not depend on any form of threading.
#
# This is useful for scheduling CGI execution of both long-lived
# git-http-backend(1) process (for "git clone") as well as short-lived
# processes such as git-apply(1).

package PublicInbox::Qspawn;
use v5.12;
use PublicInbox::Spawn qw(popen_rd);
use PublicInbox::GzipFilter;
use Scalar::Util qw(blessed);
use PublicInbox::Limiter;
use PublicInbox::Aspawn qw(run_await);
use PublicInbox::Syscall qw(EPOLLIN);
use PublicInbox::InputPipe;
use Carp qw(carp confess);

# n.b.: we get EAGAIN with public-inbox-httpd, and EINTR on other PSGI servers
use Errno qw(EAGAIN EINTR);

my $def_limiter;

# declares a command to spawn (but does not spawn it).
# $cmd is the command to spawn
# $cmd_env is the environ for the child process (not PSGI env)
# $opt can include redirects and perhaps other process spawning options
# {qsp_err} is an optional error buffer callers may access themselves
sub new {
	my ($class, $cmd, $cmd_env, $opt) = @_;
	bless { args => [ $cmd, $cmd_env, $opt ? { %$opt } : {} ] }, $class;
}

sub _do_spawn {
	my ($self, $start_cb, $limiter) = @_;
	my ($cmd, $cmd_env, $opt) = @{$self->{args}};
	my %o = %{$opt || {}};
	$self->{limiter} = $limiter;
	for my $k (@PublicInbox::Spawn::RLIMITS) {
		$opt->{$k} = $limiter->{$k} // next;
	}
	$self->{-quiet} = 1 if $o{quiet};
	$limiter->{running}++;
	if ($start_cb) {
		eval { # popen_rd may die on EMFILE, ENFILE
			$self->{rpipe} = popen_rd($cmd, $cmd_env, $opt,
							\&waitpid_err, $self);
			$start_cb->($self); # EPOLL_CTL_ADD may ENOSPC/ENOMEM
		};
	} else {
		eval { run_await($cmd, $cmd_env, $opt, \&wait_await, $self) };
		warn "E: $@" if $@;
	}
	finish($self, $@) if $@;
}

sub psgi_status_err { # Qspawn itself is useful w/o PSGI
	require PublicInbox::WwwStatic;
	PublicInbox::WwwStatic::r($_[0] // 500);
}

sub finalize ($) {
	my ($self) = @_;

	# process is done, spawn whatever's in the queue
	my $limiter = delete $self->{limiter} or return;
	my $running = --$limiter->{running};

	if ($running < $limiter->{max}) {
		if (my $next = shift(@{$limiter->{run_queue}})) {
			_do_spawn(@$next, $limiter);
		}
	}
	if (my $err = $self->{_err}) { # set by finish or waitpid_err
		utf8::decode($err);
		if (my $dst = $self->{qsp_err}) {
			$$dst .= $$dst ? " $err" : "; $err";
		}
		warn "E: @{$self->{args}->[0]}: $err\n" if !$self->{-quiet};
	}

	my ($env, $qx_cb_arg) = delete @$self{qw(psgi_env qx_cb_arg)};
	if ($qx_cb_arg) {
		my $cb = shift @$qx_cb_arg;
		eval { $cb->($self->{args}->[2]->{1}, @$qx_cb_arg) };
		return unless $@;
		warn "E: $@"; # hope qspawn.wcb can handle it
	}
	return if $self->{passed}; # another command chained it
	if (my $wcb = delete $env->{'qspawn.wcb'}) {
		# have we started writing, yet?
		$wcb->(psgi_status_err($env->{'qspawn.fallback'}));
	}
}

sub waitpid_err { # callback for awaitpid
	my (undef, $self) = @_; # $_[0]: pid
	$self->{_err} = ''; # for defined check in ->finish
	if ($?) { # XXX this may be redundant
		my $status = $? >> 8;
		my $sig = $? & 127;
		$self->{_err} .= "exit status=$status";
		$self->{_err} .= " signal=$sig" if $sig;
	}
	finalize($self) if !$self->{rpipe};
}

sub wait_await { # run_await cb
	my ($pid, $cmd, $cmd_env, $opt, $self) = @_;
	waitpid_err($pid, $self);
}

sub yield_chunk { # $_[-1] is sysread buffer (or undef)
	my ($self, $ipipe) = @_;
	if (!defined($_[-1])) {
		warn "error reading body: $!";
	} elsif ($_[-1] eq '') { # normal EOF
		$self->finish;
		$self->{qfh}->close;
	} elsif (defined($self->{qfh}->write($_[-1]))) {
		return; # continue while HTTP client is reading our writes
	} # else { # HTTP client disconnected
	delete $self->{rpipe};
	$ipipe->close;
}

sub finish ($;$) {
	my ($self, $err) = @_;
	$self->{_err} //= $err; # only for $@

	# we can safely finalize if pipe was closed before, or if
	# {_err} is defined by waitpid_err.  Deleting {rpipe} will
	# trigger PublicInbox::ProcessIO::DESTROY -> waitpid_err,
	# but it may not fire right away if inside the event loop.
	my $closed_before = !delete($self->{rpipe});
	finalize($self) if $closed_before || defined($self->{_err});
}

sub start ($$$) {
	my ($self, $limiter, $start_cb) = @_;
	if ($limiter->{running} < $limiter->{max}) {
		_do_spawn($self, $start_cb, $limiter);
	} else {
		push @{$limiter->{run_queue}}, [ $self, $start_cb ];
	}
}

# Similar to `backtick` or "qx" ("perldoc -f qx"), it calls @qx_cb_arg with
# the stdout of the given command when done; but respects the given limiter
# $env is the PSGI env.  As with ``/qx; only use this when output is small
# and safe to slurp.
sub psgi_qx {
	my ($self, $env, $limiter, @qx_cb_arg) = @_;
	$self->{psgi_env} = $env;
	$self->{qx_cb_arg} = \@qx_cb_arg;
	$limiter ||= $def_limiter ||= PublicInbox::Limiter->new(32);
	start($self, $limiter, undef);
}

sub yield_pass {
	my ($self, $ipipe, $res) = @_; # $ipipe = InputPipe
	my $env = $self->{psgi_env};
	my $wcb = delete $env->{'qspawn.wcb'} // confess('BUG: no qspawn.wcb');
	if (ref($res) eq 'CODE') { # chain another command
		delete $self->{rpipe};
		$ipipe->close if $ipipe;
		$res->($wcb);
		$self->{passed} = 1;
		return; # all done
	}
	confess("BUG: $res unhandled") if ref($res) ne 'ARRAY';

	my $filter = blessed($res->[2]) && $res->[2]->can('attach') ?
			pop(@$res) : delete($env->{'qspawn.filter'});
	$filter //= PublicInbox::GzipFilter::qsp_maybe($res->[1], $env);

	if (scalar(@$res) == 3) { # done early (likely error or static file)
		delete $self->{rpipe};
		$ipipe->close if $ipipe;
		$wcb->($res); # all done
		return;
	}
	scalar(@$res) == 2 or confess("BUG: scalar(res) != 2: @$res");
	return ($wcb, $filter) if !$ipipe; # generic PSGI
	# streaming response
	my $qfh = $wcb->($res); # get PublicInbox::HTTP::(Chunked|Identity)
	$qfh = $filter->attach($qfh) if $filter;
	my ($bref) = @{delete $self->{yield_parse_hdr}};
	$qfh->write($$bref) if $$bref ne '';
	$self->{qfh} = $qfh; # keep $ipipe open
}

sub parse_hdr_done ($$) {
	my ($self) = @_;
	my ($ret, $err);
	if (defined $_[-1]) {
		my ($bref, $ph_cb, @ph_arg) = @{$self->{yield_parse_hdr}};
		$$bref .= $_[-1];
		$ret = eval { $ph_cb->(length($_[-1]), $bref, @ph_arg) };
		if (($err = $@)) {
			$ret = psgi_status_err();
		} elsif (!$ret && $_[-1] eq '') {
			$err = 'EOF';
			$ret = psgi_status_err();
		}
	} else {
		$err = "$!";
		$ret = psgi_status_err();
	}
	carp <<EOM if $err;
E: $err @{$self->{args}->[0]} ($self->{psgi_env}->{REQUEST_URI})
EOM
	$ret; # undef if headers incomplete
}

sub ipipe_cb { # InputPipe callback
	my ($ipipe, $self) = @_; # $_[-1] rbuf
	if ($self->{qfh}) { # already streaming
		yield_chunk($self, $ipipe, $_[-1]);
	} elsif (my $res = parse_hdr_done($self, $_[-1])) {
		yield_pass($self, $ipipe, $res);
	} # else: headers incomplete, keep reading
}

sub _yield_start { # may run later, much later...
	my ($self) = @_;
	if ($self->{psgi_env}->{'pi-httpd.async'}) {
		require PublicInbox::ProcessIONBF;
		my $rpipe = $self->{rpipe};
		PublicInbox::ProcessIONBF->replace($rpipe);
		PublicInbox::InputPipe::consume($rpipe, \&ipipe_cb, $self);
	} else {
		require PublicInbox::GetlineResponse;
		PublicInbox::GetlineResponse::response($self);
	}
}

# Used for streaming the stdout of one process as a PSGI response.
#
# $env is the PSGI env.
# optional keys in $env:
#   $env->{'qspawn.wcb'} - the write callback from the PSGI server
#                          optional, use this if you've already
#                          captured it elsewhere.  If not given,
#                          psgi_yield will return an anonymous
#                          sub for the PSGI server to call
#
#   $env->{'qspawn.filter'} - filter object, responds to ->attach for
#                             pi-httpd.async and ->translate for generic
#                             PSGI servers
#
# $limiter - the Limiter object to use (uses the def_limiter if not given)
#
# @parse_hdr_arg - Initial read cb+args; often for parsing CGI header output.
#              It will be given the return value of sysread from the pipe
#              and a string ref of the current buffer.  Returns an arrayref
#              for PSGI responses.  2-element arrays in PSGI mean the
#              body will be streamed, later, via writes (push-based) to
#              psgix.io.  3-element arrays means the body is available
#              immediately (or streamed via ->getline (pull-based)).

sub psgi_yield {
	my ($self, $env, $limiter, @parse_hdr_arg)= @_;
	$self->{psgi_env} = $env;
	$self->{yield_parse_hdr} = [ \(my $buf = ''), @parse_hdr_arg ];
	$limiter ||= $def_limiter ||= PublicInbox::Limiter->new(32);

	# the caller already captured the PSGI write callback from
	# the PSGI server, so we can call ->start, here:
	$env->{'qspawn.wcb'} ? start($self, $limiter, \&_yield_start) : sub {
		# the caller will return this sub to the PSGI server, so
		# it can set the response callback (that is, for
		# PublicInbox::HTTP, the chunked_wcb or identity_wcb callback),
		# but other HTTP servers are supported:
		$env->{'qspawn.wcb'} = $_[0];
		start($self, $limiter, \&_yield_start);
	}
}

no warnings 'once';
*DESTROY = \&finalize; # ->finalize is idempotent

1;
