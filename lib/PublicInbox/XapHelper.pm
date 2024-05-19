# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Perl + SWIG||XS implementation if XapHelperCxx / xap_helper.h isn't usable.
package PublicInbox::XapHelper;
use v5.12;
use Getopt::Long (); # good API even if we only use short options
our $GLP = Getopt::Long::Parser->new;
$GLP->configure(qw(require_order bundling no_ignore_case no_auto_abbrev));
use PublicInbox::Search qw(xap_terms);
use PublicInbox::CodeSearch;
use PublicInbox::IPC;
use PublicInbox::IO qw(read_all);
use Socket qw(SOL_SOCKET SO_TYPE SOCK_SEQPACKET AF_UNIX);
use PublicInbox::DS qw(awaitpid);
use autodie qw(open getsockopt);
use POSIX qw(:signal_h);
use Fcntl qw(LOCK_UN LOCK_EX);
use Carp qw(croak);
my $X = \%PublicInbox::Search::X;
our (%SRCH, %WORKERS, $nworker, $workerset, $in, $SHARD_NFD, $MY_FD_MAX);
our $stderr = \*STDERR;

sub cmd_test_inspect {
	my ($req) = @_;
	print { $req->{0} } "pid=$$ has_threadid=",
		($req->{srch}->has_threadid ? 1 : 0)
}

sub cmd_test_sleep { select(undef, undef, undef, 0.01) while 1 }

sub iter_retry_check ($) {
	if (ref($@) =~ /\bDatabaseModifiedError\b/) {
		$_[0]->{srch}->reopen;
		undef; # retries
	} elsif (ref($@) =~ /\bDocNotFoundError\b/) {
		warn "doc not found: $@";
		0; # continue to next doc
	} else {
		die;
	}
}

sub term_length_extract ($) {
	my ($req) = @_;
	@{$req->{A_len}} = map {
		my $len = s/([0-9]+)\z// ? ($1 + 0) : undef;
		[ $_, $len ];
	} @{$req->{A}};
}

sub dump_ibx_iter ($$$) {
	my ($req, $ibx_id, $it) = @_;
	my $out = $req->{0};
	eval {
		my $doc = $it->get_document;
		for my $pair (@{$req->{A_len}}) {
			my ($pfx, $len) = @$pair;
			my @t = xap_terms($pfx, $doc);
			@t = grep { length == $len } @t if defined($len);
			for (@t) {
				print $out "$_ $ibx_id\n" or die "print: $!";
				++$req->{nr_out};
			}
		}
	};
	$@ ? iter_retry_check($req) : 0;
}

sub emit_mset_stats ($$) {
	my ($req, $mset) = @_;
	my $err = $req->{1} or croak "BUG: caller only passed 1 FD";
	say $err 'mset.size='.$mset->size.' nr_out='.$req->{nr_out}
}

sub cmd_dump_ibx {
	my ($req, $ibx_id, $qry_str) = @_;
	$qry_str // die 'usage: dump_ibx [OPTIONS] IBX_ID QRY_STR';
	$req->{A} or die 'dump_ibx requires -A PREFIX';
	term_length_extract $req;
	my $max = $req->{'m'} // $req->{srch}->{xdb}->get_doccount;
	my $opt = { relevance => -1, limit => $max, offset => $req->{o} // 0 };
	$opt->{eidx_key} = $req->{O} if defined $req->{O};
	my $mset = $req->{srch}->mset($qry_str, $opt);
	$req->{0}->autoflush(1);
	for my $it ($mset->items) {
		for (my $t = 10; $t > 0; --$t) {
			$t = dump_ibx_iter($req, $ibx_id, $it) // $t;
		}
	}
	emit_mset_stats($req, $mset);
}

sub dump_roots_iter ($$$) {
	my ($req, $root2off, $it) = @_;
	eval {
		my $doc = $it->get_document;
		my $G = join(' ', map { $root2off->{$_} } xap_terms('G', $doc));
		for my $pair (@{$req->{A_len}}) {
			my ($pfx, $len) = @$pair;
			my @t = xap_terms($pfx, $doc);
			@t = grep { length == $len } @t if defined($len);
			for (@t) {
				$req->{wbuf} .= "$_ $G\n";
				++$req->{nr_out};
			}
		}
	};
	$@ ? iter_retry_check($req) : 0;
}

sub dump_roots_flush ($$) {
	my ($req, $fh) = @_;
	if ($req->{wbuf} ne '') {
		until (flock($fh, LOCK_EX)) { die "LOCK_EX: $!" if !$!{EINTR} }
		print { $req->{0} } $req->{wbuf} or die "print: $!";
		until (flock($fh, LOCK_UN)) { die "LOCK_UN: $!" if !$!{EINTR} }
		$req->{wbuf} = '';
	}
}

sub cmd_dump_roots {
	my ($req, $root2off_file, $qry_str) = @_;
	$qry_str // die 'usage: dump_roots [OPTIONS] ROOT2ID_FILE QRY_STR';
	$req->{A} or die 'dump_roots requires -A PREFIX';
	term_length_extract $req;
	open my $fh, '<', $root2off_file;
	my $root2off; # record format: $OIDHEX "\0" uint32_t
	my @x = split(/\0/, read_all $fh);
	while (defined(my $oidhex = shift @x)) {
		$root2off->{$oidhex} = shift @x;
	}
	my $opt = { relevance => -1, limit => $req->{'m'},
			offset => $req->{o} // 0 };
	my $mset = $req->{srch}->mset($qry_str, $opt);
	$req->{0}->autoflush(1);
	$req->{wbuf} = '';
	for my $it ($mset->items) {
		for (my $t = 10; $t > 0; --$t) {
			$t = dump_roots_iter($req, $root2off, $it) // $t;
		}
		if (!($req->{nr_out} & 0x3fff)) {
			dump_roots_flush($req, $fh);
		}
	}
	dump_roots_flush($req, $fh);
	emit_mset_stats($req, $mset);
}

sub mset_iter ($$) {
	my ($req, $it) = @_;
	say { $req->{0} } $it->get_docid, "\0",
			$it->get_percent, "\0", $it->get_rank;
}

sub cmd_mset { # to be used by WWW + IMAP
	my ($req, $qry_str) = @_;
	$qry_str // die 'usage: mset [OPTIONS] QRY_STR';
	my $opt = { limit => $req->{'m'}, offset => $req->{o} // 0 };
	$opt->{relevance} = 1 if $req->{r};
	$opt->{threads} = 1 if defined $req->{t};
	$opt->{git_dir} = $req->{g} if defined $req->{g};
	$opt->{eidx_key} = $req->{O} if defined $req->{O};
	$opt->{threadid} = $req->{T} if defined $req->{T};
	my $mset = $req->{srch}->mset($qry_str, $opt);
	say { $req->{0} } 'mset.size=', $mset->size,
		' .get_matches_estimated=', $mset->get_matches_estimated;
	for my $it ($mset->items) {
		for (my $t = 10; $t > 0; --$t) {
			$t = mset_iter($req, $it) // $t;
		}
	}
}

sub srch_init_extra ($) {
	my ($req) = @_;
	my $qp = $req->{srch}->{qp};
	for (@{$req->{Q}}) {
		my ($upfx, $m, $xpfx) = split /([:=])/;
		$xpfx // die "E: bad -Q $_";
		$m = $m eq '=' ? 'add_boolean_prefix' : 'add_prefix';
		$qp->$m($upfx, $xpfx);
	}
	$req->{srch}->{qp_extra_done} = 1;
}

sub dispatch {
	my ($req, $cmd, @argv) = @_;
	my $fn = $req->can("cmd_$cmd") or return;
	$GLP->getoptionsfromarray(\@argv, $req, @PublicInbox::Search::XH_SPEC)
		or return;
	my $dirs = delete $req->{d} or die 'no -d args';
	my $key = "-d\0".join("\0-d\0", @$dirs);
	$key .= "\0".join("\0", map { ('-Q', $_) } @{$req->{Q}}) if $req->{Q};
	my $new;
	$req->{srch} = $SRCH{$key} // do {
		$new = { qp_flags => $PublicInbox::Search::QP_FLAGS };
		my $nfd = scalar(@$dirs) * PublicInbox::Search::SHARD_COST;
		$SHARD_NFD += $nfd;
		if ($SHARD_NFD > $MY_FD_MAX) {
			$SHARD_NFD = $nfd;
			%SRCH = ();
		}
		my $first = shift @$dirs;
		for my $retried (0, 1) {
			my $slow_phrase = -f "$first/iamchert";
			eval {
				$new->{xdb} = $X->{Database}->new($first);
				for (@$dirs) {
					$slow_phrase ||= -f "$_/iamchert";
					$new->{xdb}->add_database(
							$X->{Database}->new($_))
				}
			};
			last unless $@;
			if ($retried) {
				die "E: $@\n";
			} else { # may be EMFILE/ENFILE/ENOMEM....
				warn "W: $@, retrying...\n";
				%SRCH = ();
				$SHARD_NFD = $nfd;
			}
			$slow_phrase or $new->{qp_flags}
				|= PublicInbox::Search::FLAG_PHRASE();
		}
		bless $new, $req->{c} ? 'PublicInbox::CodeSearch' :
					'PublicInbox::Search';
		$new->{qp} = $new->qparse_new;
		$SRCH{$key} = $new;
	};
	$req->{srch}->{xdb}->reopen unless $new;
	$req->{Q} && !$req->{srch}->{qp_extra_done} and
		srch_init_extra $req;
	my $timeo = $req->{K};
	alarm($timeo) if $timeo;
	$fn->($req, @argv);
	alarm(0) if $timeo;
}

sub recv_loop {
	local $SIG{__WARN__} = sub { print $stderr @_ };
	my $rbuf;
	local $SIG{TERM} = sub { undef $in };
	local $SIG{USR1} = \&reopen_logs;
	while (defined($in)) {
		PublicInbox::DS::sig_setmask($workerset);
		my @fds = eval { # we undef $in in SIG{TERM}
			$PublicInbox::IPC::recv_cmd->($in, $rbuf, 4096*33)
		};
		if ($@) {
			exit if !$in; # hit by SIGTERM
			die;
		}
		scalar(@fds) or exit(66); # EX_NOINPUT
		die "recvmsg: $!" if !defined($fds[0]);
		PublicInbox::DS::block_signals(POSIX::SIGALRM);
		my $req = bless {}, __PACKAGE__;
		my $i = 0;
		open($req->{$i++}, '+<&=', $_) for @fds;
		local $stderr = $req->{1} // \*STDERR;
		die "not NUL-terminated" if chop($rbuf) ne "\0";
		my @argv = split(/\0/, $rbuf);
		$req->{nr_out} = 0;
		$req->dispatch(@argv) if @argv;
	}
}

sub reap_worker { # awaitpid CB
	my ($pid, $nr) = @_;
	delete $WORKERS{$nr};
	if (($? >> 8) == 66) { # EX_NOINPUT
		undef $in;
	} elsif ($?) {
		warn "worker[$nr] died \$?=$?\n";
	}
	PublicInbox::DS::requeue(\&start_workers) if $in;
}

sub start_worker ($) {
	my ($nr) = @_;
	my $pid = eval { PublicInbox::DS::fork_persist } // return(warn($@));
	if ($pid == 0) {
		undef %WORKERS;
		$SIG{TTIN} = $SIG{TTOU} = 'IGNORE';
		$SIG{CHLD} = 'DEFAULT'; # Xapian may use this
		recv_loop();
		exit(0);
	} else {
		$WORKERS{$nr} = $pid;
		awaitpid($pid, \&reap_worker, $nr);
	}
}

sub start_workers {
	for my $nr (grep { !defined($WORKERS{$_}) } (0..($nworker - 1))) {
		start_worker($nr) if $in;
	}
}

sub do_sigttou {
	if ($in && $nworker > 1) {
		--$nworker;
		my @nr = grep { $_ >= $nworker } keys %WORKERS;
		kill('TERM', @WORKERS{@nr});
	}
}

sub reopen_logs {
	my $p = $ENV{STDOUT_PATH};
	defined($p) && open(STDOUT, '>>', $p) and STDOUT->autoflush(1);
	$p = $ENV{STDERR_PATH};
	defined($p) && open(STDERR, '>>', $p) and STDERR->autoflush(1);
}

sub parent_reopen_logs {
	reopen_logs();
	kill('USR1', values %WORKERS);
}

sub xh_alive { $in || scalar(keys %WORKERS) }

sub start (@) {
	my (@argv) = @_;
	my $c = getsockopt(local $in = \*STDIN, SOL_SOCKET, SO_TYPE);
	unpack('i', $c) == SOCK_SEQPACKET or die 'stdin is not SOCK_SEQPACKET';

	local (%SRCH, %WORKERS, $SHARD_NFD, $MY_FD_MAX);
	PublicInbox::Search::load_xapian();
	$GLP->getoptionsfromarray(\@argv, my $opt = { j => 1 }, 'j=i') or
		die 'bad args';
	local $workerset = POSIX::SigSet->new;
	$workerset->fillset or die "fillset: $!";
	for (@PublicInbox::DS::UNBLOCKABLE, POSIX::SIGUSR1) {
		$workerset->delset($_) or die "delset($_): $!";
	}
	$MY_FD_MAX = PublicInbox::Search::ulimit_n //
		die "E: unable to get RLIMIT_NOFILE: $!";
	warn "W: RLIMIT_NOFILE=$MY_FD_MAX too low\n" if $MY_FD_MAX < 72;
	$MY_FD_MAX -= 64;

	local $nworker = $opt->{j};
	return recv_loop() if $nworker == 0;
	die '-j must be >= 0' if $nworker < 0;
	for (POSIX::SIGTERM, POSIX::SIGCHLD) {
		$workerset->delset($_) or die "delset($_): $!";
	}
	my $sig = {
		TTIN => sub {
			if ($in) {
				++$nworker;
				PublicInbox::DS::requeue(\&start_workers)
			}
		},
		TTOU => \&do_sigttou,
		CHLD => \&PublicInbox::DS::enqueue_reap,
		USR1 => \&parent_reopen_logs,
	};
	PublicInbox::DS::block_signals();
	start_workers();
	@PublicInbox::DS::post_loop_do = \&xh_alive;
	PublicInbox::DS::event_loop($sig);
}

1;
