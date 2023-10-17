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
use PublicInbox::Git qw(read_all);
use Socket qw(SOL_SOCKET SO_TYPE SOCK_SEQPACKET AF_UNIX);
use PublicInbox::DS qw(awaitpid);
use autodie qw(open);
use POSIX qw(:signal_h);
use Fcntl qw(LOCK_UN LOCK_EX);
my $X = \%PublicInbox::Search::X;
our (%SRCH, %WORKERS, $parent_pid, $alive, $nworker, $workerset);
our $stderr = \*STDERR;

# only short options for portability in C++ implementation
our @SPEC = (
	'a', # ascending sort
	'c', # code search
	'd=s@', # shard dirs
	'k=i', # sort column (like sort(1))
	'm=i', # maximum number of results
	'o=i', # offset
	'r', # 1=relevance then column
	't', # collapse threads
	'A=s@', # prefixes
	'O=s', # eidx_key
	'T=i', # timeout in seconds
);

sub cmd_test_inspect {
	my ($req) = @_;
	print { $req->{0} } "pid=$$ has_threadid=",
		($req->{srch}->has_threadid ? 1 : 0)
}

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

sub dump_ibx_iter ($$$) {
	my ($req, $ibx_id, $it) = @_;
	my $out = $req->{0};
	eval {
		my $doc = $it->get_document;
		for my $p (@{$req->{A}}) {
			for (xap_terms($p, $doc)) {
				print $out "$_ $ibx_id\n" or die "print: $!";
				++$req->{nr_out};
			}
		}
	};
	$@ ? iter_retry_check($req) : 0;
}

sub emit_mset_stats ($$) {
	my ($req, $mset) = @_;
	my $err = $req->{1} or return;
	say $err 'mset.size='.$mset->size.' nr_out='.$req->{nr_out}
}

sub cmd_dump_ibx {
	my ($req, $ibx_id, $qry_str) = @_;
	$qry_str // return warn('usage: dump_ibx [OPTIONS] IBX_ID QRY_STR');
	$req->{A} or return warn('dump_ibx requires -A PREFIX');
	my $max = $req->{srch}->{xdb}->get_doccount;
	my $opt = { relevance => -1, limit => $max, offset => $req->{o} // 0 };
	$opt->{eidx_key} = $req->{O} if defined $req->{O};
	my $mset = $req->{srch}->mset($qry_str, $opt);
	$req->{0}->autoflush(1);
	for my $it ($mset->items) {
		for (my $t = 10; $t > 0; --$t) {
			$t = dump_ibx_iter($req, $ibx_id, $it) // $t;
		}
	}
	if (my $err = $req->{1}) {
		say $err 'mset.size='.$mset->size.' nr_out='.$req->{nr_out}
	}
}

sub dump_roots_iter ($$$) {
	my ($req, $root2id, $it) = @_;
	eval {
		my $doc = $it->get_document;
		my $G = join(' ', map { $root2id->{$_} } xap_terms('G', $doc));
		for my $p (@{$req->{A}}) {
			for (xap_terms($p, $doc)) {
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
	my ($req, $root2id_file, $qry_str) = @_;
	$qry_str // return
		warn('usage: dump_roots [OPTIONS] ROOT2ID_FILE QRY_STR');
	$req->{A} or return warn('dump_roots requires -A PREFIX');
	open my $fh, '<', $root2id_file;
	my $root2id; # record format: $OIDHEX "\0" uint32_t
	my @x = split(/\0/, read_all($fh));
	while (@x) {
		my $oidhex = shift @x;
		$root2id->{$oidhex} = shift @x;
	}
	my $opt = { relevance => -1, limit => $req->{'m'},
			offset => $req->{o} // 0 };
	my $mset = $req->{srch}->mset($qry_str, $opt);
	$req->{0}->autoflush(1);
	$req->{wbuf} = '';
	for my $it ($mset->items) {
		for (my $t = 10; $t > 0; --$t) {
			$t = dump_roots_iter($req, $root2id, $it) // $t;
		}
		if (!($req->{nr_out} & 0x3fff)) {
			dump_roots_flush($req, $fh);
		}
	}
	dump_roots_flush($req, $fh);
	emit_mset_stats($req, $mset);
}

sub dispatch {
	my ($req, $cmd, @argv) = @_;
	my $fn = $req->can("cmd_$cmd") or return;
	$GLP->getoptionsfromarray(\@argv, $req, @SPEC) or return;
	my $dirs = delete $req->{d} or return warn 'no -d args';
	my $key = join("\0", @$dirs);
	$req->{srch} = $SRCH{$key} //= do {
		my $new = { qp_flags => $PublicInbox::Search::QP_FLAGS };
		my $first = shift @$dirs;
		my $slow_phrase = -f "$first/iamchert";
		$new->{xdb} = $X->{Database}->new($first);
		for (@$dirs) {
			$slow_phrase ||= -f "$_/iamchert";
			$new->{xdb}->add_database($X->{Database}->new($_));
		}
		$slow_phrase or
			$new->{qp_flags} |= PublicInbox::Search::FLAG_PHRASE();
		bless $new, $req->{c} ? 'PublicInbox::CodeSearch' :
					'PublicInbox::Search';
		$new->{qp} = $new->qparse_new;
		$new;
	};
	eval { $fn->($req, @argv) };
	warn "E: $@" if $@;
}

sub recv_loop {
	local $SIG{__WARN__} = sub { print $stderr @_ };
	my $rbuf;
	my $in = \*STDIN;
	while (!defined($parent_pid) || getppid == $parent_pid) {
		PublicInbox::DS::sig_setmask($workerset);
		my @fds = $PublicInbox::IPC::recv_cmd->($in, $rbuf, 4096*33);
		scalar(@fds) or exit(66); # EX_NOINPUT
		die "recvmsg: $!" if !defined($fds[0]);
		PublicInbox::DS::block_signals();
		my $req = bless {}, __PACKAGE__;
		my $i = 0;
		open($req->{$i++}, '+<&=', $_) for @fds;
		local $stderr = $req->{1} // \*STDERR;
		if (chop($rbuf) ne "\0") {
			warn "not NUL-terminated";
			next;
		}
		my @argv = split(/\0/, $rbuf);
		$req->{nr_out} = 0;
		eval { $req->dispatch(@argv) } if @argv;
	}
}

sub reap_worker { # awaitpid CB
	my ($pid, $nr) = @_;
	delete $WORKERS{$nr};
	if (($? >> 8) == 66) { # EX_NOINPUT
		$alive = undef;
	} elsif ($?) {
		warn "worker[$nr] died \$?=$?\n";
	}
	PublicInbox::DS::requeue(\&start_workers) if $alive;
}

sub start_worker ($) {
	my ($nr) = @_;
	my $pid = fork;
	if (!defined($pid)) {
		warn("fork: $!");
		return undef;
	};
	if ($pid == 0) {
		undef %WORKERS;
		PublicInbox::DS::Reset();
		$SIG{TERM} = sub { $parent_pid = -1 };
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
		start_worker($nr) if $alive;
	}
}

sub do_sigttou {
	if ($alive && $nworker > 1) {
		--$nworker;
		my @nr = grep { $_ >= $nworker } keys %WORKERS;
		kill('TERM', @WORKERS{@nr});
	}
}

sub xh_alive { $alive || scalar(keys %WORKERS) }

sub start (@) {
	my (@argv) = @_;
	my $c = getsockopt(STDIN, SOL_SOCKET, SO_TYPE) or die "getsockopt: $!";
	unpack('i', $c) == SOCK_SEQPACKET or die 'stdin is not SOCK_SEQPACKET';

	local (%SRCH, %WORKERS);
	local $alive = 1;
	PublicInbox::Search::load_xapian();
	$GLP->getoptionsfromarray(\@argv, my $opt = { j => 1 }, 'j=i') or
		die 'bad args';
	local $workerset = POSIX::SigSet->new;
	$workerset->fillset or die "fillset: $!";
	for (@PublicInbox::DS::UNBLOCKABLE) {
		$workerset->delset($_) or die "delset($_): $!";
	}

	local $nworker = $opt->{j};
	return recv_loop() if $nworker == 0;
	die '-j must be >= 0' if $nworker < 0;
	for (POSIX::SIGTERM, POSIX::SIGCHLD) {
		$workerset->delset($_) or die "delset($_): $!";
	}
	local $parent_pid = $$;
	my $sig = {
		TTIN => sub {
			if ($alive) {
				++$nworker;
				PublicInbox::DS::requeue(\&start_workers)
			}
		},
		TTOU => \&do_sigttou,
		CHLD => \&PublicInbox::DS::enqueue_reap,
	};
	PublicInbox::DS::block_signals();
	start_workers();
	@PublicInbox::DS::post_loop_do = \&xh_alive;
	PublicInbox::DS::event_loop($sig);
}

1;
