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
use Fcntl qw(LOCK_UN LOCK_EX);
my $X = \%PublicInbox::Search::X;
our (%SRCH, %PIDS, $parent_pid);
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

sub cmd_dump_ibx {
	my ($req, $ibx_id, $qry_str) = @_;
	$qry_str // return warn('usage: dump_ibx [OPTIONS] IBX_ID QRY_STR');
	my @pfx = @{$req->{A}} or return warn('dump_ibx requires -A PREFIX');
	my $max = $req->{srch}->{xdb}->get_doccount;
	my $opt = { relevance => -1, limit => $max, offset => $req->{o} // 0 };
	$opt->{eidx_key} = $req->{O} if defined $req->{O};
	my $mset = $req->{srch}->mset($qry_str, $opt);
	my $out = $req->{0};
	$out->autoflush(1);
	my $nr = 0;
	for my $it ($mset->items) {
		my $doc = $it->get_document;
		for my $p (@pfx) {
			for (xap_terms($p, $doc)) {
				print $out "$_ $ibx_id\n" or die "print: $!";
				++$nr;
			}
		}
	}
	if (my $err = $req->{1}) {
		say $err 'mset.size='.$mset->size.' nr_out='.$nr
	}
}

sub cmd_dump_roots {
	my ($req, $root2id_file, $qry_str) = @_;
	$qry_str // return
		warn('usage: dump_roots [OPTIONS] ROOT2ID_FILE QRY_STR');
	my @pfx = @{$req->{A}} or return warn('dump_roots requires -A PREFIX');
	open my $fh, '<', $root2id_file or die "open($root2id_file): $!";
	my %root2id; # record format: $OIDHEX "\0" uint32_t
	my @x = split(/\0/, do { local $/; <$fh> } // die "readline: $!");
	while (@x) {
		my $oidhex = shift @x;
		$root2id{$oidhex} = shift @x;
	}
	my $opt = { relevance => -1, limit => $req->{'m'},
			offset => $req->{o} // 0 };
	my $mset = $req->{srch}->mset($qry_str, $opt);
	$req->{0}->autoflush(1);
	my $buf = '';
	my $nr = 0;
	for my $it ($mset->items) {
		my $doc = $it->get_document;
		my $G = join(' ', map { $root2id{$_} } xap_terms('G', $doc));
		for my $p (@pfx) {
			for (xap_terms($p, $doc)) {
				$buf .= "$_ $G\n";
				++$nr;
			}
		}
		if (!($nr & 0x3fff)) {
			flock($fh, LOCK_EX) or die "flock: $!";
			print { $req->{0} } $buf or die "print: $!";
			flock($fh, LOCK_UN) or die "flock: $!";
			$buf = '';
		}
	}
	if ($buf ne '') {
		flock($fh, LOCK_EX) or die "flock: $!";
		print { $req->{0} } $buf or die "print: $!";
		flock($fh, LOCK_UN) or die "flock: $!";
	}
	if (my $err = $req->{1}) {
		say $err 'mset.size='.$mset->size.' nr_out='.$nr
	}
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
	while (!defined($parent_pid) || getppid != $parent_pid) {
		my $req = bless {}, __PACKAGE__;
		my @fds = PublicInbox::IPC::recv_cmd(\*STDIN, $rbuf, 4096*33);
		scalar(@fds) or exit(66); # EX_NOINPUT
		$fds[0] // die "recvmsg: $!";
		my $i = 0;
		for my $fd (@fds) {
			open($req->{$i++}, '+<&=', $fd) and next;
			warn("open(+<&=$fd) (FD=$i): $!");
			undef $req;
			last;
		}
		$req or next;
		local $stderr = $req->{1} // \*STDERR;
		if (chop($rbuf) ne "\0") {
			warn "not NUL-terminated";
			next;
		}
		my @argv = split(/\0/, $rbuf);
		eval { $req->dispatch(@argv) } if @argv;
	}
}

sub start_worker ($) {
	my ($nr) = @_;
	my $pid = fork // return warn("fork: $!");
	if ($pid == 0) {
		undef %PIDS;
		recv_loop();
		exit(0);
	} else {
		$PIDS{$pid} = $nr;
	}
}

sub start (@) {
	my (@argv) = @_;
	local (%SRCH, %PIDS, $parent_pid);
	PublicInbox::Search::load_xapian();
	$GLP->getoptionsfromarray(\@argv, my $opt = { j => 1 }, 'j=i') or
		die 'bad args';
	return recv_loop() if !$opt->{j};
	die '-j must be >= 0' if $opt->{j} < 0;
	start_worker($_) for (1..($opt->{j}));

	my $quit;
	until ($quit) {
		my $p = waitpid(-1, 0) or return;
		if (defined(my $nr = delete $PIDS{$p})) {
			$quit = 1 if ($? >> 8) == 66; # EX_NOINPUT
			start_worker($nr) if !$quit;
		} else {
			warn "W: unknown pid=$p reaped\n";
		}
	}
}

1;
