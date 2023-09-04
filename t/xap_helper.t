#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
require_mods(qw(DBD::SQLite Search::Xapian));
my $msg = no_scm_rights;
plan(skip_all => $msg) if $msg; # TODO: FIFO support?
use PublicInbox::Spawn qw(spawn);
use Socket qw(AF_UNIX SOCK_SEQPACKET SOCK_STREAM);
require PublicInbox::AutoReap;
require PublicInbox::IPC;
require PublicInbox::XapClient;
use autodie;
my ($tmp, $for_destroy) = tmpdir();

my $fi_data = './t/git.fast-import-data';
open my $fi_fh, '<', $fi_data;
open my $dh, '<', '.';
my $crepo = create_coderepo 'for-cindex', sub {
	my ($d) = @_;
	xsys_e([qw(git init -q --bare)]);
	xsys_e([qw(git fast-import --quiet)], undef, { 0 => $fi_fh });
	chdir($dh);
	run_script([qw(-cindex --dangerous -L medium --no-fsync -q -j1), $d])
		or xbail '-cindex internal';
	run_script([qw(-cindex --dangerous -L medium --no-fsync -q -j3 -d),
		"$d/cidx-ext", $d]) or xbail '-cindex "external"';
};
$dh = $fi_fh = undef;

my $v2 = create_inbox 'v2', indexlevel => 'medium', version => 2,
			tmpdir => "$tmp/v2", sub {
	my ($im) = @_;
	for my $f (qw(t/data/0001.patch t/data/binary.patch
			t/data/message_embed.eml
			t/solve/0001-simple-mod.patch
			t/solve/0002-rename-with-modifications.patch
			t/solve/bare.patch)) {
		$im->add(eml_load($f)) or BAIL_OUT;
	}
};

my @ibx_idx = glob("$v2->{inboxdir}/xap*/?");
my (@int) = glob("$crepo/public-inbox-cindex/cidx*/?");
my (@ext) = glob("$crepo/cidx-ext/cidx*/?");
is(scalar(@ext), 2, 'have 2 external shards') or diag explain(\@ext);
is(scalar(@int), 1, 'have 1 internal shard') or diag explain(\@int);

my $doreq = sub {
	my ($s, @arg) = @_;
	my $err = pop @arg if ref($arg[-1]);
	pipe(my $x, my $y);
	my $buf = join("\0", @arg, '');
	my @fds = fileno($y);
	push @fds, fileno($err) if $err;
	my $n = PublicInbox::IPC::send_cmd($s, \@fds, $buf, 0);
	$n // xbail "send: $!";
	my $arg = "@arg";
	$arg =~ s/\Q$tmp\E/\$TMP/gs;
	is(length($buf), $n, "req $arg sent");
	$x;
};

my $env = { PERL5LIB => join(':', @INC) };
my $test = sub {
	my (@arg) = @_;
	socketpair(my $s, my $y, AF_UNIX, SOCK_SEQPACKET, 0);
	my $pid = spawn([$^X, '-w', @arg], $env, { 0 => $y });
	my $ar = PublicInbox::AutoReap->new($pid);
	diag "$arg[-1] running pid=$pid";
	close $y;
	my $r = $doreq->($s, qw(test_inspect -d), $ibx_idx[0]);
	my %info = map { split(/=/, $_, 2) } split(/ /, do { local $/; <$r> });
	is($info{has_threadid}, '1', 'has_threadid true for inbox');
	like($info{pid}, qr/\A\d+\z/, 'got PID from inbox inspect');

	$r = $doreq->($s, qw(test_inspect -d), $int[0]);
	my %cinfo = map { split(/=/, $_, 2) } split(/ /, do { local $/; <$r> });
	is($cinfo{has_threadid}, '0', 'has_threadid false for cindex');
	is($cinfo{pid}, $info{pid}, 'PID unchanged for cindex');

	my @dump = (qw(dump_ibx -A XDFID), (map { ('-d', $_) } @ibx_idx),
			qw(13 rt:0..));
	$r = $doreq->($s, @dump);
	my @res;
	while (sysread($r, my $buf, 512) != 0) { push @res, $buf }
	is(grep(/\n\z/s, @res), scalar(@res), 'line buffered');

	pipe(my $err_rd, my $err_wr);
	$r = $doreq->($s, @dump, $err_wr);
	close $err_wr;
	my $res = do { local $/; <$r> };
	is(join('', @res), $res, 'got identical response w/ error pipe');
	my $stats = do { local $/; <$err_rd> };
	is($stats, "mset.size=6 nr_out=6\n", 'mset.size reported');

	return $ar if $cinfo{pid} == $pid;

	# test worker management:
	kill('TERM', $cinfo{pid});
	my $tries = 0;
	do {
		$r = $doreq->($s, qw(test_inspect -d), $ibx_idx[0]);
		%info = map { split(/=/, $_, 2) }
			split(/ /, do { local $/; <$r> });
	} while ($info{pid} == $cinfo{pid} && ++$tries < 10);
	isnt($info{pid}, $cinfo{pid}, 'spawned new worker');

	my %pids;
	$tries = 0;
	my @ins = ($s, qw(test_inspect -d), $ibx_idx[0]);
	kill('TTIN', $pid);
	until (scalar(keys %pids) >= 2 || ++$tries > 10) {
		tick;
		my @r = map { $doreq->(@ins) } (0..5);
		for my $fh (@r) {
			my $buf = do { local $/; <$fh> } // die "read: $!";
			$buf =~ /\bpid=(\d+)/ and $pids{$1} = undef;
		}
	}
	is(scalar keys %pids, 2, 'have two pids');

	kill('TTOU', $pid);
	%pids = ();
	my $delay = $tries * 0.11 * ($ENV{VALGRIND} ? 10 : 1);
	$tries = 0;
	diag 'waiting '.$delay.'s for SIGTTOU';
	tick($delay);
	until (scalar(keys %pids) == 1 || ++$tries > 100) {
		%pids = ();
		my @r = map { $doreq->(@ins) } (0..5);
		for my $fh (@r) {
			my $buf = do { local $/; <$fh> } // die "read: $!";
			$buf =~ /\bpid=(\d+)/ and $pids{$1} = undef;
		}
	}
	is(scalar keys %pids, 1, 'have one pid') or diag explain(\%pids);
	is($info{pid}, (keys %pids)[0], 'kept oldest PID after TTOU');

	$ar;
};

my @NO_CXX = (1);
unless ($ENV{TEST_XH_CXX_ONLY}) {
	my $ar = $test->(qw[-MPublicInbox::XapHelper -e
			PublicInbox::XapHelper::start('-j0')]);
	$ar = $test->(qw[-MPublicInbox::XapHelper -e
			PublicInbox::XapHelper::start('-j1')]);
}
SKIP: {
	eval {
		require PublicInbox::XapHelperCxx;
		PublicInbox::XapHelperCxx::check_build();
	};
	skip "XapHelperCxx build: $@", 1 if $@ || $ENV{PI_NO_CXX};

	@NO_CXX = $ENV{TEST_XH_CXX_ONLY} ? (0) : (0, 1);
	my $ar = $test->(qw[-MPublicInbox::XapHelperCxx -e
			PublicInbox::XapHelperCxx::start('-j0')]);
	$ar = $test->(qw[-MPublicInbox::XapHelperCxx -e
			PublicInbox::XapHelperCxx::start('-j1')]);
};

require PublicInbox::CodeSearch;
my $cs_int = PublicInbox::CodeSearch->new("$crepo/public-inbox-cindex");
my $root2id_file = "$tmp/root2id";
my @id2root;
{
	open my $fh, '>', $root2id_file;
	my $i = -1;
	for ($cs_int->all_terms('G')) {
		print $fh $_, "\0", ++$i, "\0";
		$id2root[$i] = $_;
	}
	close $fh;
}

my $ar;
for my $n (@NO_CXX) {
	local $ENV{PI_NO_CXX} = $n;
	my ($xhc, $pid) = PublicInbox::XapClient::start_helper('-j0');
	$ar = PublicInbox::AutoReap->new($pid);
	pipe(my $err_r, my $err_w);

	# git patch-id --stable <t/data/0001.patch | awk '{print $1}'
	my $dfid = '91ee6b761fc7f47cad9f2b09b10489f313eb5b71';
	my $mid = '20180720072141.GA15957@example';
	my $r = $xhc->mkreq([ undef, $err_w ], qw(dump_ibx -A XDFID -A Q),
				(map { ('-d', $_) } @ibx_idx),
				9, "mid:$mid");
	close $err_w;
	my $res = do { local $/; <$r> };
	is($res, "$dfid 9\n$mid 9\n", "got expected result ($xhc->{impl})");
	my $err = do { local $/; <$err_r> };
	is($err, "mset.size=1 nr_out=2\n", "got expected status ($xhc->{impl})");

	pipe($err_r, $err_w);
	$r = $xhc->mkreq([ undef, $err_w ], qw(dump_roots -c -A XDFID),
			(map { ('-d', $_) } @int),
			$root2id_file, 'dt:19700101'.'000000..');
	close $err_w;
	my @res = <$r>;
	is(scalar(@res), 5, 'got expected rows');
	is(scalar(@res), scalar(grep(/\A[0-9a-f]{40,} [0-9]+\n\z/, @res)),
		'entries match format');
	$err = do { local $/; <$err_r> };
	is($err, "mset.size=6 nr_out=5\n", "got expected status ($xhc->{impl})");
}

done_testing;
