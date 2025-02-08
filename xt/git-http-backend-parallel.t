#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# ensure publicinboxlimiter.-httpbackend.{depth,max} knobs work
# and returns 503 (too busy) errors on overload.
use v5.12;
use autodie;
use PublicInbox::TestCommon;
use File::Path qw(remove_tree);
use PublicInbox::IO qw(write_file try_cat);
use PublicInbox::Spawn qw(spawn);
use PublicInbox::Git qw(git_exe);
require PublicInbox::Sigfd;
my $git_dir = $ENV{GIANT_GIT_DIR} //
	plan 'skip_all' => 'GIANT_GIT_DIR not defined';
require_mods qw(-httpd psgi);
my $tmpdir = tmpdir;
my $henv = { TMPDIR => $tmpdir, PI_CONFIG => "$tmpdir/cfg" };
write_file '>', $henv->{PI_CONFIG}, <<EOM;
[coderepo "giant.git"]
	dir = $git_dir
EOM
my @tail = split ' ', $PublicInbox::TestCommon::tail_cmd // '';
my $uri;

my $run_clones = sub {
	my ($max) = @_;
	my (%wait, $tpid, %chld_status, @f);
	if (@tail) {
		my @f = map {
			my $f = "$tmpdir/$_.err";
			open my $fh, '>', $f;
			$f;
		} (1..$max);
		$tpid = spawn([ @tail, @f ], undef, { 1 => 2 });
	}
	for my $n (1..$max) {
		my $rdr;
		my $err = "$tmpdir/$n.err";
		open $rdr->{2}, '>', $err;
		my $pid = spawn([git_exe, qw(clone -q --mirror), $uri,
				"$tmpdir/$n.git"], undef, $rdr);
		$wait{$pid} = $n;
	}
	while (keys %wait) {
		my $pid = waitpid(-1, 0);
		my $n = delete $wait{$pid} // next;
		push @{$chld_status{$?}}, $n;
		remove_tree "$tmpdir/$n.git";
	}
	if ($tpid) {
		kill 'TERM', $tpid;
		waitpid($tpid, 0);
	}
	\%chld_status;
};

my $sigfd = PublicInbox::Sigfd->new;
my $reload_cfg = sub {
	write_file '>>', $henv->{PI_CONFIG}, @_;
	kill 'HUP', $PublicInbox::TestCommon::CURRENT_DAEMON->{pid};
	# signalfd/EVFILT_SIGNAL platforms should handle signals more
	# predictably and not need tick
	tick(1) unless $sigfd;
};

my $ck_503 = sub {
	my ($chld_status) = @_;
	ok scalar(keys %$chld_status), 'got non-zero statuses from git clone';
	for my $s (keys %$chld_status) {
		my @unexpected;
		for my $n (@{$chld_status->{$s}}) {
			my $msg = try_cat "$tmpdir/$n.err";
			push @unexpected, $msg if $msg !~ /\b503\b/s;
		}
		ok !@unexpected, 'no unexpected errors' or
			diag explain([ "status $s", \@unexpected ]);
		diag "chld_status=$s: ".scalar(@{$chld_status->{$s}})
			.' instances'
	}
};

my $client = sub {
	$uri = "$ENV{PLACK_TEST_EXTERNALSERVER_URI}/giant.git/";
	my $chld_status = $run_clones->(64);
	my $ok = delete $chld_status->{0} // [];
	is scalar(@$ok), 64, 'got all successes';
	is keys(%$chld_status), 0, 'no other statuses' or
		diag explain($chld_status);

	$reload_cfg->(<<EOM);
[publicinboxlimiter "-httpbackend"]
	depth = 1
EOM

	$chld_status = $run_clones->(40);
	$ok = delete $chld_status->{0} // [];
	ok scalar(@$ok) >= 33, 'got at least 33 successes' or
		diag 'got '.scalar(@$ok).' successes';
	$ck_503->($chld_status);

	$reload_cfg->("\tmax = 1\n");

	$chld_status = $run_clones->(10);
	$ok = delete $chld_status->{0} // [];
	ok scalar(@$ok) >= 2, 'got at least 2 successes' or
		diag 'got '.scalar(@$ok).' successes';
	$ck_503->($chld_status);
};
test_httpd $henv, $client;
done_testing;
