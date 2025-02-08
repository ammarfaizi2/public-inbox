#!perl -w
# Copyright (C)  all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use Cwd qw(abs_path);
require_git v2.6;
use PublicInbox::ContentHash qw(git_sha);
use PublicInbox::Spawn qw(run_qx which);
use File::Path qw(remove_tree);
use PublicInbox::IO qw(write_file try_cat);
use autodie qw(close kill mkdir open read rename symlink unlink);
require_mods qw(DBD::SQLite Xapian);
require PublicInbox::SolverGit;
my $rdr = { 2 => \(my $null) };
my $git_dir = xqx([qw(git rev-parse --git-common-dir)], undef, $rdr);
$git_dir = xqx([qw(git rev-parse --git-dir)], undef, $rdr) if $? != 0;
$? == 0 or plan skip_all => "$0 must be run from a git working tree";
chomp $git_dir;

# needed for alternates, and --absolute-git-dir is only in git 2.13+
$git_dir = abs_path($git_dir);

my $patch2 = eml_load 't/solve/0002-rename-with-modifications.patch';
my $patch2_oid = git_sha(1, $patch2)->hexdigest;

my ($tmpdir, $for_destroy) = tmpdir();
my $gone_repo = "$tmpdir/to-be-deleted.git";

my $ibx = create_inbox 'v2', version => 2,
			indexlevel => 'medium', sub {
	my ($im) = @_;
	$im->add(eml_load 't/solve/0001-simple-mod.patch') or BAIL_OUT;
	$im->add($patch2) or BAIL_OUT;
};
my $md = "$tmpdir/md";
File::Path::make_path(map { $md.$_ } (qw(/cur /new /tmp)));
symlink abs_path('t/solve/0001-simple-mod.patch'), "$md/cur/foo:2,";

my $v1_0_0_rev = '8a918a8523bc9904123460f85999d75f6d604916';
my $v1_0_0_tag = 'cb7c42b1e15577ed2215356a2bf925aef59cdd8d';
my $v1_0_0_tag_short = substr($v1_0_0_tag, 0, 16);
my $expect = '69df7d565d49fbaaeb0a067910f03dc22cd52bd0';
my $non_existent = 'ee5e32211bf62ab6531bdf39b84b6920d0b6775a';
my $stderr_empty = sub {
	no_httpd_errors "$tmpdir/stderr.log",
		$_[0] // 'stderr.log is empty';
};

test_lei({tmpdir => "$tmpdir/blob"}, sub {
	lei_ok('blob', '--mail', $patch2_oid, '-I', $ibx->{inboxdir},
		\'--mail works for existing oid');
	is($lei_out, $patch2->as_string, 'blob matches');
	ok(!lei('blob', '--mail', '69df7d5', '-I', $ibx->{inboxdir}),
		"--mail won't run solver");
	like($lei_err, qr/\b69df7d5\b/, 'OID in error by git(1)');

	lei_ok('blob', '69df7d5', '-I', $ibx->{inboxdir});
	is(git_sha(1, \$lei_out)->hexdigest, $expect, 'blob contents output');
	my $prev = $lei_out;
	lei_ok(qw(blob --no-mail 69df7d5 -I), $ibx->{inboxdir});
	is($lei_out, $prev, '--no-mail works');
	ok(!lei(qw(blob -I), $ibx->{inboxdir}, $non_existent),
			'non-existent blob fails');
	my $abbrev = substr($non_existent, 0, 7);
	like($lei_err, qr/could not find $abbrev/, 'failed abbreviation noted');
	SKIP: {
		skip '/.git exists', 1 if -e '/.git';
		lei_ok(qw(-C / blob 69df7d5 -I), $ibx->{inboxdir},
			"--git-dir=$git_dir");
		is($lei_out, $prev, '--git-dir works');

		ok(!lei(qw(-C / blob --no-cwd 69df7d5 -I), $ibx->{inboxdir}),
			'--no-cwd works');
		like($lei_err, qr/no --git-dir to try/,
			'lack of --git-dir noted');

		ok(!lei(qw(-C / blob -I), $ibx->{inboxdir}, $non_existent),
			'non-existent blob fails');
		like($lei_err, qr/no --git-dir to try/,
			'lack of --git-dir noted');
	}

	# fallbacks
	lei_ok('blob', $v1_0_0_tag, '-I', $ibx->{inboxdir});
	lei_ok('blob', $v1_0_0_tag_short, '-I', $ibx->{inboxdir});
});

test_lei({tmpdir => "$tmpdir/rediff"}, sub {
	lei_ok(qw(rediff -q -U9 t/solve/0001-simple-mod.patch));
	like($lei_out, qr!^\Q+++\E b/TODO\n@@ -103,9 \+103,11 @@!sm,
		'got more context with -U9');

	my (undef, $re) = split(/\n\n/, $lei_out, 2);
	$re =~ s/^/> /sgm;
	substr($re, 0, 0, <<EOM);
From: me\@example.com
Subject: Re: awesome advice

WEB DESIGN EXPERT wrote:
EOM
	lei_ok([qw(rediff --full-index -U16 --drq)], undef,
		{ 0 => \$re, %$lei_opt });
	my $exp = <<'EOM';
From: me@example.com
Subject: Re: awesome advice

EOM
	like($lei_out, qr/\Q$exp\E/, '--drq preserved header');

	# n.b. --drq can requote the attribution line ("So-and-so wrote:"),
	# but it's probably not worth preventing...

	$exp = <<'EOM';
> ---
>  TODO | 2 ++
>  Ω    | 5 --
>  1 file changed, 2 insertions(+)
>
> diff --git a/TODO b/TODO
> index 605013e4904baabecd4a0a55997aebd8e8477a8f..69df7d565d49fbaaeb0a067910f03dc22cd52bd0 100644
> --- a/TODO
> +++ b/TODO
> @@ -96,16 +96,18 @@ all need to be considered for everything we introduce)
EOM
	$exp =~ s/^>$/> /sgm; # re-add trailing white space
	like($lei_out, qr/\Q$exp\E/, '--drq diffstat + context');

	lei_ok(qw(rediff -q --full-index -U9 t/solve/bare.patch));
	$exp = <<'EOM';
diff --git a/script/public-inbox-extindex b/script/public-inbox-extindex
old mode 100644
new mode 100755
index 15ac20eb871bf47697377e58a27db23102a38fca..771486c425b315bae70fd8a82d62ab0331e0a827
--- a/script/public-inbox-extindex
+++ b/script/public-inbox-extindex
@@ -1,13 +1,12 @@
 #!perl -w
EOM
	like($lei_out, qr/\Q$exp\E/,
		'preserve mode, regen header + context from -U0 patch');
	is($lei_err, '', 'no warnings from bare patch');
	my $e = { GIT_DIR => "$ENV{HOME}/.local/share/lei/store/ALL.git" };
	my @x = xqx([qw(git cat-file --batch-all-objects --batch-check)], $e);
	is_deeply(\@x, [], 'no objects stored') or diag explain(\@x);
});

test_lei({tmpdir => "$tmpdir/index-eml-only"}, sub {
	lei_ok(qw(index), $md);
	lei_ok(qw(blob 69df7d5)); # hits LeiSearch->smsg_eml -> lms->local_blob
});

my $git = PublicInbox::Git->new($git_dir);
$ibx->{-repo_objs} = [ $git ];
my $res;
my $solver = PublicInbox::SolverGit->new($ibx, sub { $res = $_[0] });
open my $log, '+>>', "$tmpdir/solve.log";
my $psgi_env = { 'psgi.errors' => \*STDERR, 'psgi.url_scheme' => 'http',
		'HTTP_HOST' => 'example.com' };
$solver->solve($psgi_env, $log, '69df7d5', {});
ok($res, 'solved a blob!');
my $wt_git = $res->[0];
is(ref($wt_git), 'PublicInbox::Git', 'got a git object for the blob');
is($res->[1], $expect, 'resolved blob to unabbreviated identifier');
is($res->[2], 'blob', 'type specified');
is($res->[3], 4405, 'size returned');

is(ref($wt_git->cat_file($res->[1])), 'SCALAR', 'wt cat-file works');
is_deeply([$expect, 'blob', 4405],
	  [$wt_git->check($res->[1])], 'wt check works');

my $oid = $expect;
for my $i (1..2) {
	my $more;
	my $s = PublicInbox::SolverGit->new($ibx, sub { $more = $_[0] });
	$s->solve($psgi_env, $log, $oid, {});
	is($more->[1], $expect, 'resolved blob to long OID '.$i);
	chop($oid);
}

$solver = undef;
$res = undef;
my $wt_git_dir = $wt_git->{git_dir};
$wt_git = undef;
ok(!-d $wt_git_dir, 'no references to WT held');

$solver = PublicInbox::SolverGit->new($ibx, sub { $res = $_[0] });
$solver->solve($psgi_env, $log, '0'x40, {});
is($res, undef, 'no error on z40');

my $git_v2_20_1_tag = '7a95a1cd084cb665c5c2586a415e42df0213af74';
$solver = PublicInbox::SolverGit->new($ibx, sub { $res = $_[0] });
$solver->solve($psgi_env, $log, $git_v2_20_1_tag, {});
is($res, undef, 'no error on a tag not in our repo');

$solver = PublicInbox::SolverGit->new($ibx, sub { $res = $_[0] });
$solver->solve($psgi_env, $log, '0a92431', {});
ok($res, 'resolved without hints');

my $hints = {
	oid_a => '3435775',
	path_a => 'HACKING',
	path_b => 'CONTRIBUTING'
};
$solver = PublicInbox::SolverGit->new($ibx, sub { $res = $_[0] });
$solver->solve($psgi_env, $log, '0a92431', $hints);
my $hinted = $res;
# don't compare ::Git objects:
shift @$res; shift @$hinted;
is_deeply($res, $hinted, 'hints work (or did not hurt :P');

SKIP: {
	require_mods qw(psgi 1);
	require PublicInbox::WWW;
	my $binfoo = "$ibx->{inboxdir}/binfoo.git";
	my $l = "$ibx->{inboxdir}/inbox.lock";
	-f $l or BAIL_OUT "BUG: $l missing: $!";
	require_ok 'PublicInbox::ViewVCS';
	my $big_size = do {
		no warnings 'once';
		$PublicInbox::ViewVCS::MAX_SIZE + 1;
	};
	my %bin = (big => $big_size, small => 1);
	my %oid; # (small|big) => OID
	require PublicInbox::Lock;
	my $lk = PublicInbox::Lock->new($l);
	my $acq = $lk->lock_for_scope;
	my $stamp = "$binfoo/stamp-";
	if (CORE::open my $fh, '<', $stamp) {
		%oid = map { chomp; split(/=/, $_) } (<$fh>);
	} else {
		PublicInbox::Import::init_bare($binfoo);
		my $cmd = [ qw(git hash-object -w --stdin) ];
		my $env = { GIT_DIR => $binfoo };
		while (my ($label, $size) = each %bin) {
			my $rdr = { 0 => \("\0" x $size) };
			chomp(my $x = run_qx($cmd , $env, $rdr));
			xbail "@$cmd: \$?=$?" if $?;
			$oid{$label} = $x;
		}

		open my $null, '<', '/dev/null';
		my $t = xqx([qw(git mktree)], $env, { 0 => $null });
		xbail "mktree: $?" if $?;
		chomp($t);
		my $non_utf8 = "K\x{e5}g";
		$env->{GIT_AUTHOR_NAME} = $non_utf8;
		$env->{GIT_AUTHOR_EMAIL} = 'e@example.com';
		$env->{GIT_COMMITTER_NAME} = $env->{GIT_AUTHOR_NAME};
		$env->{GIT_COMMITTER_EMAIL} = $env->{GIT_AUTHOR_EMAIL};
		my $in = \"$non_utf8\n\nK\x{e5}g\n";
		my @ct = qw(git -c i18n.commitEncoding=iso-8859-1 commit-tree);
		my $c = xqx([@ct, $t], $env, { 0 => $in });
		xbail "commit-tree: $?" if $?;
		chomp($c);
		$oid{'iso-8859-1'} = $c;

		$c = xqx([@ct, '-p', $c, $t], $env, { 0 => $in });
		xbail "commit-tree: $?" if $?;
		chomp($c);
		$oid{'8859-parent'} = $c;

		open my $fh, '>', "$stamp.$$";
		while (my ($k, $v) = each %oid) {
			print $fh "$k=$v\n";
		}
		close $fh;
		rename "$stamp.$$", $stamp;
	}
	undef $acq;
	# ensure the PSGI frontend (ViewVCS) works:
	my $name = $ibx->{name};
	my $cfgpfx = "publicinbox.$name";
	my $cfgpath = "$tmpdir/httpd-config";
	write_file '>', $cfgpath, <<EOF;
[coderepo]
	snapshots = tar.gz
[publicinbox "$name"]
	address = $ibx->{-primary_address}
	inboxdir = $ibx->{inboxdir}
	coderepo = public-inbox
	coderepo = binfoo
	url = http://example.com/$name
[coderepo "public-inbox"]
	dir = $git_dir
	cgiturl = http://example.com/public-inbox
[coderepo "binfoo"]
	dir = $binfoo
	cgiturl = http://example.com/binfoo
[coderepo "goner"]
	dir = $gone_repo
EOF
	my $exp_digest;
	{
		my $exp = xqx([qw(git archive --format=tar.gz
				--prefix=public-inbox-1.0.0/ v1.0.0)],
				{ GIT_DIR => $git_dir });
		is($?, 0, 'no error from git archive');
		ok(length($exp) > 1024, 'expected archive generated');
		$exp_digest = git_sha(256, \$exp)->hexdigest;
	};

	my $cfg = PublicInbox::Config->new($cfgpath);
	my $www = PublicInbox::WWW->new($cfg);
	my $client = sub {
		my ($cb) = @_;
		my $mid = '20190401081523.16213-1-BOFH@YHBT.net';
		my @warn;
		my $res = do {
			local $SIG{__WARN__} = sub { push @warn, @_ };
			$cb->(GET("/$name/$mid/"));
		};
		is_deeply(\@warn, [], 'no warnings from rendering diff');
		like($res->content, qr!>&#937;</a>!, 'omega escaped');

		$res = $cb->(GET("/$name/3435775/s/"));
		is($res->code, 200, 'success with existing blob');

		$res = $cb->(GET("/$name/".('0'x40).'/s/'));
		is($res->code, 404, 'failure with null OID');

		$res = $cb->(GET("/$name/$non_existent/s/"));
		is($res->code, 404, 'failure with non-existent OID');

		$res = $cb->(GET("/$name/$v1_0_0_tag/s/"));
		is($res->code, 200, 'shows commit (unabbreviated)');
		$res = $cb->(GET("/$name/$v1_0_0_tag_short/s/"));
		is($res->code, 200, 'shows commit (abbreviated)');
		while (my ($label, $size) = each %bin) {
			$res = $cb->(GET("/$name/$oid{$label}/s/"));
			is($res->code, 200, "$label binary file");
			ok(index($res->content,
				"blob $oid{$label} $size bytes") >= 0,
				"showed $label binary blob size");
			$res = $cb->(GET("/$name/$oid{$label}/s/raw"));
			is($res->code, 200, "$label raw binary download");
			is($res->content, "\0" x $size,
				"$label content matches");
		}
		my $utf8 = 'e022d3377fd2c50fd9931bf96394728958a90bf3';
		$res = $cb->(GET("/$name/$utf8/s/"));
		is($res->code, 200, 'shows commit w/ utf8.eml');
		like($res->content, qr/El&#233;anor/,
				'UTF-8 commit shown properly');

		# WwwCoderepo
		my $olderr;
		if (defined $ENV{PLACK_TEST_EXTERNALSERVER_URI}) {
			$stderr_empty->('nothing in stderr.log, yet');
		} else {
			open $olderr, '>&', \*STDERR;
			open STDERR, '+>>', "$tmpdir/stderr.log";
		}
		$res = $cb->(GET('/binfoo/'));
		defined($ENV{PLACK_TEST_EXTERNALSERVER_URI}) or
			open STDERR, '>&', $olderr;
		is($res->code, 200, 'coderepo summary (binfoo)');
		$stderr_empty->();

		$res = $cb->(GET("/binfoo/$oid{'iso-8859-1'}/s/"));
		is($res->code, 200, 'ISO-8859-1 commit');
		like($res->content, qr/K&#229;g/, 'ISO-8859-1 commit message');
		$stderr_empty->();

		$res = $cb->(GET("/binfoo/$oid{'8859-parent'}/s/"));
		is($res->code, 200, 'commit w/ ISO-8859-parent');
		like($res->content, qr/K&#229;g/, 'ISO-8859-1 commit message');
		$stderr_empty->();

		$res = $cb->(GET('/public-inbox/'));
		is($res->code, 200, 'coderepo summary (public-inbox)');

		my $tip = 'invalid-'.int(rand(0xdeadbeef));
		$res = $cb->(GET('/public-inbox/?h='.$tip));
		is($res->code, 200, 'coderepo summary on dead branch');
		like($res->content, qr/no commits in `\Q$tip\E', yet/,
			'lack of commits noted');

		$res = $cb->(GET('/public-inbox'));
		is($res->code, 301, 'redirected');

		my $fn = 'public-inbox-1.0.0.tar.gz';
		$res = $cb->(GET("/public-inbox/snapshot/$fn"));
		is($res->code, 200, 'tar.gz snapshot');
		is($res->header('Content-Disposition'),
			qq'inline; filename="$fn"', 'c-d header');
		is($res->header('ETag'), qq'"$v1_0_0_rev"', 'etag header');

		my $got = $res->content;
		is(git_sha(256, \$got)->hexdigest, $exp_digest,
			"content matches installed `git archive' output");
		undef $got;

		$fn = 'public-inbox-1.0.2.tar.gz';
		$res = $cb->(GET("/public-inbox/snapshot/$fn"));
		is($res->code, 404, '404 on non-existent tag');

		$fn = 'public-inbox-1.0.0.tar.bz2';
		$res = $cb->(GET("/public-inbox/snapshot/$fn"));
		is($res->code, 404, '404 on unconfigured snapshot format');

		$res = $cb->(GET('/public-inbox/atom/'));
		is($res->code, 200, 'Atom feed');
		SKIP: {
			require_mods('XML::TreePP', 1);
			my $t = eval { XML::TreePP->new->parse($res->content) }
				or diag explain($res);
			is(scalar @{$t->{feed}->{entry}}, 50,
				'got 50 entries') or diag explain([$t, $res]);

			$res = $cb->(GET('/public-inbox/atom/COPYING'));
			is($res->code, 200, 'file Atom feed');
			$t = XML::TreePP->new->parse($res->content);
			ok($t->{feed}->{entry}, 'got entry') or
				diag explain([ $t, $res ]);

			$res = $cb->(GET('/public-inbox/atom/README.md'));
			is($res->code, 404, '404 on missing file Atom feed');

			$res = $cb->(GET('/public-inbox/atom/?h=gone'));
			is($res->code, 404, '404 on missing Atom feed branch');
		}

		$res = $cb->(GET('/public-inbox/tree/'));
		is($res->code, 200, 'got 200 for root listing');
		$got = $res->content;
		like($got, qr/\bgit ls-tree\b/, 'ls-tree help shown');

		$res = $cb->(GET('/public-inbox/tree/README'));
		is($res->code, 200, 'got 200 for regular file');
		$got = $res->content;
		like($got, qr/\bgit show\b/, 'git show help shown');

		$res = $cb->(GET('/public-inbox/tree/Documentation'));
		is($res->code, 200, 'got 200 for a directory');
		$got = $res->content;
		like($got, qr/\bgit ls-tree\b/, 'ls-tree help shown');

		$res = $cb->(GET('/public-inbox/tree/?h=no-branch'));
		is($res->code, 404, 'got 404 for non-existent ref root');
		$res = $cb->(GET('/public-inbox/tree/README?h=no-file'));
		is($res->code, 404, 'got 404 for non-existent ref README');
		$res = $cb->(GET('/public-inbox/tree/Documentation?h=no-dir'));
		is($res->code, 404, 'got 404 for non-existent ref directory');

		$res = $cb->(GET('/public-inbox/tags.atom'));
		is($res->code, 200, 'Atom feed');
		SKIP: {
			require_mods('XML::TreePP', 1);
			my $t = XML::TreePP->new->parse($res->content);
			ok(scalar @{$t->{feed}->{entry}}, 'got tag entries');
		}

		# test disappearing repos
		$res = $cb->(GET('/goner/'));
		is $res->code, 200, 'coderepo summary for goner repo';
		$res = $cb->(GET("/goner/$oid{'iso-8859-1'}/s/"));
		is $res->code, 200, 'goner repo OID /s/ still available';
		$stderr_empty->();
		remove_tree $gone_repo;

		$res = $cb->(GET('/goner/'));
		is $res->code, 404, '/goner/ repo summary';
		$stderr_empty->();

		$res = $cb->(GET("/goner/$oid{'iso-8859-1'}/s/"));
		is $res->code, 404, 'gone repo OID /s/';
		$stderr_empty->();
	};

	state $cp = which('cp');
	$cp or xbail "`cp' not available (WTF!?)";
	xsys_e $cp, qw(-Rp), $binfoo, $gone_repo;

	test_psgi(sub { $www->call(@_) }, $client);
	my $env = { PI_CONFIG => $cfgpath, TMPDIR => $tmpdir };

	xsys_e $cp, qw(-Rp), $binfoo, $gone_repo; # for test_httpd $client
	my $has_log;
	SKIP: { # some distros may split out this middleware
		require_mods 'Plack::Middleware::AccessLog::Timed', 1;
		$has_log = $env->{psgi_file} = 't/psgi_log.psgi';
	}
	test_httpd($env, $client, 7, sub {
	SKIP: {
		my $td_pid = $PublicInbox::TestCommon::CURRENT_DAEMON->{pid};
		my $lis = $PublicInbox::TestCommon::CURRENT_LISTENER;
		kill 'STOP', $td_pid;
		my @req = ("GET /$name/69df7d5/s/ HTTP/1.0\r\n", "\r\n");
		my $c0 = tcp_connect($lis);
		print $c0 $req[0];
		my @c = map {
			my $c = tcp_connect($lis);
			print $c @req;
			$c;
		} (1..30);
		close $_ for @c[(1..29)]; # premature disconnects
		kill 'CONT', $td_pid;
		read $c[0], my $buf, 16384;
		print $c0 $req[1];
		read $c0, $buf, 16384;

		if ($has_log) {
			# kick server to ensure all CBs are handled, first
			$c0 = tcp_connect($lis);
			print $c0 <<EOM;
GUH /$name/69df7d5/s/ HTTP/1.1\r
Connection: close\r
\r
EOM
			read $c0, $buf, 16384;
			my %counts;
			my @n = map {
					my $code = (split ' ', $_)[5];
					$counts{$code}++;
					$code;
				} grep m! HTTP/1\.0\b!,
					try_cat("$tmpdir/stdout.log");
			ok $counts{499}, 'got some 499s from disconnects';
			ok $counts{200} >= 2, 'got at least two 200 codes' or
				diag explain(\%counts);
			is $counts{499} + $counts{200}, 31,
				'all 1.0 connections logged for disconnects';
		}
		undef $c0;
		kill 'STOP', $td_pid;
		@c = map {
			my $c = tcp_connect($lis);
			print $c $req[0];
			$c;
		}  (0..66);
		print $_ $req[1] for @c;
		kill 'CONT', $td_pid;
		my %codes;
		for (@c) {
			read $_, $buf, 16384;
			my $code = $buf =~ m!\AHTTP/1\.0 (\d+) ! ? $1 : '';
			++$codes{$code};
		}
		is $codes{''}, undef, 'all valid '.scalar(@c).' HTTP responses';
		ok $codes{503}, 'got some 503 too busy errors';
		is $codes{503} + $codes{200},
			scalar(@c), 'only got 200 and 503 codes';

		require_cmd('curl', 1) or skip 'no curl', 1;
		mkdir "$tmpdir/ext";
		my $rurl = "$ENV{PLACK_TEST_EXTERNALSERVER_URI}/$name";
		test_lei({tmpdir => "$tmpdir/ext"}, sub {
			lei_ok(qw(blob --no-mail 69df7d5 -I), $rurl);
			is(git_sha(1, \$lei_out)->hexdigest, $expect,
				'blob contents output');
			ok(!lei(qw(blob -I), $rurl, $non_existent),
					'non-existent blob fails');
		});
	}});
}

done_testing();
