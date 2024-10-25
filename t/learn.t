# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use autodie;
use PublicInbox::TestCommon;
plan skip_all => "cannot test $0 as root" if $> == 0;
require_mods qw(DBD::SQLite);
my ($v2, $v2ro, @v2cfg);
my $tmpdir = tmpdir;
my $eml = eml_load 't/plack-qp.eml';
my $v1 = create_inbox 'v1', indexlevel => 'basic', tmpdir => "$tmpdir/v1", sub {
	my ($im) = @_;
	$im->add($eml);
};

my $v1ro = create_inbox 'v1ro', indexlevel => 'basic',
		tmpdir => "$tmpdir/v1ro", sub {
	my ($im) = @_;
	$im->add($eml);
};
chmod 0500, $v1ro->{inboxdir};
chmod 0400, glob("$v1ro->{inboxdir}/public-inbox/xapian*/over.sqlite3"),
		glob("$v1ro->{inboxdir}/public-inbox/msgmap.sqlite3");

SKIP: {
	require_git v2.6, 1;
	$v2 = create_inbox 'v2', indexlevel => 'basic', version => 2,
			tmpdir => "$tmpdir/v2", sub {
		my ($im, $ibx) = @_;
		$im->add($eml);
	};
	$v2ro = create_inbox 'v2', indexlevel => 'basic', version => 2,
			tmpdir => "$tmpdir/v2ro", sub {
		my ($im, $ibx) = @_;
		$im->add($eml);
	};
	chmod 0500, $v2ro->{inboxdir}, "$v2ro->{inboxdir}/git/0.git";
	chmod 0400, glob("$v2ro->{inboxdir}/xap*/over.sqlite3"),
		glob("$v2ro->{inboxdir}/msgmap.sqlite3");
	@v2cfg = (<<EOM);
[publicinbox "v2ro"]
	inboxdir = $v2ro->{inboxdir};
	address = v2ro\@example.com
	indexlevel = basic
[publicinbox "v2"]
	inboxdir = $v2->{inboxdir};
	address = v2\@example.com
	indexlevel = basic
EOM
}

my $cfg = cfg_new $tmpdir, <<EOM, @v2cfg;
[publicinbox "v1ro"]
	inboxdir = $v1ro->{inboxdir}
	address = v1ro\@example.com
	indexlevel = basic
[publicinbox "v1"]
	inboxdir = $v1->{inboxdir}
	address = v1\@example.com
	indexlevel = basic
EOM

my $opt = {
	0 => \($eml->as_string),
	1 => \(my $out),
	2 => \(my $err),
};
my $env = { PI_CONFIG => $cfg->{-f} };

run_script [ qw(-learn rm --all -k) ], $env, $opt;
isnt $?, 0, 'learn $? is non-zero';
is 0, $v1->over->max, 'removed from r/w v1';
is 1, $v1ro->over->max, 'not removed from r/o v1';
my $nr = 1;
SKIP: {
	require_git v2.6, 1;
	is 0, $v2->over->max, 'removed from r/w v2';
	is 1, $v2ro->over->max, 'not removed from r/o v2';
	$nr = 2;
}

like $err, qr/E: $nr inbox\(es\) failed/, 'failures noted in stderr';
is $out, '', 'stdout is empty';

done_testing;
