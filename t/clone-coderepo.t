#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use PublicInbox::Import;
use File::Temp;
use Digest::SHA qw(sha1_hex);
require_mods(qw(json Plack::Builder HTTP::Date HTTP::Status));
require_git '1.8.5';
require_ok 'PublicInbox::LeiMirror';
my ($tmpdir, $for_destroy) = tmpdir();
my $pa = "$tmpdir/src/a.git";
my $pb = "$tmpdir/src/b.git";
PublicInbox::Import::init_bare($pa);
my ($stdout, $stderr) = ("$tmpdir/out.log", "$tmpdir/err.log");
my $pi_config = "$tmpdir/pi_config";
my $td;
my $tcp = tcp_server();
my $url = 'http://'.tcp_host_port($tcp).'/';
my $set_manifest = sub {
	my ($m, $f) = @_;
	$f //= "$tmpdir/src/manifest.js.gz";
	my $ft = File::Temp->new(TMPDIR => $tmpdir, UNLINK => 0);
	PublicInbox::LeiMirror::dump_manifest($m, $ft);
	PublicInbox::LeiMirror::ft_rename($ft, $f, 0666);
};
my $read_manifest = sub {
	my ($f) = @_;
	open my $fh, '<', $f or xbail "open($f): $!";
	PublicInbox::LeiMirror::decode_manifest($fh, $f, $f);
};

my $t0 = time - 1;
my $m; # manifest hashref

{
	my $rdr = {};
	my $fi_data = './t/git.fast-import-data';
	open $rdr->{0}, '<', $fi_data or xbail "open($fi_data): $!";
	my @git = ('git', "--git-dir=$pa");
	xsys_e([@git, qw(fast-import --quiet)], undef, $rdr);
	xsys_e([qw(/bin/cp -Rp a.git b.git)], undef, { -C => "$tmpdir/src" });
	open my $fh, '>', $pi_config or xbail "open($pi_config): $!";
	print $fh <<EOM or xbail "print: $!";
[publicinbox]
	cgitrc = $tmpdir/cgitrc
	cgit = fallback
EOM
	close $fh or xbail "close: $!";

	my $f = "$tmpdir/cgitrc";
	open $fh, '>', $f or xbail "open($f): $!";
	print $fh <<EOM or xbail "print: $!";
project-list=$tmpdir/src/projects.list
scan-path=$tmpdir/src
EOM
	close $fh or xbail "close($f): $!";

	my $cmd = [ '-httpd', '-W0', "--stdout=$stdout", "--stderr=$stderr",
		File::Spec->rel2abs('t/clone-coderepo.psgi') ];
	my $env = { TEST_DOCROOT => "$tmpdir/src", PI_CONFIG => $pi_config };
	$td = start_script($cmd, $env, { 3 => $tcp });
	my $fp = sha1_hex(my $refs = xqx([@git, 'show-ref']));
	$m = {
		'/a.git' => {
			fingerprint => $fp,
			modified => 1,
			owner => 'Alice',
		},
		'/b.git' => {
			fingerprint => $fp,
			modified => 1,
			owner => 'Bob',
		},
	};
	$set_manifest->($m);
	$f = "$tmpdir/src/projects.list";
	open $fh, '>', $f, or xbail "open($f): $!";
	print $fh <<EOM or xbail "print($f): $!";
a.git
b.git
EOM
	close $fh or xbail "close($f): $!";
}

my $cmd = [qw(-clone --inbox-config=never --manifest= --project-list=
	--objstore= -p -q), $url, "$tmpdir/dst"];
ok(run_script($cmd), 'clone');
is(xqx([qw(git config gitweb.owner)], { GIT_DIR => "$tmpdir/dst/a.git" }),
	"Alice\n", 'a.git gitweb.owner set');
is(xqx([qw(git config gitweb.owner)], { GIT_DIR => "$tmpdir/dst/b.git" }),
	"Bob\n", 'b.git gitweb.owner set');

my $dst_pl = "$tmpdir/dst/projects.list";
my $dst_mf = "$tmpdir/dst/manifest.js.gz";
ok(!-d "$tmpdir/dst/objstore", 'no objstore created w/o forkgroups');
my $r = $read_manifest->($dst_mf);
is_deeply($r, $m, 'manifest matches');

is(PublicInbox::Git::try_cat($dst_pl), "a.git\nb.git\n",
	'wrote projects.list');

{ # check symlinks
	$m->{'/a.git'}->{symlinks} = [ '/old/a.git' ];
	$set_manifest->($m);
	utime($t0, $t0, $dst_mf) or xbail "utime: $!";
	ok(run_script($cmd), 'clone again +symlinks');
	ok(-l "$tmpdir/dst/old/a.git", 'symlink created');
	is(PublicInbox::Git::try_cat($dst_pl), "a.git\nb.git\n",
		'projects.list does not include symlink by default');

	$r = $read_manifest->($dst_mf);
	is_deeply($r, $m, 'updated manifest matches');
}
{ # cleanup old projects from projects.list
	open my $fh, '>>', $dst_pl or xbail $!;
	print $fh "gone.git\n" or xbail $!;
	close $fh or xbail $!;

	utime($t0, $t0, $dst_mf) or xbail "utime: $!";
	my $rdr = { 2 => \(my $err = '') };
	ok(run_script($cmd, undef, $rdr), 'clone again for expired gone.git');
	is(PublicInbox::Git::try_cat($dst_pl), "a.git\nb.git\n",
		'project list cleaned');
	like($err, qr/no longer exist.*\bgone\.git\b/s, 'gone.git noted');
}


done_testing;
