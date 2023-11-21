#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# cindex --join functionality against mwrap, a small projects
# started as C+Ruby and got forked to C+Perl/XS w/ public inboxes for each
use v5.12;
use PublicInbox::TestCommon;
use PublicInbox::IO qw(write_file);
use PublicInbox::Import;
use PublicInbox::Config;
use autodie;
use File::Spec;
$ENV{TEST_REMOTE_JOIN} or plan skip_all => 'TEST_REMOTE_JOIN unset';
local $ENV{TAIL_ALL} = $ENV{TAIL_ALL} // 1; # while features are unstable
require_mods(qw(json Xapian DBD::SQLite));
my @code = qw(https://80x24.org/mwrap-perl.git
		https://80x24.org/mwrap.git);
my @inboxes = qw(https://80x24.org/mwrap-public 2 inbox.comp.lang.ruby.mwrap
	https://80x24.org/mwrap-perl 2 inbox.comp.lang.perl.mwrap);
my (%code, %inboxes);
my $topdir = File::Spec->rel2abs('.');
my $tmpdir = tmpdir;
while (my $url = shift @code) {
	my ($key) = ($url =~ m!/([^/]+\.git)\z!);
	$code{$key} = create_coderepo $key, sub {
		PublicInbox::Import::init_bare '.';
		write_file '>>', 'config', <<EOM;
[remote "origin"]
	url = $url
	fetch = +refs/*:refs/*
	mirror = true
EOM
		if (my $d = $code{'mwrap-perl.git'}) {
			$d = File::Spec->abs2rel("$topdir/$d", 'objects');
			write_file '>','objects/info/alternates',"$d/objects\n"
		}
		diag "mirroring coderepo: $url ...";
		xsys_e qw(git fetch -q origin);
	};
}

while (my ($url, $v, $ng) = splice(@inboxes, 0, 3)) {
	my ($key) = ($url =~ m!/([^/]+)\z!);
	my @opt = (version => $v, tmpdir => "$tmpdir/$key" -no_gc => 1);
	$inboxes{$key} = create_inbox $key, @opt, sub {
		my ($im, $ibx) = @_;
		$im->done;
		diag "cloning public-inbox $url ...";
		run_script([qw(-clone -q), $url, $ibx->{inboxdir}]) or
			xbail "clone: $?";
		diag "indexing $ibx->{inboxdir} ...";
		run_script([qw(-index -v -L medium --dangerous),
				$ibx->{inboxdir}]) or xbail "index: $?";
	};
	$inboxes{$key}->{newsgroup} = $ng;
};
my $env = {};
open my $fh, '>', $env->{PI_CONFIG} = "$tmpdir/pi_config";
for (sort keys %inboxes) {
	print $fh <<EOM;
[publicinbox "$_"]
	inboxdir = $inboxes{$_}->{inboxdir}
	address = $_\@80x24.org
	newsgroup = $inboxes{$_}->{newsgroup}
EOM
}
close $fh;
my $cidxdir = "$tmpdir/cidx";
# this should be fast since mwrap* are small
my $rdr = { 1 => \my $cout, 2 => \my $cerr };
ok run_script([qw(-cindex -v --all --show=join_data),
		'--join=aggressive,dt:..2022-12-01',
		'-d', $cidxdir, values %code ],
		$env, $rdr), 'initial join inboxes w/ coderepos';
my $out = PublicInbox::Config->json->decode($cout);
is($out->{join_data}->{dt}->[0], '19700101'.'000000',
	'dt:..$END_DATE starts from epoch');

ok run_script([qw(-cindex -v --all -u --join --show),
		'-d', $cidxdir], $env, $rdr), 'incremental --join';

done_testing;
