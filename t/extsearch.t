#!perl -w
# Copyright (C) 2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use Test::More;
use PublicInbox::TestCommon;
use Fcntl qw(:seek);
require_git(2.6);
require_mods(qw(DBD::SQLite Search::Xapian));
use_ok 'PublicInbox::ExtSearch';
use_ok 'PublicInbox::ExtSearchIdx';
my ($home, $for_destroy) = tmpdir();
local $ENV{HOME} = $home;
mkdir "$home/.public-inbox" or BAIL_OUT $!;
open my $fh, '>', "$home/.public-inbox/config" or BAIL_OUT $!;
print $fh <<EOF or BAIL_OUT $!;
[publicinboxMda]
	spamcheck = none
EOF
close $fh or BAIL_OUT $!;
my $v2addr = 'v2test@example.com';
my $v1addr = 'v1test@example.com';
ok(run_script([qw(-init -V2 v2test --newsgroup v2.example), "$home/v2test",
	'http://example.com/v2test', $v2addr ]), 'v2test init');
my $env = { ORIGINAL_RECIPIENT => $v2addr };
open($fh, '<', 't/utf8.eml') or BAIL_OUT("open t/utf8.eml: $!");
run_script(['-mda', '--no-precheck'], $env, { 0 => $fh }) or BAIL_OUT '-mda';

ok(run_script([qw(-init -V1 v1test), "$home/v1test",
	'http://example.com/v1test', $v1addr ]), 'v1test init');
$env = { ORIGINAL_RECIPIENT => $v1addr };
seek($fh, 0, SEEK_SET) or BAIL_OUT $!;
run_script(['-mda', '--no-precheck'], $env, { 0 => $fh }) or BAIL_OUT '-mda';
run_script(['-index', "$home/v1test"]) or BAIL_OUT "index $?";

ok(run_script([qw(-eindex --all), "$home/eindex"]), 'eindex init');

my $es = PublicInbox::ExtSearch->new("$home/eindex");
{
	my $smsg = $es->over->get_art(1);
	ok($smsg, 'got first article');
	is($es->over->get_art(2), undef, 'only one added');
	my $xref3 = $es->over->get_xref3(1);
	like($xref3->[0], qr/\A\Qv2.example\E:1:/, 'order preserved 1');
	like($xref3->[1], qr!\A\Q$home/v1test\E:1:!, 'order preserved 2');
	is(scalar(@$xref3), 2, 'only to entries');
}

{
	my ($in, $out, $err);
	$in = $out = $err = '';
	my $opt = { 0 => \$in, 1 => \$out, 2 => \$err };
	my $env = { MAIL_EDITOR => "$^X -i -p -e 's/test message/BEST MSG/'" };
	my $cmd = [ qw(-edit -Ft/utf8.eml), "$home/v2test" ];
	ok(run_script($cmd, $env, $opt), '-edit');
	ok(run_script([qw(-eindex --all), "$home/eindex"], undef, $opt),
		'eindex again');
	like($err, qr/discontiguous range/, 'warned about discontiguous range');
	my $msg1 = $es->over->get_art(1) or BAIL_OUT 'msg1 missing';
	my $msg2 = $es->over->get_art(2) or BAIL_OUT 'msg2 missing';
	is($msg1->{mid}, $msg2->{mid}, 'edited message indexed');
	isnt($msg1->{blob}, $msg2->{blob}, 'blobs differ');
	my $eml2 = $es->smsg_eml($msg2);
	like($eml2->body, qr/BEST MSG/, 'edited body in #2');
	unlike($eml2->body, qr/test message/, 'old body discarded in #2');
	my $eml1 = $es->smsg_eml($msg1);
	like($eml1->body, qr/test message/, 'original body in #1');
	my $x1 = $es->over->get_xref3(1);
	my $x2 = $es->over->get_xref3(2);
	is(scalar(@$x1), 1, 'original only has one xref3');
	is(scalar(@$x2), 1, 'new message has one xref3');
	isnt($x1->[0], $x2->[0], 'xref3 differs');
}

done_testing;
