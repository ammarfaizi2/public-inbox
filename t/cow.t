# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12; use autodie; use PublicInbox::TestCommon;
use File::Temp 0.19;
use PublicInbox::IO;
my $dir = $ENV{BTRFS_TESTDIR};
plan skip_all => 'BTRFS_TESTDIR not defined' if !$dir;
plan skip_all => 'test is Linux-only' if $^O ne 'linux';
require_mods 'v2';
my $lsattr = require_cmd 'lsattr';
my $tmp = File::Temp->newdir('cow-XXXX', DIR => $dir);
local $ENV{PI_CONFIG} = "$tmp/pi-cfg";

PublicInbox::IO::write_file '>', $ENV{PI_CONFIG}, <<EOM;
[publicinboxmda]
	spamcheck = none
EOM

my $addr = 'a@example.com';
my $baddr = 'b@example.com';
my $a2ddr = 'a2@example.com';
my $b2addr = 'b2@example.com';

run_script([qw(-init -L medium a),
	"$tmp/a", qw(https://example.com/a), $addr]);

my $lsa = xqx([$lsattr, '-l', "$tmp/a/public-inbox"]);
like $lsa, qr/\bNo_COW\b/, 'No_COW set by default (v1)' or
	diag explain($lsa);

run_script([qw(-init --cow -L medium b),
	"$tmp/b", qw(https://example.com/b), $baddr]);

$lsa = xqx([$lsattr, '-l', "$tmp/b/public-inbox"]);
unlike $lsa, qr/\bNo_COW\b/, 'No_COW not set' or
	diag explain($lsa);

run_script([qw(-init -V2 -L medium a2),
	"$tmp/a2", qw(https://example.com/a2 a2@example.com)]);
$lsa = xqx([$lsattr, '-l', "$tmp/a2/", glob("$tmp/a2/xap*/")]);
like $lsa, qr/\bNo_COW\b/, 'No_COW set by default (v2)' or
	diag explain($lsa);

ok run_script([qw(-init --cow -V2 -L medium b2),
	"$tmp/b2", qw(https://example.com/b2 b2@example.com)]);
$lsa = xqx([$lsattr, '-l', "$tmp/b2/", glob("$tmp/b2/xap*/")]);
unlike $lsa, qr/\bNo_COW\b/, 'No_COW not set with --cow';

ok run_script([qw(-init -L basic --cow c),
	"$tmp/c", qw(https://example.com/c c@example.com)]),
	'-init -V1 w/o CoW';

my $eml = eml_load 't/plack-qp.eml';
run_script([qw(-mda --no-precheck)],
	{ ORIGINAL_RECIPIENT => 'c@example.com' },
	{ 0 => \($eml->as_string) });

$lsa = xqx([$lsattr, '-l', "$tmp/b/public-inbox"]);
unlike $lsa, qr/\bNo_COW\b/, 'No_COW not set' or
	diag explain($lsa);

ok run_script([qw(-convert --cow), "$tmp/c", "$tmp/c2"]),
       '-convert --cow';
$lsa = xqx([$lsattr, '-lR', glob("$tmp/c2/xap*/")]);
unlike $lsa, qr/\bNo_COW\b/i, 'CoW preserved w/ -convert --cow';

ok run_script([qw(-convert), "$tmp/c", "$tmp/C2"]),
       '-convert w/o --cow';
$lsa = xqx([$lsattr, '-lR', glob("$tmp/C2/xap*/")]);
like $lsa, qr/\bNo_COW\b/i, '-convert unsets CoW w/o --cow';

ok run_script([qw(-index --cow -L medium), "$tmp/c2"]),
       '-index -V2 --cow + Xapian';
$lsa = xqx([$lsattr, '-lR', "$tmp/c2/", glob("$tmp/c2/xap*/")]);
unlike $lsa, qr/\bNo_COW\b/i, 'CoW preserved w/ -convert --cow + Xapian';

ok run_script([qw(-xcpdb -R1 --cow), "$tmp/c2"]),
	'xcpdb respects --cow';
$lsa = xqx([$lsattr, '-lR', glob("$tmp/c2/xap*/")]);
unlike $lsa, qr/\bNo_COW\b/i, '-xcpdb --cow works';

done_testing;
