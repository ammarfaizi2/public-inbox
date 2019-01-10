# Copyright (C) 2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use File::Temp qw/tempdir/;
require './t/common.perl';
require_git(2.6);
my @mods = qw(IPC::Run DBI DBD::SQLite Search::Xapian);
foreach my $mod (@mods) {
	eval "require $mod";
	plan skip_all => "missing $_ for t/purge.t" if $@;
};
use Cwd qw(abs_path);
my $purge = abs_path('blib/script/public-inbox-purge');
my $tmpdir = tempdir('pi-purge-XXXXXX', TMPDIR => 1, CLEANUP => 1);
use_ok 'PublicInbox::V2Writable';
my $mainrepo = "$tmpdir/v2";
my $ibx = PublicInbox::Inbox->new({
	mainrepo => $mainrepo,
	name => 'test-v2purge',
	version => 2,
	-primary_address => 'test@example.com',
	indexlevel => 'basic',
});

my $raw = <<'EOF';
From: a@example.com
To: test@example.com
Subject: this is a subject
Message-ID: <a-mid@b>
Date: Fri, 02 Oct 1993 00:00:00 +0000

Hello World

EOF

local $ENV{NPROC} = '1';
my $cfgfile = "$tmpdir/config";
local $ENV{PI_CONFIG} = $cfgfile;
open my $cfg_fh, '>', $cfgfile or die "open: $!";

my $v2w = PublicInbox::V2Writable->new($ibx, 1);
my $mime = PublicInbox::MIME->new($raw);
ok($v2w->add($mime), 'add message to be purged');
$v2w->done;

# failing cases, first:
my $in = "$raw\nMOAR\n";
my ($out, $err) = ('', '');
ok(IPC::Run::run([$purge, '-f', $mainrepo], \$in, \$out, \$err),
	'purge -f OK');

$out = $err = '';
ok(!IPC::Run::run([$purge, $mainrepo], \$in, \$out, \$err),
	'mismatch fails without -f');
is($? >> 8, 1, 'missed purge exits with 1');

# a successful case:
ok(IPC::Run::run([$purge, $mainrepo], \$raw, \$out, \$err), 'match OK');
like($out, qr/^\t[a-f0-9]{40,}/m, 'removed commit noted');

# add (old) vger filter to config file
print $cfg_fh <<EOF or die "print $!";
[publicinbox "test-v2purge"]
	mainrepo = $mainrepo
	address = test\@example.com
	indexlevel = basic
	filter = PublicInbox::Filter::Vger
EOF
close $cfg_fh or die "close: $!";

ok($v2w->add($mime), 'add vger-signatured message to be purged');
$v2w->done;

my $pre_scrub = $raw . <<'EOF';

--
To unsubscribe from this list: send the line "unsubscribe linux-kernel" in
the body of a message to majordomo@vger.kernel.org
More majordomo info at  http://vger.kernel.org/majordomo-info.html
Please read the FAQ at  http://www.tux.org/lkml/
EOF

$out = $err = '';
ok(chdir('/'), "chdir / OK for --all test");
ok(IPC::Run::run([$purge, '--all'], \$pre_scrub, \$out, \$err),
	'scrub purge OK');
like($out, qr/^\t[a-f0-9]{40,}/m, 'removed commit noted');
# diag "out: $out"; diag "err: $err";

$out = $err = '';
ok(!IPC::Run::run([$purge, '--all' ], \$pre_scrub, \$out, \$err),
	'scrub purge not idempotent without -f');
# diag "out: $out"; diag "err: $err";

done_testing();
