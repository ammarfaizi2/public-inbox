#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use PublicInbox::TestCommon;
require_mods 'PublicInbox::Lg2';
use PublicInbox::Git qw(git_exe);
use PublicInbox::Spawn qw(run_die run_wait);
use_ok 'PublicInbox::Lg2';
use PublicInbox::Config;
my $tmpdir = tmpdir;
my $f = "$tmpdir/cfg";
my $cfgwr_commit = $ENV{TEST_VALIDATE_GIT_BEHAVIOR} ? sub {
	my ($file, $todo) = @_;
	my @x = (git_exe, 'config', '-f', $file);
	for my $c (@$todo) {
		unshift @$c, @x;
		if ($c->[scalar(@x)] eq '--unset-all') {
			run_wait $c;
			# ignore ret=5 if no matches (see git-config(1))
			die "E: @$c \$?=$?" if ($? && ($? >> 8) != 5);
		} else {
			run_die $c;
		}
	}
} : PublicInbox::Lg2->can('cfgwr_commit');

SKIP: {
$cfgwr_commit or skip 'libgit2 '.PublicInbox::Lg2->modversion.
		' < 1.8 too old for multiline configs', 1;
my $cfg; # for explain() if needed
my $get = sub {
	my (@key) = @_;
	$cfg = PublicInbox::Config->new($f);
	@key > 1 ? (map { $_ => $cfg->{$_} } @key) : $cfg->{$key[0]};
};

$cfgwr_commit->($f, [ [ qw(a.b a) ] ]);
is $get->('a.b'), 'a', 'config set works';

$cfgwr_commit->($f, [ [ qw(--add a.b a) ] ]);
is_xdeeply $get->('a.b'), [ qw(a a) ], 'config --add works to append';
$cfgwr_commit->($f, [ [ qw(--add x.b c) ] ]);
my %cfg = $get->('x.b', 'a.b');
is $cfg{'x.b'}, 'c', 'config --add works to create';
is_xdeeply $cfg{'a.b'}, [ qw(a a) ], 'config --add left existing alone';

$cfgwr_commit->($f, [ [ qw(--unset-all a.b) ] ]);
is $get->('a.b'), undef, 'config --unset-all works';

$cfgwr_commit->($f, [ [ qw(--unset-all bogus.entry) ] ]);
is $get->('bogus.entry'), undef, 'config --unset-all non-match';

$cfgwr_commit->($f, [ [ qw(x.b d) ] ]);
is $get->('x.b'), 'd', 'set clobbers existing value';

eval { $cfgwr_commit->($f, []) };
ok !$@, 'no exception or errors on empty todo';

$cfgwr_commit->($f, [ [ qw(x.b d) ], [ qw(--add x.b e) ],
	[ qw(--add x.b f) ] ]);
is_xdeeply $get->('x.b'), [ qw(d e f) ], 'multiple changes chained';

use PublicInbox::IO qw(write_file try_cat);
my $src = <<EOM;
[foo]
	bar = some \\
very \\
long line
EOM
write_file '>', $f, $src;
$cfgwr_commit->($f, [ [ qw(x.b d) ], [ qw(--add x.b e) ] ]);
like try_cat($f), qr/^\Q$src\E/sm, 'newlines preserved';
} # SKIP

done_testing;
