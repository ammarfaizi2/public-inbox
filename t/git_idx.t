# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPLv3 or later (https://www.gnu.org/licenses/agpl-3.0.txt)
use strict;
use warnings;
use Test::More;
use File::Temp qw/tempdir/;
use Email::MIME;
my $tmpdir = tempdir('pi-git-idx-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $git_dir = "$tmpdir/a.git";
use_ok 'PublicInbox::Git';
use_ok 'PublicInbox::GitIdx';
my $git = PublicInbox::Git->new($git_dir);
is(0, system(qw(git init -q --bare), $git_dir), "git init (main)");

$git->qx(qw(config core.sharedRepository 0644));
is(git_umask_for($git_dir), oct '022', 'umask is correct for 644');

$git->qx(qw(config core.sharedRepository 0664));
is(git_umask_for($git_dir), oct '002', 'umask is correct for 664');

$git->qx(qw(config core.sharedRepository group));
is(git_umask_for($git_dir), oct '007', 'umask is correct for "group"');

done_testing();
