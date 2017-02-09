# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)
use strict;
use warnings;
use Test::More;
use PublicInbox::RepoGit qw(git_unquote);

is("foo\nbar", git_unquote('"foo\\nbar"'), 'unquoted newline');
is("Eléanor", git_unquote('"El\\303\\251anor"'), 'unquoted octal');

done_testing();
