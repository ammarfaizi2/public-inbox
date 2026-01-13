#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12; use PublicInbox::TestCommon;
use Config;
use POSIX qw(uname);
use PublicInbox::Syscall;
plan skip_all => 'inotify is Linux and FreeBSD 15+ only'
	unless $PublicInbox::Syscall::INOTIFY;
unless (eval { require PublicInbox::Inotify3 }) {
	my (undef, undef, undef, undef, $machine) = uname();
	diag '<cppsymbols>';
	diag "\t$_" for (split /(?<!\\)\s+/, $Config{cppsymbols});
	diag '</cppsymbols>';
	diag "$_ => $Config{$_}" for qw(ptrsize sizesize);
	plan skip_all => "inotify constants not defined on $machine";
}

use_ok 'PublicInbox::Inotify3';
my $in = PublicInbox::Inotify3->new;
my $tmpdir = tmpdir;
my $w = $in->watch("$tmpdir", PublicInbox::Inotify3::IN_ALL_EVENTS());
$in->blocking(0);
is_xdeeply [ $in->read ], [], 'non-blocking has no events, yet';
undef $tmpdir;
my @list = $in->read;
ok scalar(@list), 'got events';
ok $w->cancel, 'watch canceled';

done_testing;
