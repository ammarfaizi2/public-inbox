#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12; use PublicInbox::TestCommon;
plan skip_all => 'inotify is Linux-only' if $^O ne 'linux';
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
