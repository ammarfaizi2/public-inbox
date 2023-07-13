#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use POSIX qw(_exit);
my ($tmpdir, $for_destroy) = tmpdir();
use_ok 'PublicInbox::TailNotify';
my $f = "$tmpdir/log";
open my $fh, '>>', $f or xbail $!;
my $tn = PublicInbox::TailNotify->new($f);
my @x = $tn->getlines(1);
is_deeply(\@x, [], 'nothing, yet');
my $pid = fork // xbail "fork: $!";
if ($pid == 0) {
	tick;
	syswrite $fh, "hi\n" // xbail "syswrite: $!";
	_exit(0);
}
@x = $tn->getlines;
is_deeply(\@x, [ "hi\n" ], 'got line');
waitpid($pid, 0) // xbail "waitpid: $!";
is($?, 0, 'writer done');

$pid = fork // xbail "fork: $!";
if ($pid == 0) {
	tick;
	unlink $f // xbail "unlink($f): $!";
	open $fh, '>>', $f or xbail $!;
	syswrite $fh, "bye\n" // xbail "syswrite: $!";
	_exit(0);
}
@x = $tn->getlines;
is_deeply(\@x, [ "bye\n" ], 'got line after reopen');
waitpid($pid, 0) // xbail "waitpid: $!";
is($?, 0, 'writer done');

done_testing;
