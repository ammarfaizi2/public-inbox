#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Ensure KQNotify can pick up rename(2) and link(2) operations
# used by Maildir writing tools
use v5.12;
use PublicInbox::TestCommon;
use autodie;
require_bsd;
require_mods('IO::KQueue');
use_ok 'PublicInbox::KQNotify';
my ($tmpdir, $for_destroy) = tmpdir();
mkdir "$tmpdir/new";

my $kqn = PublicInbox::KQNotify->new;
my $mask = PublicInbox::KQNotify::MOVED_TO_OR_CREATE();
my $w = $kqn->watch("$tmpdir/new", $mask);

open my $fh, '>', "$tmpdir/tst";
close $fh;
rename("$tmpdir/tst", "$tmpdir/new/tst");
my $hit = [ map { $_->fullname } $kqn->read ];
is_deeply($hit, ["$tmpdir/new/tst"],
		'rename(2) detected (via NOTE_EXTEND)')
		or diag explain($hit);

open $fh, '>', "$tmpdir/tst";
close $fh;
link("$tmpdir/tst", "$tmpdir/new/link");
my @read = map { $_->fullname } $kqn->read;
$hit = [ grep(m!/link$!, @read) ];
is_deeply($hit, ["$tmpdir/new/link"], 'link(2) detected (via NOTE_WRITE)')
	or diag explain(\@read);

{
	my $d = "$tmpdir/new/ANOTHER";
	mkdir $d;
	$hit = [ map { $_->fullname } $kqn->read ];
	is_xdeeply($hit, [ $d ], 'mkdir detected');
	rmdir $d;
	# TODO: should we always watch for directory removals?
}

$w->cancel;
link("$tmpdir/new/tst", "$tmpdir/new/link2");
$hit = [ map { $_->fullname } $kqn->read ];
is_deeply($hit, [], 'link(2) not detected after cancel');

# rearm:
my $GONE = PublicInbox::KQNotify::NOTE_DELETE() |
	PublicInbox::KQNotify::NOTE_REVOKE() |
	PublicInbox::KQNotify::NOTE_ATTRIB() |
	PublicInbox::KQNotify::NOTE_WRITE() |
	PublicInbox::KQNotify::NOTE_RENAME();
$w = $kqn->watch("$tmpdir/new", $mask|$GONE);
my @unlink = sort glob("$tmpdir/new/*");
unlink(@unlink);
$hit = [ sort(map { $_->fullname } $kqn->read) ];
is_xdeeply($hit, \@unlink, 'unlinked files match');

# this is unreliable on Dragonfly tmpfs (fixed post-6.4)
rmdir "$tmpdir/new";
$hit = [ sort(map { $_->fullname } $kqn->read) ];
is(scalar(@$hit), 1, 'detected self removal') or check_broken_tmpfs;

done_testing;
