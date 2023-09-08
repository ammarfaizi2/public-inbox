#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Ensure KQNotify can pick up rename(2) and link(2) operations
# used by Maildir writing tools
use v5.12;
use PublicInbox::TestCommon;
use autodie;
plan skip_all => 'KQNotify is only for *BSD systems' if $^O !~ /bsd/;
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

$w->cancel;
link("$tmpdir/new/tst", "$tmpdir/new/link2");
$hit = [ map { $_->fullname } $kqn->read ];
is_deeply($hit, [], 'link(2) not detected after cancel');

done_testing;
