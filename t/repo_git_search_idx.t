# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use File::Temp qw/tempdir/;
use_ok 'PublicInbox::RepoGitSearchIdx';
my $test = require './t/repobrowse_common_git.perl';
my $git_dir = $test->{git_dir};
my $xdir = "$git_dir/rg";
my $idx = PublicInbox::RepoGitSearchIdx->new($git_dir, $xdir);
ok($idx->xdb && -d $xdir, 'Xapian dir created');
$idx->index_sync;

my $mset = $idx->query('bs:"add header"');
my $doc;
$doc = $_->get_document foreach $mset->items;
ok($doc, 'got document');
is('cb3b92d257e628b512a2eee0861f8935c594cd12', $doc->get_data, 'DATA OK');

foreach my $q (qw(id:cb3b92d257e628b512a2eee0861f8935c594cd12 id:cb3b92d2*)) {
	$mset = $idx->query($q);
	$doc = undef;
	$doc = $_->get_document foreach $mset->items;
	ok($doc, "got document for $q");
}

done_testing();
