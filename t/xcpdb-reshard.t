# Copyright (C) 2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
my @mods = qw(DBI DBD::SQLite Search::Xapian);
foreach my $mod (@mods) {
	eval "require $mod";
	plan skip_all => "missing $mod for $0" if $@;
};
require './t/common.perl';
require_git('2.6');
use File::Temp qw/tempdir/;
use PublicInbox::MIME;
use PublicInbox::InboxWritable;

my $mime = PublicInbox::MIME->create(
	header => [
		From => 'a@example.com',
		To => 'test@example.com',
		Subject => 'this is a subject',
		Date => 'Fri, 02 Oct 1993 00:00:00 +0000',
	],
	body => '',
);

my ($this) = (split('/', $0))[-1];
my $tmpdir = tempdir($this.'-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $ibx = PublicInbox::Inbox->new({
	mainrepo => "$tmpdir/testbox",
	name => $this,
	version => 2,
	-primary_address => 'test@example.com',
	indexlevel => 'medium',
});
my $path = 'blib/script';
my @xcpdb = ("$path/public-inbox-xcpdb", '-q');
my $nproc = 8;
my $ndoc = 13;
my $im = PublicInbox::InboxWritable->new($ibx, {nproc => $nproc})->importer(1);
for my $i (1..$ndoc) {
	$mime->header_set('Message-ID', "<m$i\@example.com>");
	ok($im->add($mime), "message $i added");
}
$im->done;
my @shards = grep(m!/\d+\z!, glob("$ibx->{mainrepo}/xap*/*"));
is(scalar(@shards), $nproc, 'got expected shards');
my $orig = $ibx->over->query_xover(1, $ndoc);
my %nums = map {; "$_->{num}" => 1 } @$orig;

# ensure we can go up or down in shards, or stay the same:
for my $R (qw(2 4 1 3 3)) {
	delete $ibx->{search}; # release old handles
	is(system(@xcpdb, "-R$R", $ibx->{mainrepo}), 0, "xcpdb -R$R");
	my @new_shards = grep(m!/\d+\z!, glob("$ibx->{mainrepo}/xap*/*"));
	is(scalar(@new_shards), $R, 'resharded to two shards');
	my $msgs = $ibx->search->query('s:this');
	is(scalar(@$msgs), $ndoc, 'got expected docs after resharding');
	my %by_mid = map {; "$_->{mid}" => $_ } @$msgs;
	ok($by_mid{"m$_\@example.com"}, "$_ exists") for (1..$ndoc);

	delete $ibx->{search}; # release old handles

	# ensure docids in Xapian match NNTP article numbers
	my $tot = 0;
	my %tmp = %nums;
	foreach my $d (@new_shards) {
		my $xdb = Search::Xapian::Database->new($d);
		$tot += $xdb->get_doccount;
		my $it = $xdb->postlist_begin('');
		my $end = $xdb->postlist_end('');
		for (; $it != $end; $it++) {
			my $docid = $it->get_docid;
			if ($xdb->get_document($docid)) {
				ok(delete($tmp{$docid}), "saw #$docid");
			}
		}
	}
	is(scalar keys %tmp, 0, 'all docids seen');
}

done_testing();
1;