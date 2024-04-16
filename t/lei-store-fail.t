#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# ensure we detect errors in lei/store
use v5.12;
use PublicInbox::TestCommon;
use autodie qw(pipe open close seek);
use Fcntl qw(SEEK_SET);
use File::Path qw(remove_tree);

my $start_home = $ENV{HOME}; # bug guard
my $utf8_oid = '9bf1002c49eb075df47247b74d69bcd555e23422';
test_lei(sub {
	lei_ok qw(import -q t/plack-qp.eml); # start the store
	ok(!lei(qw(blob --mail), $utf8_oid), 't/utf8.eml not imported, yet');

	my $opt;
	pipe($opt->{0}, my $in_w);
	open $opt->{1}, '+>', undef;
	open $opt->{2}, '+>', undef;
	$opt->{-CLOFORK} = [ $in_w ];
	my $cmd = [ qw(lei import -q -F mboxrd) ];
	my $tp = start_script($cmd, undef, $opt);
	close $opt->{0};
	$in_w->autoflush(1);
	print $in_w <<EOM or xbail "print: $!";
From k\@y Fri Oct  2 00:00:00 1993
From: <k\@example.com>
Date: Sat, 02 Oct 2010 00:00:00 +0000
Subject: hi
Message-ID: <0\@t>

will this save?
EOM
	# import another message w/ delay while mboxrd import is still running
	lei_ok qw(import -q --commit-delay=300 t/utf8.eml);
	lei_ok qw(blob --mail), $utf8_oid,
		\'blob immediately available despite --commit-delay';
	lei_ok qw(q m:testmessage@example.com);
	is($lei_out, "[null]\n", 'delayed commit is unindexed');

	# make immediate ->sto_barrier_request fail from mboxrd import:
	remove_tree("$ENV{HOME}/.local/share/lei/store");
	# subsequent lei commands are undefined behavior,
	# but we need to make sure the current lei command fails:

	close $in_w; # should trigger ->done
	$tp->join;
	isnt($?, 0, 'lei import -F mboxrd error code set on failure');
	is(-s $opt->{1}, 0, 'nothing in stdout');
	isnt(-s $opt->{2}, 0, 'stderr not empty');
	seek($opt->{2}, 0, SEEK_SET);
	my @err = readline($opt->{2});
	ok(grep(!/^#/, @err), 'noted error in stderr') or diag "@err";
});

done_testing;
