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
test_lei(sub {
	lei_ok qw(import -q t/plack-qp.eml); # start the store
	my $opt;
	pipe($opt->{0}, my $in_w);
	open $opt->{1}, '+>', undef;
	open $opt->{2}, '+>', undef;
	$opt->{-CLOFORK} = [ $in_w ];
	my $cmd = [ qw(lei import -q -F mboxrd) ];
	my $tp = start_script($cmd, undef, $opt);
	close $opt->{0};
	$in_w->autoflush(1);
	for (1..500) { # need to fill up 64k read buffer
		print $in_w <<EOM or xbail "print $!";
From k\@y Fri Oct  2 00:00:00 1993
From: <k\@example.com>
Date: Sat, 02 Oct 2010 00:00:00 +0000
Subject: hi
Message-ID: <$_\@t>

will this save?
EOM
	}
	tick 0.2; # XXX ugh, this is so hacky

	# make sto_done_request fail:
	remove_tree("$ENV{HOME}/.local/share/lei/store");
	# subsequent lei commands are undefined behavior,
	# but we need to make sure the current lei command fails:

	close $in_w; # should trigger ->done
	$tp->join;
	isnt($?, 0, 'lei import error code set on failure');
	is(-s $opt->{1}, 0, 'nothing in stdout');
	isnt(-s $opt->{2}, 0, 'stderr not empty');
	seek($opt->{2}, 0, SEEK_SET);
	my @err = readline($opt->{2});
	ok(grep(!/^#/, @err), 'noted error in stderr') or diag "@err";
});

done_testing;
