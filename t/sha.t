#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::SHA;
use Test::More;

{
	my $dig = PublicInbox::SHA->new(1);
	open my $fh, '<', 'COPYING' or die "open: $!";
	$dig->add(do { local $/; <$fh> });
	is($dig->hexdigest, '78e50e186b04c8fe1defaa098f1c192181b3d837',
		'AGPL-3 matches');
}

SKIP: {
	my $n = $ENV{TEST_LEAK_NR} or skip 'TEST_LEAK_NR unset', 1;
	for (1..$n) {
		PublicInbox::SHA->new(1)->add('hello')->digest;
		PublicInbox::SHA->new(1)->add('hello');
		PublicInbox::SHA->new(1);
	}
}

done_testing;
