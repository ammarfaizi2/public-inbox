# Copyright (C) all contributors <meta@public-inbox.org>
# Licensed the same as Danga::Socket (and Perl5)
# License: GPL-1.0+ or Artistic-1.0-Perl
#  <https://www.gnu.org/licenses/gpl-1.0.txt>
#  <https://dev.perl.org/licenses/artistic.html>
use v5.12;
use Test::More;
unless (eval { require IO::KQueue }) {
	my $m = ($^O =~ /bsd/ || $^O eq 'dragonfly') ?
		"no IO::KQueue, skipping $0: $@" :
		'DSKQXS is only for *BSD systems';
	plan skip_all => $m;
}

local $ENV{TEST_IOPOLLER} = 'PublicInbox::DSKQXS';
require './t/ds-poll.t';
