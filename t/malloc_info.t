# Copyright (C) 2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use PublicInbox::Spawn ();

if (!PublicInbox::Spawn->can('inbox_malloc_info')) {
	plan skip_all => 'inbox_malloc_info not enabled';
}

open my $olderr, '>&', \*STDERR or die "dup stderr: $!";
open my $tmp, '+>', undef or die "tmpfile: $!";
open STDERR, '>&', $tmp or die "redirect stderr to \$tmp: $!";
my @x = map { '0' x (1024 * 1024) } (1..128);
my $cb = $SIG{CONT};
$cb->();
@x = ('hello');
PublicInbox::Spawn::inbox_malloc_info(0);
open STDERR, '>&', $olderr or die "restore stderr: $!";
sysseek($tmp, 0, 0) == 0 or die "sysseek: $!";
my @info = <$tmp>;
like($info[0], qr/</, 'output looks like XML');

done_testing;
