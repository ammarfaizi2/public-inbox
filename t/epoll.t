#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use Test::More;
use autodie;
use PublicInbox::Syscall qw(EPOLLOUT);
plan skip_all => 'not Linux' if $^O ne 'linux';
PublicInbox::Syscall->can('epoll_pwait') or
	plan skip_all => 'Linux kernel too old for epoll_pwait?';
require_ok 'PublicInbox::Epoll';
my $ep = PublicInbox::Epoll->new;
pipe(my $r, my $w);
is($ep->ep_add($w, EPOLLOUT), 0, 'epoll_ctl pipe EPOLLOUT');

my @events;
$ep->ep_wait(10000, \@events);
is(scalar(@events), 1, 'got one event');
is($events[0], fileno($w), 'got expected FD');
close $w;
$ep->ep_wait(0, \@events);
is(scalar(@events), 0, 'epoll_pwait timeout');

done_testing;
