# Copyright (C) all contributors <meta@public-inbox.org>
# Licensed the same as Danga::Socket (and Perl5)
# License: GPL-1.0+ or Artistic-1.0-Perl
#  <https://www.gnu.org/licenses/gpl-1.0.txt>
#  <https://dev.perl.org/licenses/artistic.html>
use v5.12;
use Test::More;
use PublicInbox::Syscall qw(EPOLLIN EPOLLOUT EPOLLONESHOT);
use autodie qw(close pipe syswrite);
my $cls = $ENV{TEST_IOPOLLER} // 'PublicInbox::DSPoll';
use_ok $cls;
my $p = $cls->new;

my ($r, $w, $x, $y);
pipe($r, $w);
pipe($x, $y);
is($p->ep_add($r, EPOLLIN), 0, 'add EPOLLIN');
my $events = [];
$p->ep_wait(0, $events);
is_deeply($events, [], 'no events set');
is($p->ep_add($w, EPOLLOUT|EPOLLONESHOT), 0, 'add EPOLLOUT|EPOLLONESHOT');
$p->ep_wait(-1, $events);
is(scalar(@$events), 1, 'got POLLOUT event');
is($events->[0], fileno($w), '$w ready');

$p->ep_wait(0, $events);
is(scalar(@$events), 0, 'nothing ready after oneshot');
is_deeply($events, [], 'no events set after oneshot');

syswrite($w, '1') == 1 or die;
for my $t (0..1) {
	$p->ep_wait($t, $events);
	is($events->[0], fileno($r), "level-trigger POLLIN ready #$t");
	is(scalar(@$events), 1, "only event ready #$t");
}
syswrite($y, '1') == 1 or die;
is($p->ep_add($x, EPOLLIN|EPOLLONESHOT), 0, 'EPOLLIN|EPOLLONESHOT add');
$p->ep_wait(-1, $events);
is(scalar @$events, 2, 'ep_wait has 2 ready');
my @fds = sort @$events;
my @exp = sort((fileno($r), fileno($x)));
is_deeply(\@fds, \@exp, 'got both ready FDs');

is($p->ep_del($r, 0), 0, 'EPOLL_CTL_DEL OK');
$p->ep_wait(0, $events);
is(scalar @$events, 0, 'nothing ready after EPOLL_CTL_DEL');

is($p->ep_add($r, EPOLLIN), 0, 're-add');
SKIP: {
	$cls =~ m!::(?:DSPoll|Select)\z! or
		skip 'EBADF test for select|poll only', 1;
	my $old_fd = fileno($r);
	close $r;
	my @w;
	eval {
		local $SIG{__WARN__} = sub { push @w, @_ };
		$p->ep_wait(0, $events);
	};
	ok($@, 'error detected from bad FD');
	ok($!{EBADF}, 'EBADF errno set');
	@w and ok(grep(/\bFD=$old_fd invalid/, @w), 'carps invalid FD');
}

done_testing;
