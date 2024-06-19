use v5.12;
use autodie;
use Test::More;
use PublicInbox::Syscall;
use Socket qw(AF_UNIX SOCK_STREAM);
my $sendmsg_more = PublicInbox::Syscall->can('sendmsg_more') or
	plan skip_all => "sendmsg syscalls not defined on $^O";

socketpair(my $s1, my $s2, AF_UNIX, SOCK_STREAM, 0);
is $sendmsg_more->($s1, 'hello', 'world'), 10, 'sendmsg_more expected size';
is sysread($s2, my $buf, 11), 10, 'reader got expected size from sendmsg_more';
is $buf, 'helloworld', 'sendmsg_more sent expected message';

done_testing;
