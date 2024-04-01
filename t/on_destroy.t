#!perl -w
use v5.12;
use Test::More;
use PublicInbox::OnDestroy;
use POSIX qw(_exit);
my @x;
my $od = on_destroy sub { push @x, 'hi' };
is_deeply(\@x, [], 'not called, yet');
undef $od;
is_deeply(\@x, [ 'hi' ], 'no args works');
$od = on_destroy sub { $x[0] = $_[0] }, 'bye';
is_deeply(\@x, [ 'hi' ], 'nothing changed while alive');
undef $od;
is_deeply(\@x, [ 'bye' ], 'arg passed');
$od = on_destroy sub { @x = @_ }, qw(x y);
undef $od;
is_deeply(\@x, [ 'x', 'y' ], '2 args passed');

open my $tmp, '+>>', undef or BAIL_OUT $!;
$tmp->autoflush(1);
$od = on_destroy sub { print $tmp "$$ DESTROY\n" };
my $pid = PublicInbox::OnDestroy::fork_tmp;
if ($pid == 0) { undef $od; _exit 0; };
waitpid($pid, 0);
is $?, 0, 'test process exited';
is(-s $tmp, 0, '$tmp is empty on pid mismatch');
$od->cancel;
undef $od;
is(-s $tmp, 0, '$tmp is empty after ->cancel');
$od = on_destroy sub { $tmp = $$ };
undef $od;
is($tmp, $$, '$tmp set to $$ by callback');

$od = on_destroy sub { $tmp = 'foo' };
$od->cancel;
$od = undef;
isnt($tmp, 'foo', '->cancel');

if (my $nr = $ENV{TEST_LEAK_NR}) {
	for (0..$nr) {
		$od = on_destroy sub { @x = @_ }, qw(x y);
	}
}

done_testing;
