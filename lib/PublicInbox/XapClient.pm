#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# This talks to (XapHelperCxx.pm + xap_helper.h) or XapHelper.pm
# and will eventually allow users with neither XS nor SWIG Perl
# bindings to use Xapian as long as they have Xapian development
# headers/libs and a C++ compiler
package PublicInbox::XapClient;
use v5.12;
use PublicInbox::Spawn qw(spawn);
use Socket qw(AF_UNIX SOCK_SEQPACKET MSG_EOR);
use PublicInbox::IPC;

sub mkreq {
	my ($self, $ios, @arg) = @_;
	my ($r, $w, $n);
	if (!defined($ios->[0])) {
		pipe($r, $w) or die "pipe: $!";
		$ios->[0] = $w;
	}
	my @fds = map fileno($_), @$ios;
	my $buf = join("\0", @arg, '');
	$n = PublicInbox::IPC::send_cmd($self->{io}, \@fds, $buf, MSG_EOR) //
		die "send_cmd: $!";
	$n == length($buf) or die "send_cmd: $n != ".length($buf);
	$r;
}

sub start_helper {
	my @argv = @_;
	socketpair(my $sock, my $in, AF_UNIX, SOCK_SEQPACKET, 0) or
		die "socketpair: $!";
	my $cls = ($ENV{PI_NO_CXX} ? undef : eval {
			require PublicInbox::XapHelperCxx;
			PublicInbox::XapHelperCxx::check_build();
			'PublicInbox::XapHelperCxx';
		}) // do {
			require PublicInbox::XapHelper;
			'PublicInbox::XapHelper';
		};
	# ensure the child process has the same @INC we do:
	my $env = { PERL5LIB => join(':', @INC) };
	my $pid = spawn([$^X, ($^W ? ('-w') : ()), "-M$cls", '-e',
				$cls.'::start(@ARGV)', '--', @argv],
			$env, { 0 => $in });
	((bless { io => $sock, impl => $cls }, __PACKAGE__), $pid);
}

1;
