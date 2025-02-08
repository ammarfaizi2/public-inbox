# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# unit tests for common (public-facing) daemon code
use v5.12;
use autodie;
use PublicInbox::TestCommon;
use Socket qw(SOCK_STREAM);
require_ok 'PublicInbox::Daemon';
use PublicInbox::IO qw(poll_in);

# $fn is stream_hup or tcp_hup
my $ck_hup = sub {
	my ($s, $connect, $fn, $type) = @_;
	my $c = $connect->();
	poll_in $s; # needed on *BSD
	my $addr = accept(my $acc, $s);
	close $c;
	my $ck = PublicInbox::Daemon->can($fn);
	poll_in($acc) if $^O ne 'linux'; # netbsd needs this, at least...
	ok $ck->($acc), "$fn detected close ($type)";
	$c = $connect->();
	syswrite $c, 'hi';
	poll_in $s; # needed on *BSD
	$addr = accept($acc, $s);
	ok !$ck->($acc), "$fn false when still established ($type)";
};

{
	my $tmpdir = tmpdir;
	my $l = "$tmpdir/named.sock";
	my $s = IO::Socket::UNIX->new(Listen => 5, Local => $l,
				Type => SOCK_STREAM) or
			xbail "bind+listen($l): $!";
	my $connect = sub {
		IO::Socket::UNIX->new(Peer => $l, Type => SOCK_STREAM) or
			xbail "connect($l): $!";
	};
	$ck_hup->($s, $connect, 'stream_hup', 'UNIX');
}

{
	my $s = tcp_server;
	my $tcp_conn = sub { tcp_connect($s) };
	$ck_hup->($s, $tcp_conn, 'stream_hup', 'TCP');
	$ck_hup->($s, $tcp_conn, 'tcp_hup', 'TCP');
}

SKIP: {
	$^O =~ /\A(?:linux|freebsd|netbsd|openbsd)\z/ or
		skip "no TCP_INFO support $^O", 1;
	isnt \&PublicInbox::Daemon::stream_hup,
		\&PublicInbox::Daemon::tcp_hup,
		"stream_hup and tcp_hup are different on \$^O=$^O";
}

done_testing;
