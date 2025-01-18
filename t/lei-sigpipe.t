#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use v5.10.1;
use PublicInbox::TestCommon;
use POSIX qw(WTERMSIG WIFSIGNALED SIGPIPE);
use PublicInbox::OnDestroy;
use PublicInbox::Syscall qw($F_SETPIPE_SZ);
use autodie qw(close open pipe seek sysread);
use PublicInbox::IO qw(write_file);
my $inboxdir = $ENV{GIANT_INBOX_DIR};
SKIP: {
	$inboxdir // skip 'GIANT_INBOX_DIR unset to test large results', 1;
	require PublicInbox::Inbox;
	my $ibx = PublicInbox::Inbox->new({
		name => 'unconfigured-test',
		address => [ "test\@example.com" ],
		inboxdir => $inboxdir,
	});
	$ibx->search or xbail "GIANT_INBOX_DIR=$inboxdir has no search";
}

# undo systemd (and similar) ignoring SIGPIPE, since lei expects to be run
# from an interactive terminal:
# https://public-inbox.org/meta/20220227080422.gyqowrxomzu6gyin@sourcephile.fr/
my $oldSIGPIPE = $SIG{PIPE};
$SIG{PIPE} = 'DEFAULT';
my $cleanup = on_destroy(sub { $SIG{PIPE} = $oldSIGPIPE });

test_lei(sub {
	my $f = "$ENV{HOME}/big.eml";
	my $imported;
	for my $out ([], [qw(-f mboxcl2)], [qw(-f text)]) {
		pipe(my $r, my $w);
		my $size = ($F_SETPIPE_SZ && fcntl($w, $F_SETPIPE_SZ, 4096)) ||
		    65536;
		unless (-f $f) {
			my $fh = write_file '>', $f, <<'EOM';
From: big@example.com
Message-ID: <big@example.com>
EOM
			print $fh 'Subject:';
			print $fh (' '.('x' x 72)."\n") x (($size / 73) + 1);
			print $fh "\nbody\n";
			close $fh;
		}

		lei_ok(qw(import), $f) if $imported++ == 0;
		open my $errfh, '+>>', "$ENV{HOME}/stderr.log";
		my $opt = { run_mode => 0, 2 => $errfh, 1 => $w };
		my $cmd = [qw(lei q -q -t), @$out, 'z:1..'];
		push @$cmd, '--only='.$inboxdir if defined $inboxdir;
		my $tp = start_script($cmd, undef, $opt);
		close $w;
		vec(my $rvec = '', fileno($r), 1) = 1;
		if (!select($rvec, undef, undef, 30)) {
			seek $errfh, 0, 0;
			my $s = do { local $/; <$errfh> };
			xbail "lei q had no output after 30s, stderr=$s";
		}
		is(sysread($r, my $buf, 1), 1, 'read one byte');
		close $r; # trigger SIGPIPE
		$tp->join;
		ok(WIFSIGNALED($?), "signaled @$out");
		is(WTERMSIG($?), SIGPIPE, "got SIGPIPE @$out");
		seek $errfh, 0, 0;
		my $s = do { local $/; <$errfh> };
		is($s, '', "quiet after sigpipe @$out");
	}
});

done_testing;
