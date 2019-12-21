# Copyright (C) 2015-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# internal APIs used only for tests
package PublicInbox::TestCommon;
use strict;
use parent qw(Exporter);
use Fcntl qw(FD_CLOEXEC F_SETFD F_GETFD :seek);
use POSIX qw(dup2);
use IO::Socket::INET;
our @EXPORT = qw(tmpdir tcp_server tcp_connect require_git
	run_script start_script key2sub);

sub tmpdir (;$) {
	my ($base) = @_;
	require File::Temp;
	unless (defined $base) {
		($base) = ($0 =~ m!\b([^/]+)\.[^\.]+\z!);
	}
	my $tmpdir = File::Temp->newdir("pi-$base-$$-XXXXXX", TMPDIR => 1);
	($tmpdir->dirname, $tmpdir);
}

sub tcp_server () {
	IO::Socket::INET->new(
		LocalAddr => '127.0.0.1',
		ReuseAddr => 1,
		Proto => 'tcp',
		Type => Socket::SOCK_STREAM(),
		Listen => 1024,
		Blocking => 0,
	)
}

sub tcp_connect {
	my ($dest, %opt) = @_;
	my $s = IO::Socket::INET->new(
		Proto => 'tcp',
		Type => Socket::SOCK_STREAM(),
		PeerAddr => $dest->sockhost . ':' . $dest->sockport,
		%opt,
	);
	$s->autoflush(1);
	$s;
}

sub require_git ($;$) {
	my ($req, $maybe) = @_;
	my ($req_maj, $req_min) = split(/\./, $req);
	my ($cur_maj, $cur_min) = (`git --version` =~ /version (\d+)\.(\d+)/);

	my $req_int = ($req_maj << 24) | ($req_min << 16);
	my $cur_int = ($cur_maj << 24) | ($cur_min << 16);
	if ($cur_int < $req_int) {
		return 0 if $maybe;
		Test::More::plan(skip_all =>
				"git $req+ required, have $cur_maj.$cur_min");
	}
	1;
}

sub key2script ($) {
	my ($key) = @_;
	return $key if (index($key, '/') >= 0);
	# n.b. we may have scripts which don't start with "public-inbox" in
	# the future:
	$key =~ s/\A([-\.])/public-inbox$1/;
	'blib/script/'.$key;
}

my @io_mode = ([ *STDIN{IO}, '<&' ], [ *STDOUT{IO}, '>&' ],
		[ *STDERR{IO}, '>&' ]);

sub _prepare_redirects ($) {
	my ($fhref) = @_;
	my $orig_io = [];
	for (my $fd = 0; $fd <= $#io_mode; $fd++) {
		my $fh = $fhref->[$fd] or next;
		my ($oldfh, $mode) = @{$io_mode[$fd]};
		open my $orig, $mode, $oldfh or die "$$oldfh $mode stash: $!";
		$orig_io->[$fd] = $orig;
		open $oldfh, $mode, $fh or die "$$oldfh $mode redirect: $!";
	}
	$orig_io;
}

sub _undo_redirects ($) {
	my ($orig_io) = @_;
	for (my $fd = 0; $fd <= $#io_mode; $fd++) {
		my $fh = $orig_io->[$fd] or next;
		my ($oldfh, $mode) = @{$io_mode[$fd]};
		open $oldfh, $mode, $fh or die "$$oldfh $mode redirect: $!";
	}
}

# $opt->{run_mode} (or $ENV{TEST_RUN_MODE}) allows chosing between
# three ways to spawn our own short-lived Perl scripts for testing:
#
# 0 - (fork|vfork) + execve, the most realistic but slowest
# 1 - preloading and running in a forked subprocess (fast)
# 2 - preloading and running in current process (slightly faster than 1)
#
# 2 is not compatible with scripts which use "exit" (which we'll try to
# avoid in the future).
# The default is 2.
our $run_script_exit_code;
sub RUN_SCRIPT_EXIT () { "RUN_SCRIPT_EXIT\n" };
sub run_script_exit (;$) {
	$run_script_exit_code = $_[0] // 0;
	die RUN_SCRIPT_EXIT;
}

my %cached_scripts;
sub key2sub ($) {
	my ($key) = @_;
	$cached_scripts{$key} //= do {
		my $f = key2script($key);
		open my $fh, '<', $f or die "open $f: $!";
		my $str = do { local $/; <$fh> };
		my $pkg = (split(m!/!, $f))[-1];
		$pkg =~ s/([a-z])([a-z0-9]+)(\.t)?\z/\U$1\E$2/;
		$pkg .= "_T" if $3;
		$pkg =~ tr/-.//d;
		$pkg = "PublicInbox::TestScript::$pkg";
		eval <<EOF;
package $pkg;
use strict;
use subs qw(exit);

*exit = *PublicInbox::TestCommon::run_script_exit;
sub main {
# the below "line" directive is a magic comment, see perlsyn(1) manpage
# line 1 "$f"
$str
	0;
}
1;
EOF
		$pkg->can('main');
	}
}

sub _run_sub ($$$) {
	my ($sub, $key, $argv) = @_;
	local @ARGV = @$argv;
	$run_script_exit_code = undef;
	my $exit_code = eval { $sub->(@$argv) };
	if ($@ eq RUN_SCRIPT_EXIT) {
		$@ = '';
		$exit_code = $run_script_exit_code;
		$? = ($exit_code << 8);
	} elsif (defined($exit_code)) {
		$? = ($exit_code << 8);
	} elsif ($@) { # mimic die() behavior when uncaught
		warn "E: eval-ed $key: $@\n";
		$? = ($! << 8) if $!;
		$? = (255 << 8) if $? == 0;
	} else {
		die "BUG: eval-ed $key: no exit code or \$@\n";
	}
}

sub run_script ($;$$) {
	my ($cmd, $env, $opt) = @_;
	my ($key, @argv) = @$cmd;
	my $run_mode = $ENV{TEST_RUN_MODE} // $opt->{run_mode} // 1;
	my $sub = $run_mode == 0 ? undef : key2sub($key);
	my $fhref = [];
	my $spawn_opt = {};
	for my $fd (0..2) {
		my $redir = $opt->{$fd};
		next unless ref($redir);
		open my $fh, '+>', undef or die "open: $!";
		$fhref->[$fd] = $fh;
		$spawn_opt->{$fd} = fileno($fh);
		next if $fd > 0;
		$fh->autoflush(1);
		print $fh $$redir or die "print: $!";
		seek($fh, 0, SEEK_SET) or die "seek: $!";
	}
	if ($run_mode == 0) {
		# spawn an independent new process, like real-world use cases:
		require PublicInbox::Spawn;
		my $cmd = [ key2script($key), @argv ];
		my $pid = PublicInbox::Spawn::spawn($cmd, $env, $spawn_opt);
		defined($pid) or die "spawn: $!";
		if (defined $pid) {
			my $r = waitpid($pid, 0);
			defined($r) or die "waitpid: $!";
			$r == $pid or die "waitpid: expected $pid, got $r";
		}
	} else { # localize and run everything in the same process:
		# note: "local *STDIN = *STDIN;" and so forth did not work in
		# old versions of perl
		local %ENV = $env ? (%ENV, %$env) : %ENV;
		local %SIG = %SIG;
		local $0 = join(' ', @$cmd);
		my $orig_io = _prepare_redirects($fhref);
		_run_sub($sub, $key, \@argv);
		_undo_redirects($orig_io);
	}

	# slurp the redirects back into user-supplied strings
	for my $fd (1..2) {
		my $fh = $fhref->[$fd] or next;
		seek($fh, 0, SEEK_SET) or die "seek: $!";
		my $redir = $opt->{$fd};
		local $/;
		$$redir = <$fh>;
	}
	$? == 0;
}

sub wait_for_tail () { sleep(2) }

sub start_script {
	my ($cmd, $env, $opt) = @_;
	my ($key, @argv) = @$cmd;
	my $run_mode = $ENV{TEST_RUN_MODE} // $opt->{run_mode} // 1;
	my $sub = $run_mode == 0 ? undef : key2sub($key);
	my $tail_pid;
	if (my $tail_cmd = $ENV{TAIL}) {
		my @paths;
		for (@argv) {
			next unless /\A--std(?:err|out)=(.+)\z/;
			push @paths, $1;
		}
		if (@paths) {
			defined($tail_pid = fork) or die "fork: $!\n";
			if ($tail_pid == 0) {
				# make sure files exist, first
				open my $fh, '>>', $_ for @paths;
				open(STDOUT, '>&STDERR') or die "1>&2: $!";
				exec(split(' ', $tail_cmd), @paths);
				die "$tail_cmd failed: $!";
			}
			wait_for_tail();
		}
	}
	defined(my $pid = fork) or die "fork: $!\n";
	if ($pid == 0) {
		# pretend to be systemd (cf. sd_listen_fds(3))
		# 3 == SD_LISTEN_FDS_START
		my $fd;
		for ($fd = 0; 1; $fd++) {
			my $s = $opt->{$fd};
			last if $fd >= 3 && !defined($s);
			next unless $s;
			my $fl = fcntl($s, F_GETFD, 0);
			if (($fl & FD_CLOEXEC) != FD_CLOEXEC) {
				warn "got FD:".fileno($s)." w/o CLOEXEC\n";
			}
			fcntl($s, F_SETFD, $fl &= ~FD_CLOEXEC);
			dup2(fileno($s), $fd) or die "dup2 failed: $!\n";
		}
		%ENV = (%ENV, %$env) if $env;
		my $fds = $fd - 3;
		if ($fds > 0) {
			$ENV{LISTEN_PID} = $$;
			$ENV{LISTEN_FDS} = $fds;
		}
		$0 = join(' ', @$cmd);
		if ($sub) {
			_run_sub($sub, $key, \@argv);
			POSIX::_exit($? >> 8);
		} else {
			exec(key2script($key), @argv);
			die "FAIL: ",join(' ', $key, @argv), ": $!\n";
		}
	}
	PublicInboxTestProcess->new($pid, $tail_pid);
}

package PublicInboxTestProcess;
use strict;

# prevent new threads from inheriting these objects
sub CLONE_SKIP { 1 }

sub new {
	my ($klass, $pid, $tail_pid) = @_;
	bless { pid => $pid, tail_pid => $tail_pid, owner => $$ }, $klass;
}

sub kill {
	my ($self, $sig) = @_;
	CORE::kill($sig // 'TERM', $self->{pid});
}

sub join {
	my ($self) = @_;
	my $pid = delete $self->{pid} or return;
	my $ret = waitpid($pid, 0);
	defined($ret) or die "waitpid($pid): $!";
	$ret == $pid or die "waitpid($pid) != $ret";
}

sub DESTROY {
	my ($self) = @_;
	return if $self->{owner} != $$;
	if (my $tail = delete $self->{tail_pid}) {
		PublicInbox::TestCommon::wait_for_tail();
		CORE::kill('TERM', $tail);
	}
	my $pid = delete $self->{pid} or return;
	CORE::kill('TERM', $pid);
}

1;