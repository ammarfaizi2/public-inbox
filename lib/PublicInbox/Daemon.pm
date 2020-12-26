# Copyright (C) 2015-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# contains common daemon code for the httpd, imapd, and nntpd servers.
# This may be used for read-only IMAP server if we decide to implement it.
package PublicInbox::Daemon;
use strict;
use warnings;
use Getopt::Long qw/:config gnu_getopt no_ignore_case auto_abbrev/;
use IO::Handle; # ->autoflush
use IO::Socket;
use POSIX qw(WNOHANG :signal_h);
use Socket qw(IPPROTO_TCP SOL_SOCKET);
sub SO_ACCEPTFILTER () { 0x1000 }
use Cwd qw/abs_path/;
STDOUT->autoflush(1);
STDERR->autoflush(1);
use PublicInbox::DS qw(now);
use PublicInbox::Syscall qw($SFD_NONBLOCK);
require PublicInbox::Listener;
use PublicInbox::EOFpipe;
use PublicInbox::Sigfd;
my @CMD;
my ($set_user, $oldset);
my (@cfg_listen, $stdout, $stderr, $group, $user, $pid_file, $daemonize);
my $worker_processes = 1;
my @listeners;
my %pids;
my %tls_opt; # scheme://sockname => args for IO::Socket::SSL->start_SSL
my $reexec_pid;
my ($uid, $gid);
my ($default_cert, $default_key);
my %KNOWN_TLS = ( 443 => 'https', 563 => 'nntps', 993 => 'imaps' );
my %KNOWN_STARTTLS = ( 119 => 'nntp', 143 => 'imap' );

sub accept_tls_opt ($) {
	my ($opt_str) = @_;
	# opt_str: opt1=val1,opt2=val2 (opt may repeat for multi-value)
	require PublicInbox::TLS;
	my $o = {};
	# allow ',' as delimiter since '&' is shell-unfriendly
	foreach (split(/[,&]/, $opt_str)) {
		my ($k, $v) = split(/=/, $_, 2);
		push @{$o->{$k} ||= []}, $v;
	}

	# key may be a part of cert.  At least
	# p5-io-socket-ssl/example/ssl_server.pl has this fallback:
	$o->{cert} //= [ $default_cert ];
	$o->{key} //= defined($default_key) ? [ $default_key ] : $o->{cert};
	my %ctx_opt = (SSL_server => 1);
	# parse out hostname:/path/to/ mappings:
	foreach my $k (qw(cert key)) {
		my $x = $ctx_opt{'SSL_'.$k.'_file'} = {};
		foreach my $path (@{$o->{$k}}) {
			my $host = '';
			$path =~ s/\A([^:]+):// and $host = $1;
			$x->{$host} = $path;
		}
	}
	my $ctx = IO::Socket::SSL::SSL_Context->new(%ctx_opt) or
		die 'SSL_Context->new: '.PublicInbox::TLS::err();

	# save ~34K per idle connection (cf. SSL_CTX_set_mode(3ssl))
	# RSS goes from 346MB to 171MB with 10K idle NNTPS clients on amd64
	# cf. https://rt.cpan.org/Ticket/Display.html?id=129463
	my $mode = eval { Net::SSLeay::MODE_RELEASE_BUFFERS() };
	if ($mode && $ctx->{context}) {
		eval { Net::SSLeay::CTX_set_mode($ctx->{context}, $mode) };
		warn "W: $@ (setting SSL_MODE_RELEASE_BUFFERS)\n" if $@;
	}

	{ SSL_server => 1, SSL_startHandshake => 0, SSL_reuse_ctx => $ctx };
}

sub daemon_prepare ($) {
	my ($default_listen) = @_;
	my $listener_names = {}; # sockname => IO::Handle
	$oldset = PublicInbox::Sigfd::block_signals();
	@CMD = ($0, @ARGV);
	my ($prog) = ($CMD[0] =~ m!([^/]+)\z!g);
	my $help = <<EOF;
usage: $prog [-l ADDRESS] [--cert=FILE] [--key=FILE]

options:

  -l ADDRESS    address to listen on (default: $default_listen)
  --cert=FILE   default SSL/TLS certificate
  --key=FILE    default SSL/TLS certificate
  -W WORKERS    number of worker processes to spawn (default: 1)

See public-inbox-daemon(8) and $prog(1) man pages for more.
EOF
	my %opt = (
		'l|listen=s' => \@cfg_listen,
		'1|stdout=s' => \$stdout,
		'2|stderr=s' => \$stderr,
		'W|worker-processes=i' => \$worker_processes,
		'P|pid-file=s' => \$pid_file,
		'u|user=s' => \$user,
		'g|group=s' => \$group,
		'D|daemonize' => \$daemonize,
		'cert=s' => \$default_cert,
		'key=s' => \$default_key,
		'help|h' => \(my $show_help),
	);
	GetOptions(%opt) or die $help;
	if ($show_help) { print $help; exit 0 };

	if (defined $pid_file && $pid_file =~ /\.oldbin\z/) {
		die "--pid-file cannot end with '.oldbin'\n";
	}
	@listeners = inherit($listener_names);

	# allow socket-activation users to set certs once and not
	# have to configure each socket:
	my @inherited_names = keys(%$listener_names) if defined($default_cert);

	# ignore daemonize when inheriting
	$daemonize = undef if scalar @listeners;

	push @cfg_listen, $default_listen unless (@listeners || @cfg_listen);

	foreach my $l (@cfg_listen) {
		my $orig = $l;
		my $scheme = '';
		if ($l =~ s!\A([^:]+)://!!) {
			$scheme = $1;
		} elsif ($l =~ /\A(?:\[[^\]]+\]|[^:]+):([0-9])+/) {
			my $s = $KNOWN_TLS{$1} // $KNOWN_STARTTLS{$1};
			$scheme = $s if defined $s;
		}
		if ($l =~ s!/?\?(.+)\z!!) {
			$tls_opt{"$scheme://$l"} = accept_tls_opt($1);
		} elsif (defined($default_cert)) {
			$tls_opt{"$scheme://$l"} = accept_tls_opt('');
		} elsif ($scheme =~ /\A(?:https|imaps|imaps)\z/) {
			die "$orig specified w/o cert=\n";
		}
		# TODO: use scheme to load either NNTP.pm or HTTP.pm

		next if $listener_names->{$l}; # already inherited
		my (%o, $sock_pkg);
		if (index($l, '/') == 0) {
			$sock_pkg = 'IO::Socket::UNIX';
			eval "use $sock_pkg";
			die $@ if $@;
			%o = (Type => SOCK_STREAM, Peer => $l);
			if (-S $l) {
				my $c = $sock_pkg->new(%o);
				if (!defined($c) && $!{ECONNREFUSED}) {
					unlink $l or die
"failed to unlink stale socket=$l: $!\n";
				} # else: let the bind fail
			}
			$o{Local} = delete $o{Peer};
		} else {
			# both work for IPv4, too
			for (qw(IO::Socket::IP IO::Socket::INET6)) {
				$sock_pkg = $_;
				eval "use $sock_pkg";
				$@ or last;
			}
			die $@ if $@;
			%o = (LocalAddr => $l, ReuseAddr => 1, Proto => 'tcp');
		}
		$o{Listen} = 1024;
		my $prev = umask 0000;
		my $s = eval { $sock_pkg->new(%o) };
		warn "error binding $l: $! ($@)\n" unless $s;
		umask $prev;
		if ($s) {
			$listener_names->{sockname($s)} = $s;
			$s->blocking(0);
			push @listeners, $s;
		}
	}

	# cert/key options in @cfg_listen takes precedence when inheriting,
	# but map well-known inherited ports if --listen isn't specified
	# at all
	for my $sockname (@inherited_names) {
		$sockname =~ /:([0-9]+)\z/ or next;
		if (my $scheme = $KNOWN_TLS{$1}) {
			$tls_opt{"$scheme://$sockname"} ||= accept_tls_opt('');
		} elsif (($scheme = $KNOWN_STARTTLS{$1})) {
			next if $tls_opt{"$scheme://$sockname"};
			$tls_opt{''} ||= accept_tls_opt('');
		}
	}

	die "No listeners bound\n" unless @listeners;
}

sub check_absolute ($$) {
	my ($var, $val) = @_;
	if (defined $val && index($val, '/') != 0) {
		die
"--$var must be an absolute path when using --daemonize: $val\n";
	}
}

sub daemonize () {
	if ($daemonize) {
		foreach my $i (0..$#ARGV) {
			my $arg = $ARGV[$i];
			next unless -e $arg;
			$ARGV[$i] = abs_path($arg);
		}
		check_absolute('stdout', $stdout);
		check_absolute('stderr', $stderr);
		check_absolute('pid-file', $pid_file);

		chdir '/' or die "chdir failed: $!";
	}
	if (defined($pid_file) || defined($group) || defined($user)) {
		eval { require Net::Server::Daemonize; 1 } // die <<EOF;
Net::Server required for --pid-file, --group, --user
$@
EOF
	}
	Net::Server::Daemonize::check_pid_file($pid_file) if defined $pid_file;
	$uid = Net::Server::Daemonize::get_uid($user) if defined $user;
	if (defined $group) {
		$gid = Net::Server::Daemonize::get_gid($group);
		$gid = (split /\s+/, $gid)[0];
	} elsif (defined $uid) {
		$gid = (getpwuid($uid))[3];
	}

	# We change users in the worker to ensure upgradability,
	# The upgrade will create the ".oldbin" pid file in the
	# same directory as the given pid file.
	$uid and $set_user = sub {
		$set_user = undef;
		Net::Server::Daemonize::set_user($uid, $gid);
	};

	if ($daemonize) {
		my $pid = fork;
		die "could not fork: $!\n" unless defined $pid;
		exit if $pid;

		open(STDIN, '+<', '/dev/null') or
					die "redirect stdin failed: $!\n";
		open STDOUT, '>&STDIN' or die "redirect stdout failed: $!\n";
		open STDERR, '>&STDIN' or die "redirect stderr failed: $!\n";
		POSIX::setsid();
		$pid = fork;
		die "could not fork: $!\n" unless defined $pid;
		exit if $pid;
	}
	return unless defined $pid_file;

	write_pid($pid_file);
	# for ->DESTROY:
	bless { pid => $$, pid_file => \$pid_file }, __PACKAGE__;
}

sub worker_quit { # $_[0] = signal name or number (unused)
	# killing again terminates immediately:
	exit unless @listeners;

	$_->close foreach @listeners; # call PublicInbox::DS::close
	@listeners = ();
	my $proc_name;
	my $warn = 0;
	# drop idle connections and try to quit gracefully
	PublicInbox::DS->SetPostLoopCallback(sub {
		my ($dmap, undef) = @_;
		my $n = 0;
		my $now = now();

		foreach my $s (values %$dmap) {
			$s->can('busy') or next;
			if ($s->busy($now)) {
				++$n;
			} else {
				# close as much as possible, early as possible
				$s->close;
			}
		}
		if ($n) {
			if (($warn + 5) < now()) {
				warn "$$ quitting, $n client(s) left\n";
				$warn = now();
			}
			unless (defined $proc_name) {
				$proc_name = (split(/\s+/, $0))[0];
				$proc_name =~ s!\A.*?([^/]+)\z!$1!;
			}
			$0 = "$proc_name quitting, $n client(s) left";
		}
		$n; # true: loop continues, false: loop breaks
	});
}

sub reopen_logs {
	if ($stdout) {
		open STDOUT, '>>', $stdout or
			warn "failed to redirect stdout to $stdout: $!\n";
		STDOUT->autoflush(1);
		do_chown($stdout);
	}
	if ($stderr) {
		open STDERR, '>>', $stderr or
			warn "failed to redirect stderr to $stderr: $!\n";
		STDERR->autoflush(1);
		do_chown($stderr);
	}
}

sub sockname ($) {
	my ($s) = @_;
	my $addr = getsockname($s) or return;
	my ($host, $port) = host_with_port($addr);
	if ($port == 0 && $host eq '127.0.0.1') {
		my ($path) = Socket::sockaddr_un($addr);
		return $path;
	}
	"$host:$port";
}

sub unpack_ipv6 ($) {
	my ($addr) = @_;
	my ($port, $host);

	# Socket.pm in Perl 5.14+ supports IPv6:
	eval {
		($port, $host) = Socket::unpack_sockaddr_in6($addr);
		$host = Socket::inet_ntop(Socket::AF_INET6(), $host);
	};

	if ($@) {
		# Perl 5.12 or earlier?  SpamAssassin and Net::Server use
		# Socket6, so it may be installed on our system, already
		# (otherwise die here):
		require Socket6;

		($port, $host) = Socket6::unpack_sockaddr_in6($addr);
		$host = Socket6::inet_ntop(Socket6::AF_INET6(), $host);
	}
	($host, $port);
}

sub host_with_port ($) {
	my ($addr) = @_;
	my ($port, $host);

	# this eval will die on Unix sockets:
	eval {
		if (length($addr) >= 28) {
			($host, $port) = unpack_ipv6($addr);
			$host = "[$host]";
		} else {
			($port, $host) = Socket::sockaddr_in($addr);
			$host = Socket::inet_ntoa($host);
		}
	};
	$@ ? ('127.0.0.1', 0) : ($host, $port);
}

sub inherit ($) {
	my ($listener_names) = @_;
	return () if ($ENV{LISTEN_PID} || 0) != $$;
	my $fds = $ENV{LISTEN_FDS} or return ();
	my $end = $fds + 2; # LISTEN_FDS_START - 1
	my @rv = ();
	foreach my $fd (3..$end) {
		my $s = IO::Handle->new_from_fd($fd, 'r');
		if (my $k = sockname($s)) {
			if ($s->blocking) {
				$s->blocking(0);
				warn <<"";
Inherited socket (fd=$fd) is blocking, making it non-blocking.
Set 'NonBlocking = true' in the systemd.service unit to avoid stalled
processes when multiple service instances start.

			}
			$listener_names->{$k} = $s;
			push @rv, $s;
		} else {
			warn "failed to inherit fd=$fd (LISTEN_FDS=$fds)";
		}
	}
	@rv
}

sub upgrade { # $_[0] = signal name or number (unused)
	if ($reexec_pid) {
		warn "upgrade in-progress: $reexec_pid\n";
		return;
	}
	if (defined $pid_file) {
		if ($pid_file =~ /\.oldbin\z/) {
			warn "BUG: .oldbin suffix exists: $pid_file\n";
			return;
		}
		unlink_pid_file_safe_ish($$, $pid_file);
		$pid_file .= '.oldbin';
		write_pid($pid_file);
	}
	my $pid = fork;
	unless (defined $pid) {
		warn "fork failed: $!\n";
		return;
	}
	if ($pid == 0) {
		use Fcntl qw(FD_CLOEXEC F_SETFD F_GETFD);
		$ENV{LISTEN_FDS} = scalar @listeners;
		$ENV{LISTEN_PID} = $$;
		foreach my $s (@listeners) {
			# @listeners are globs with workers, PI::L w/o workers
			$s = $s->{sock} if ref($s) eq 'PublicInbox::Listener';

			my $fl = fcntl($s, F_GETFD, 0);
			fcntl($s, F_SETFD, $fl &= ~FD_CLOEXEC);
		}
		exec @CMD;
		die "Failed to exec: $!\n";
	}
	$reexec_pid = $pid;
}

sub kill_workers ($) {
	my ($s) = @_;

	while (my ($pid, $id) = each %pids) {
		kill $s, $pid;
	}
}

sub upgrade_aborted ($) {
	my ($p) = @_;
	warn "reexec PID($p) died with: $?\n";
	$reexec_pid = undef;
	return unless $pid_file;

	my $file = $pid_file;
	$file =~ s/\.oldbin\z// or die "BUG: no '.oldbin' suffix in $file";
	unlink_pid_file_safe_ish($$, $pid_file);
	$pid_file = $file;
	eval { write_pid($pid_file) };
	warn $@, "\n" if $@;
}

sub reap_children { # $_[0] = 'CHLD' or POSIX::SIGCHLD()
	while (1) {
		my $p = waitpid(-1, WNOHANG) or return;
		if (defined $reexec_pid && $p == $reexec_pid) {
			upgrade_aborted($p);
		} elsif (defined(my $id = delete $pids{$p})) {
			warn "worker[$id] PID($p) died with: $?\n";
		} elsif ($p > 0) {
			warn "unknown PID($p) reaped: $?\n";
		} else {
			return;
		}
	}
}

sub unlink_pid_file_safe_ish ($$) {
	my ($unlink_pid, $file) = @_;
	return unless defined $unlink_pid && $unlink_pid == $$;

	open my $fh, '<', $file or return;
	local $/ = "\n";
	defined(my $read_pid = <$fh>) or return;
	chomp $read_pid;
	if ($read_pid == $unlink_pid) {
		Net::Server::Daemonize::unlink_pid_file($file);
	}
}

sub master_quit ($) {
	exit unless @listeners;
	@listeners = ();
	kill_workers($_[0]);
}

sub master_loop {
	pipe(my ($p0, $p1)) or die "failed to create parent-pipe: $!";
	my $set_workers = $worker_processes;
	reopen_logs();
	my $ignore_winch;
	my $sig = {
		USR1 => sub { reopen_logs(); kill_workers($_[0]); },
		USR2 => \&upgrade,
		QUIT => \&master_quit,
		INT => \&master_quit,
		TERM => \&master_quit,
		WINCH => sub {
			return if $ignore_winch || !@listeners;
			if (-t STDIN || -t STDOUT || -t STDERR) {
				$ignore_winch = 1;
				warn <<EOF;
ignoring SIGWINCH since we are not daemonized
EOF
			} else {
				$worker_processes = 0;
			}
		},
		HUP => sub {
			return unless @listeners;
			$worker_processes = $set_workers;
			kill_workers($_[0]);
		},
		TTIN => sub {
			return unless @listeners;
			if ($set_workers > $worker_processes) {
				++$worker_processes;
			} else {
				$worker_processes = ++$set_workers;
			}
		},
		TTOU => sub {
			$worker_processes = --$set_workers if $set_workers > 0;
		},
		CHLD => \&reap_children,
	};
	my $sigfd = PublicInbox::Sigfd->new($sig, 0);
	local %SIG = (%SIG, %$sig) if !$sigfd;
	PublicInbox::Sigfd::sig_setmask($oldset) if !$sigfd;
	while (1) { # main loop
		my $n = scalar keys %pids;
		unless (@listeners) {
			exit if $n == 0;
			$set_workers = $worker_processes = $n = 0;
		}

		if ($n > $worker_processes) {
			while (my ($k, $v) = each %pids) {
				kill('TERM', $k) if $v >= $worker_processes;
			}
			$n = $worker_processes;
		}
		my $want = $worker_processes - 1;
		if ($n <= $want) {
			PublicInbox::Sigfd::block_signals() if !$sigfd;
			for my $i ($n..$want) {
				my $pid = fork;
				if (!defined $pid) {
					warn "failed to fork worker[$i]: $!\n";
				} elsif ($pid == 0) {
					$set_user->() if $set_user;
					return $p0; # run normal work code
				} else {
					warn "PID=$pid is worker[$i]\n";
					$pids{$pid} = $i;
				}
			}
			PublicInbox::Sigfd::sig_setmask($oldset) if !$sigfd;
		}

		if ($sigfd) { # Linux and IO::KQueue users:
			$sigfd->wait_once;
		} else { # wake up every second
			sleep(1);
		}
	}
	exit # never gets here, just for documentation
}

sub tls_start_cb ($$) {
	my ($opt, $orig_post_accept) = @_;
	sub {
		my ($io, $addr, $srv) = @_;
		my $ssl = IO::Socket::SSL->start_SSL($io, %$opt);
		$orig_post_accept->($ssl, $addr, $srv);
	}
}

sub defer_accept ($$) {
	my ($s, $af_name) = @_;
	return unless defined $af_name;
	if ($^O eq 'linux') {
		my $TCP_DEFER_ACCEPT = 9; # Socket::TCP_DEFER_ACCEPT is in 5.14+
		my $x = getsockopt($s, IPPROTO_TCP, $TCP_DEFER_ACCEPT);
		return unless defined $x; # may be Unix socket
		my $sec = unpack('i', $x);
		return if $sec > 0; # systemd users may set a higher value
		setsockopt($s, IPPROTO_TCP, $TCP_DEFER_ACCEPT, 1);
	} elsif ($^O eq 'freebsd') {
		my $x = getsockopt($s, SOL_SOCKET, SO_ACCEPTFILTER);
		return if defined $x; # don't change if set
		my $accf_arg = pack('a16a240', $af_name, '');
		setsockopt($s, SOL_SOCKET, SO_ACCEPTFILTER, $accf_arg);
	}
}

sub daemon_loop ($$$$) {
	my ($refresh, $post_accept, $tlsd, $af_default) = @_;
	my %post_accept;
	while (my ($k, $v) = each %tls_opt) {
		if ($k =~ s!\A(?:https|imaps|nntps)://!!) {
			$post_accept{$k} = tls_start_cb($v, $post_accept);
		} elsif ($tlsd) { # STARTTLS, $k eq '' is OK
			$tlsd->{accept_tls} = $v;
		}
	}
	my $sig = {
		HUP => $refresh,
		INT => \&worker_quit,
		QUIT => \&worker_quit,
		TERM => \&worker_quit,
		TTIN => 'IGNORE',
		TTOU => 'IGNORE',
		USR1 => \&reopen_logs,
		USR2 => 'IGNORE',
		WINCH => 'IGNORE',
		CHLD => \&PublicInbox::DS::enqueue_reap,
	};
	if ($worker_processes > 0) {
		$refresh->(); # preload by default
		my $fh = master_loop(); # returns if in child process
		PublicInbox::EOFpipe->new($fh, \&worker_quit, undef);
	} else {
		reopen_logs();
		$set_user->() if $set_user;
		$sig->{USR2} = sub { worker_quit() if upgrade() };
		$refresh->();
	}
	$uid = $gid = undef;
	reopen_logs();
	@listeners = map {;
		my $tls_cb = $post_accept{sockname($_)};

		# NNTPS, HTTPS, HTTP, IMAPS and POP3S are client-first traffic
		# IMAP, NNTP and POP3 are server-first
		defer_accept($_, $tls_cb ? 'dataready' : $af_default);

		# this calls epoll_create:
		PublicInbox::Listener->new($_, $tls_cb || $post_accept)
	} @listeners;
	my $sigfd = PublicInbox::Sigfd->new($sig, $SFD_NONBLOCK);
	local %SIG = (%SIG, %$sig) if !$sigfd;
	if (!$sigfd) {
		# wake up every second to accept signals if we don't
		# have signalfd or IO::KQueue:
		PublicInbox::Sigfd::sig_setmask($oldset);
		PublicInbox::DS->SetLoopTimeout(1000);
	}
	PublicInbox::DS->EventLoop;
}

sub run ($$$;$) {
	my ($default, $refresh, $post_accept, $tlsd) = @_;
	local $SIG{PIPE} = 'IGNORE';
	daemon_prepare($default);
	my $af_default = $default =~ /:8080\z/ ? 'httpready' : undef;
	my $for_destroy = daemonize();
	daemon_loop($refresh, $post_accept, $tlsd, $af_default);
	PublicInbox::DS->Reset;
	# ->DESTROY runs when $for_destroy goes out-of-scope
}

sub do_chown ($) {
	my ($path) = @_;
	if (defined $uid and !chown($uid, $gid, $path)) {
		warn "could not chown $path: $!\n";
	}
}

sub write_pid ($) {
	my ($path) = @_;
	Net::Server::Daemonize::create_pid_file($path);
	do_chown($path);
}

sub DESTROY {
	unlink_pid_file_safe_ish($_[0]->{pid}, ${$_[0]->{pid_file}});
}

1;
