# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Contains common daemon code for the httpd, imapd, and nntpd servers
# and designed for handling thousands of untrusted clients over slow
# and/or lossy connections.
package PublicInbox::Daemon;
use v5.12;
use autodie qw(chdir open pipe setsockopt);
use Getopt::Long qw(:config gnu_getopt no_ignore_case auto_abbrev);
use IO::Handle; # ->autoflush
use IO::Socket;
use File::Spec;
use IO::Poll qw(POLLERR POLLIN POLLHUP);
use POSIX qw(WNOHANG :signal_h F_SETFD);
use Socket qw(IPPROTO_TCP SOL_SOCKET);
STDOUT->autoflush(1);
STDERR->autoflush(1);
use PublicInbox::DS qw(now awaitpid);
use PublicInbox::Listener;
use PublicInbox::EOFpipe;
use PublicInbox::Git;
use PublicInbox::GitAsyncCat;
use PublicInbox::Eml;
use PublicInbox::Config;
use PublicInbox::OnDestroy;
use PublicInbox::Search;
use PublicInbox::XapClient;
our $SO_ACCEPTFILTER = 0x1000;
my @CMD;
my ($set_user, $oldset, $xh_workers);
my (@cfg_listen, $stdout, $stderr, $group, $user, $pid_file, $daemonize);
my ($nworker, @listeners, %WORKERS, %logs);
my %tls_opt; # scheme://sockname => args for IO::Socket::SSL::SSL_Context->new
my $reexec_pid;
my ($uid, $gid);
my ($default_cert, $default_key);
my %KNOWN_TLS = (443 => 'https', 563 => 'nntps', 993 => 'imaps', 995 =>'pop3s');
my %KNOWN_STARTTLS = (110 => 'pop3', 119 => 'nntp', 143 => 'imap');
my %SCHEME2PORT = map { $KNOWN_TLS{$_} => $_ + 0 } keys %KNOWN_TLS;
for (keys %KNOWN_STARTTLS) { $SCHEME2PORT{$KNOWN_STARTTLS{$_}} = $_ + 0 }
$SCHEME2PORT{http} = 80;

our ($parent_pipe, %POST_ACCEPT, %XNETD);
our %WORKER_SIG = (
	INT => \&worker_quit,
	QUIT => \&worker_quit,
	TERM => \&worker_quit,
	TTIN => 'IGNORE',
	TTOU => 'IGNORE',
	USR1 => \&reopen_logs,
	USR2 => 'IGNORE',
	WINCH => 'IGNORE',
	CHLD => \&PublicInbox::DS::enqueue_reap,
);

sub listener_opt ($) {
	my ($str) = @_; # opt1=val1,opt2=val2 (opt may repeat for multi-value)
	my $o = {};
	# allow ',' as delimiter since '&' is shell-unfriendly
	for (split(/[,&]/, $str)) {
		my ($k, $v) = split(/=/, $_, 2);
		push @{$o->{$k}}, $v;
	}

	# key may be a part of cert.  At least
	# p5-io-socket-ssl/example/ssl_server.pl has this fallback:
	$o->{cert} //= [ $default_cert ] if defined($default_cert);
	$o->{key} //= defined($default_key) ? [ $default_key ] : $o->{cert};
	$o;
}

sub check_absolute ($$) {
	my ($var, $val) = @_;
	die <<EOM if index($val // '/', '/') != 0;
$var must be an absolute path when using --daemonize: $val
EOM
}

sub accept_tls_opt ($) {
	my ($opt) = @_;
	my $o = ref($opt) eq 'HASH' ? $opt : listener_opt($opt);
	return if !defined($o->{cert});
	require PublicInbox::TLS;
	my @ctx_opt;
	# parse out hostname:/path/to/ mappings:
	for my $k (qw(cert key)) {
		$o->{$k} // next;
		push(@ctx_opt, "SSL_${k}_file", {});
		foreach my $path (@{$o->{$k}}) {
			my $host = '';
			$path =~ s/\A([^:]+):// and $host = $1;
			$ctx_opt[-1]->{$host} = $path;
			check_absolute($k, $path) if $daemonize;
		}
	}
	\@ctx_opt;
}

sub do_chown ($) {
	$uid // return;
	my ($path) = @_;
	chown($uid, $gid, $path) or warn "chown $path: $!\n";
}

sub open_log_path ($$) { # my ($fh, $path) = @_; # $_[0] is modified
	open $_[0], '>>', $_[1];
	$_[0]->autoflush(1);
	do_chown($_[1]);
	$_[0];
}

sub load_mod ($;$$) {
	my ($scheme, $opt, $addr) = @_;
	my $modc = "PublicInbox::\U$scheme";
	$modc =~ s/S\z//;
	my $mod = $modc.'D';
	eval "require $mod"; # IMAPD|HTTPD|NNTPD|POP3D
	die $@ if $@;
	my %xn;
	my $tlsd = $xn{tlsd} = $mod->new;
	my %env = map {
		substr($_, length('env.')) => $opt->{$_}->[-1];
	} grep(/\Aenv\./, keys %$opt);
	$xn{refresh} = sub {
		my ($sig) = @_;
		local @ENV{keys %env} = values %env;
		$tlsd->refresh_groups($sig);
	};
	$xn{post_accept} = $tlsd->can('post_accept_cb') ?
			$tlsd->post_accept_cb : sub { $modc->new($_[0], $tlsd) };
	my @paths = qw(out err);
	if ($modc eq 'PublicInbox::HTTP') {
		@paths = qw(err);
		$xn{af_default} = 'httpready';
		if (my $p = $opt->{psgi}) {
			die "multiple psgi= options specified\n" if @$p > 1;
			check_absolute('psgi=', $p->[0]) if $daemonize;
			$tlsd->{psgi} = $p->[0];
			warn "# $scheme://$addr psgi=$p->[0]\n";
		}
	}
	for my $f (@paths) {
		my $p = $opt->{$f} or next;
		die "multiple $f= options specified\n" if @$p > 1;
		check_absolute("$f=", $p->[0]) if $daemonize;
		$p = File::Spec->canonpath($p->[0]);
		$tlsd->{$f} = $logs{$p} //= open_log_path(my $fh, $p);
		warn "# $scheme://$addr $f=$p\n";
	}
	# for per-listener $SIG{__WARN__}:
	my $err = $tlsd->{err};
	$tlsd->{warn_cb} = sub {
		print $err @_ unless PublicInbox::Eml::warn_ignore(@_)
	};
	$opt->{'multi-accept'} and
		$xn{'multi-accept'} = $opt->{'multi-accept'}->[-1];
	\%xn;
}

sub daemon_prepare ($) {
	my ($default_listen) = @_;
	my $listener_names = {}; # sockname => IO::Handle
	$oldset = PublicInbox::DS::block_signals();
	@CMD = ($0, @ARGV);
	my ($prog) = ($CMD[0] =~ m!([^/]+)\z!g);
	my $dh = defined($default_listen) ? " (default: $default_listen)" : '';
	my $help = <<EOF;
usage: $prog [-l ADDRESS] [--cert=FILE] [--key=FILE]

options:

  -l ADDRESS    address to listen on$dh
  --cert=FILE   default SSL/TLS certificate
  --key=FILE    default SSL/TLS certificate key
  -W WORKERS    number of worker processes to spawn (default: 1)
  -X XWORKERS   number of Xapian helper processes (default: undefined)

See public-inbox-daemon(8) and $prog(1) man pages for more.
EOF
	my %opt = (
		'l|listen=s' => \@cfg_listen,
		'1|stdout=s' => \$stdout,
		'2|stderr=s' => \$stderr,
		'W|worker-processes=i' => \$nworker,
		'P|pid-file=s' => \$pid_file,
		'u|user=s' => \$user,
		'g|group=s' => \$group,
		'D|daemonize' => \$daemonize,
		'multi-accept=i' => \$PublicInbox::Listener::MULTI_ACCEPT,
		'cert=s' => \$default_cert,
		'key=s' => \$default_key,
		'X|xapian-helpers=i' => \$xh_workers,
		'help|h' => \(my $show_help),
	);
	GetOptions(%opt) or die $help;
	if ($show_help) { print $help; exit 0 };

	$_ = File::Spec->canonpath($_ // next) for ($stdout, $stderr);
	if (defined $pid_file && $pid_file =~ /\.oldbin\z/) {
		die "--pid-file cannot end with '.oldbin'\n";
	}
	@listeners = inherit($listener_names);
	my @inherited_names = keys(%$listener_names);

	# ignore daemonize when inheriting
	$daemonize = undef if scalar @listeners;

	unless (@listeners || @cfg_listen) {
		$default_listen // die "no listeners specified\n";
		push @cfg_listen, $default_listen
	}
	my ($default_scheme) = (($default_listen // '') =~ m!\A([^:]+)://!);
	foreach my $l (@cfg_listen) {
		my $orig = $l;
		my ($scheme, $port, $opt);
		$l =~ s!\A([a-z0-9]+)://!! and $scheme = $1;
		$scheme //= $default_scheme;
		if ($l =~ /\A(?:\[[^\]]+\]|[^:]+):([0-9]+)/) {
			$port = $1 + 0;
			$scheme //= $KNOWN_TLS{$port} // $KNOWN_STARTTLS{$port};
		}
		$scheme // die "unable to determine URL scheme of $orig\n";
		if (!defined($port) && index($l, '/') != 0) { # AF_UNIX socket
			$port = $SCHEME2PORT{$scheme} //
				die "no port in listen=$orig\n";
			$l =~ s!\A([^/]+)!$1:$port! or
				die "unable to add port=$port to $l\n";
		}
		$l =~ s!/\z!!; # chop one trailing slash
		if ($l =~ s!/?\?(.+)\z!!) {
			$opt = listener_opt($1);
			$tls_opt{"$scheme://$l"} = accept_tls_opt($opt);
		} elsif (defined($default_cert)) {
			$tls_opt{"$scheme://$l"} = accept_tls_opt('');
		} elsif ($scheme =~ /\A(?:https|imaps|nntps|pop3s)\z/) {
			die "$orig specified w/o cert=\n";
		}
		if ($listener_names->{$l}) { # already inherited
			$XNETD{$l} = load_mod($scheme, $opt, $l);
			next;
		}
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
		$o{Listen} = 2**31 - 1; # kernel will clamp
		my $prev = umask 0000;
		my $s = eval { $sock_pkg->new(%o) } or
			warn "error binding $l: $! ($@)\n";
		umask $prev;
		$s // next;
		$s->blocking(0);
		my $sockname = sockname($s);
		warn "# bound $scheme://$sockname\n";
		$XNETD{$sockname} //= load_mod($scheme, $opt);
		$listener_names->{$sockname} = $s;
		push @listeners, $s;
	}

	# cert/key options in @cfg_listen takes precedence when inheriting,
	# but map well-known inherited ports if --listen isn't specified
	# at all.  This allows socket-activation users to set certs once
	# and not have to configure each socket:
	if (defined $default_cert) {
		my ($stls) = (($default_scheme // '') =~ /\A(pop3|nntp|imap)/);
		for my $x (@inherited_names) {
			$x =~ /:([0-9]+)\z/ or next; # no TLS for AF_UNIX
			if (my $scheme = $KNOWN_TLS{$1}) {
				$XNETD{$x} //= load_mod($scheme);
				$tls_opt{"$scheme://$x"} ||= accept_tls_opt('');
			} elsif (($scheme = $KNOWN_STARTTLS{$1})) {
				$XNETD{$x} //= load_mod($scheme);
				$tls_opt{"$scheme://$x"} ||= accept_tls_opt('');
			} elsif (defined $stls) {
				$tls_opt{"$stls://$x"} ||= accept_tls_opt('');
			}
		}
	}
	if (defined $default_scheme) {
		for my $x (@inherited_names) {
			$XNETD{$x} //= load_mod($default_scheme);
		}
	}
	die "No listeners bound\n" unless @listeners;
}

sub daemonize () {
	if ($daemonize) {
		require Cwd;
		foreach my $i (0..$#ARGV) {
			my $arg = $ARGV[$i];
			next unless -e $arg;
			$ARGV[$i] = Cwd::abs_path($arg);
		}
		check_absolute('--stdout', $stdout);
		check_absolute('--stderr', $stderr);
		check_absolute('--pid-file', $pid_file);
		check_absolute('--cert', $default_cert);
		check_absolute('--key', $default_key);
		chdir '/';
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
		my $pid = PublicInbox::OnDestroy::fork_tmp;
		exit if $pid;
		open STDIN, '+<', '/dev/null';
		open STDOUT, '>&STDIN';
		open STDERR, '>&STDIN';
		POSIX::setsid();
		$pid = PublicInbox::OnDestroy::fork_tmp;
		exit if $pid;
	}
	return unless defined $pid_file;

	write_pid($pid_file);
	on_destroy \&unlink_pid_file_safe_ish, \$pid_file;
}

sub has_busy_clients { # post_loop_do CB
	my ($state) = @_;
	my $now = now();
	my $n = PublicInbox::DS::close_non_busy();
	if ($n) {
		if ($state->{-w} < now()) {
			warn "$$ quitting, $n client(s) left\n";
			$state->{-w} = now() + 5;
		}
		unless (defined $state->{0}) {
			$state->{0} = (split(/\s+/, $0))[0];
			$state->{0} =~ s!\A.*?([^/]+)\z!$1!;
		}
		$0 = "$state->{0} quitting, $n client(s) left";
	}
	$n; # true: loop continues, false: loop breaks
}

sub worker_quit { # $_[0] = signal name or number (unused)
	# killing again terminates immediately:
	exit unless @listeners;

	$_->close foreach @listeners; # call PublicInbox::DS::close
	@listeners = ();

	# drop idle connections and try to quit gracefully
	@PublicInbox::DS::post_loop_do = (\&has_busy_clients, { -w => 0 })
}

sub spawn_xh () {
	$xh_workers // return;
	local $) = $gid if defined $gid;
	local $( = $gid if defined $gid;
	local $> = $uid if defined $uid;
	local $< = $uid if defined $uid;
	my $xhc = $PublicInbox::Search::XHC = eval {
		local $ENV{STDERR_PATH} = $stderr;
		local $ENV{STDOUT_PATH} = $stdout;
		PublicInbox::XapClient::start_helper('-j', $xh_workers)
	};
	if ($@) {
		warn "E: $@ (will attempt to continue w/o xapian-helpers)\n";
	} elsif ($xhc) {
		require PublicInbox::XhcMset;
		$xhc->{io}->blocking(0);
		awaitpid($xhc->{io}->attached_pid, \&respawn_xh);
	}
}

sub reopen_logs {
	my ($sig) = @_;
	$logs{$stdout} //= \*STDOUT if defined $stdout;
	$logs{$stderr} //= \*STDERR if defined $stderr;
	while (my ($p, $fh) = each %logs) { open_log_path($fh, $p) }
	($sig && defined($xh_workers) && $PublicInbox::Search::XHC) and
		kill('USR1', $PublicInbox::Search::XHC->{io}->attached_pid);
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
		CORE::open(my $s, '<&=', $fd) or warn "fdopen fd=$fd: $!";
		if (my $k = sockname($s)) {
			my $prev_was_blocking = $s->blocking(0);
			warn <<"" if $prev_was_blocking;
Inherited socket ($k fd=$fd) is blocking, making it non-blocking.
Set 'NonBlocking = true' in the systemd.service unit to avoid stalled
processes when multiple service instances start.

			$listener_names->{$k} = $s;
			warn "# inherited $k fd=$fd\n";
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
		unlink_pid_file_safe_ish(\$pid_file);
		$pid_file .= '.oldbin';
		write_pid($pid_file);
	}
	my $pid = eval { PublicInbox::OnDestroy::fork_tmp };
	if (!defined($pid)) {
		warn "fork failed: $! $@\n";
	} elsif ($pid == 0) {
		$ENV{LISTEN_FDS} = scalar @listeners;
		$ENV{LISTEN_PID} = $$;
		foreach my $s (@listeners) {
			# @listeners are globs with workers, PI::L w/o workers
			$s = $s->{sock} if ref($s) eq 'PublicInbox::Listener';
			fcntl($s, F_SETFD, 0) // die "F_SETFD: $!";
		}
		exec @CMD;
		die "Failed to exec: $!\n";
	} else {
		awaitpid($pid, \&upgrade_aborted);
		$reexec_pid = $pid;
	}
}

sub kill_workers ($) { kill $_[0], values(%WORKERS) }

sub upgrade_aborted {
	my ($pid) = @_;
	warn "reexec PID($pid) died with: $?\n";
	$reexec_pid = undef;
	return unless $pid_file;

	my $file = $pid_file;
	$file =~ s/\.oldbin\z// or die "BUG: no '.oldbin' suffix in $file";
	unlink_pid_file_safe_ish(\$pid_file);
	$pid_file = $file;
	eval { write_pid($pid_file) };
	warn $@, "\n" if $@;
}

sub unlink_pid_file_safe_ish ($) {
	my ($fref) = @_;

	CORE::open my $fh, '<', $$fref or return;
	local $/ = "\n";
	defined(my $read_pid = <$fh>) or return;
	chomp $read_pid;
	Net::Server::Daemonize::unlink_pid_file($$fref) if $read_pid == $$;
}

sub master_quit ($) {
	exit unless @listeners;
	@listeners = ();
	exit unless kill_workers($_[0]);
}

sub reap_worker { # awaitpid CB
	my ($pid, $nr) = @_;
	warn "worker[$nr] died \$?=$?\n" if $?;
	delete $WORKERS{$nr};
	exit if !@listeners && !keys(%WORKERS);
	PublicInbox::DS::requeue(\&start_workers);
}

sub start_worker ($) {
	my ($nr) = @_;
	return unless @listeners;
	my $pid = PublicInbox::DS::fork_persist;
	if ($pid == 0) {
		undef %WORKERS;
		undef $xh_workers;
		local $PublicInbox::DS::Poller; # allow epoll/kqueue
		$set_user->() if $set_user;
		PublicInbox::EOFpipe->new($parent_pipe, \&worker_quit);
		worker_loop();
		exit 0;
	} else {
		$WORKERS{$nr} = $pid;
		awaitpid($pid, \&reap_worker, $nr);
	}
}

sub start_workers {
	my @idx = grep { !defined($WORKERS{$_}) } (0..($nworker - 1)) or return;
	eval { start_worker($_) for @idx };
	warn "E: $@\n" if $@;
}

sub trim_workers {
	my @nr = grep { $_ >= $nworker } keys %WORKERS;
	kill('TERM', @WORKERS{@nr});
}

sub master_loop {
	local $parent_pipe;
	pipe $parent_pipe, my $p1;
	my $set_workers = $nworker; # for SIGWINCH
	reopen_logs();
	spawn_xh;
	my $msig = {
		USR1 => sub { reopen_logs($_[0]); kill_workers($_[0]); },
		USR2 => \&upgrade,
		QUIT => \&master_quit,
		INT => \&master_quit,
		TERM => \&master_quit,
		WINCH => sub {
			$nworker = 0;
			trim_workers();
		},
		HUP => sub {
			$nworker = $set_workers; # undo WINCH
			kill_workers($_[0]);
			PublicInbox::DS::requeue(\&start_workers)
		},
		TTIN => sub {
			if ($set_workers > $nworker) {
				++$nworker;
			} else {
				$nworker = ++$set_workers;
			}
			PublicInbox::DS::requeue(\&start_workers);
		},
		TTOU => sub {
			return if $nworker <= 0;
			--$nworker;
			trim_workers();
		},
		CHLD => \&PublicInbox::DS::enqueue_reap,
	};
	$msig->{WINCH} = sub {
		warn "ignoring SIGWINCH since we are not daemonized\n";
	} if -t STDIN || -t STDOUT || -t STDERR;
	start_workers();
	PublicInbox::DS::event_loop($msig, $oldset);
	exit # never gets here, just for documentation
}

sub tls_cb {
	my ($post_accept, $tlsd) = @_;
	sub {
		my ($io, $addr, $srv) = @_;
		$post_accept->(PublicInbox::TLS::start($io, $tlsd), $addr, $srv)
	}
}

sub defer_accept ($$) {
	my ($s, $af_name) = @_;
	$af_name // return;
	eval {
		if ($^O eq 'linux') {
			# Socket::TCP_DEFER_ACCEPT is only in 5.14+
			my $TCP_DEFER_ACCEPT = 9;
			my $x = getsockopt($s, IPPROTO_TCP, $TCP_DEFER_ACCEPT)
				// return; # may be Unix socket
			my $sec = unpack('i', $x);
			return if $sec > 0; # systemd users may this higher
			setsockopt $s, IPPROTO_TCP, $TCP_DEFER_ACCEPT, 1;
		} elsif ($^O =~ /\A(?:freebsd|netbsd|dragonfly)\z/) {
			# getsockopt can EINVAL if SO_ACCEPTFILTER is unset:
			my $x = getsockopt($s, SOL_SOCKET, $SO_ACCEPTFILTER);
			return if ($x // '') =~ /[^\0]/s; # don't change if set
			CORE::setsockopt $s, SOL_SOCKET, $SO_ACCEPTFILTER,
					pack('a16a240', $af_name, '') or
				warn 'W: ', sockname($s), <<EOM;
 SO_ACCEPTFILTER $af_name: $! (try `kldload $af_name')
EOM
		}
	};
	my $err = $@; # sockname() clobbers $@
	warn 'W: ', sockname($s), ' ', $err, "\n" if $err;
}

sub daemon_loop () {
	local $PublicInbox::Config::DEDUPE = {}; # enable dedupe cache
	my $refresh = $WORKER_SIG{HUP} = sub {
		my ($sig) = @_;
		%$PublicInbox::Config::DEDUPE = (); # clear cache
		for my $xn (values %XNETD) {
			delete $xn->{tlsd}->{ssl_ctx}; # PublicInbox::TLS::start
			eval { $xn->{refresh}->($sig) };
			warn "refresh $@\n" if $@;
		}
	};
	while (my ($k, $ctx_opt) = each %tls_opt) {
		$ctx_opt // next;
		my ($scheme, $l) = split(m!://!, $k, 2);
		my $xn = $XNETD{$l} // die "BUG: no xnetd for $k";
		$xn->{tlsd}->{ssl_ctx_opt} //= $ctx_opt;
		$scheme =~ m!\A(?:https|imaps|nntps|pop3s)! and
			$POST_ACCEPT{$l} = tls_cb(@$xn{qw(post_accept tlsd)});
	}
	undef %tls_opt;
	if ($nworker > 0) {
		$refresh->(); # preload by default
		return master_loop();
	} else {
		reopen_logs();
		$set_user->() if $set_user;
		$WORKER_SIG{USR2} = sub { worker_quit() if upgrade() };
		$refresh->();
	}
	local $PublicInbox::DS::Poller; # allow epoll/kqueue
	worker_loop();
}

sub worker_loop {
	$uid = $gid = undef;
	reopen_logs();
	spawn_xh; # only for -W0
	@listeners = map {;
		my $l = sockname($_);
		my $tls_cb = $POST_ACCEPT{$l};
		my $xn = $XNETD{$l} // die "BUG: no xnetd for $l";

		# NNTPS, HTTPS, HTTP, IMAPS and POP3S are client-first traffic
		# IMAP, NNTP and POP3 are server-first
		defer_accept($_, $tls_cb ? 'dataready' : $xn->{af_default});

		# this calls epoll_create:
		PublicInbox::Listener->new($_, $tls_cb || $xn->{post_accept},
						$xn->{'multi-accept'})
	} @listeners;
	PublicInbox::DS::event_loop(\%WORKER_SIG, $oldset);
}

sub respawn_xh { # awaitpid cb
	my ($pid) = @_;
	return unless @listeners;
	warn "W: xap_helper PID:$pid died: \$?=$?, respawning...\n";
	spawn_xh;
}

sub run {
	my ($default_listen) = @_;
	$nworker = 1;
	local (%XNETD, %POST_ACCEPT);
	daemon_prepare($default_listen);
	my $unlink_on_leave = daemonize();

	# localize GCF2C for tests:
	local $PublicInbox::GitAsyncCat::GCF2C;
	local $PublicInbox::Git::async_warn = 1;
	local $SIG{__WARN__} = PublicInbox::Eml::warn_ignore_cb();
	local %WORKER_SIG = %WORKER_SIG;
	local $PublicInbox::XapClient::tries = 0;
	local $PublicInbox::Search::XHC;

	daemon_loop();
	# $unlink_on_leave runs
}

sub write_pid ($) {
	my ($path) = @_;
	Net::Server::Daemonize::create_pid_file($path);
	do_chown($path);
}

sub stream_hup ($) {
	my $ev = POLLIN;
	my $n = IO::Poll::_poll(0, fileno($_[0]) // return, $ev) or return;
	return 1 if $ev & (POLLHUP|POLLERR);

	# n.b. POLLHUP isn't reliably detected, so check FIONREAD on POLLIN
	if (defined(PublicInbox::Syscall::FIONREAD) && ($ev & POLLIN)) {
		ioctl($_[0], PublicInbox::Syscall::FIONREAD, $n = "") //
			return;
		return (unpack('i', $n) == 0);
	}
	undef;
}

if (PublicInbox::Syscall->can('TCP_ESTABLISHED')) {
	eval <<'EOM';
sub tcp_hup ($) {
	my $buf = getsockopt($_[0], Socket::IPPROTO_TCP, Socket::TCP_INFO)
		or return;
	unpack('C', $buf) != PublicInbox::Syscall::TCP_ESTABLISHED
}
EOM
	warn "E: $@" if $@;
}

no warnings 'once';
*tcp_hup = \&stream_hup if !__PACKAGE__->can('tcp_hup');

1;
