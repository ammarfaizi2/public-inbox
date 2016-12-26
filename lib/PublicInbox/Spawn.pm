# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# This allows vfork to be used for spawning subprocesses if
# PERL_INLINE_DIRECTORY is explicitly defined in the environment.
# Under Linux, vfork can make a big difference in spawning performance
# as process size increases (fork still needs to mark pages for CoW use).
# Currently, we only use this for code intended for long running
# daemons (inside the PSGI code (-httpd) and -nntpd).  The short-lived
# scripts (-mda, -index, -learn, -init) either use IPC::run or standard
# Perl routines.

package PublicInbox::Spawn;
use strict;
use warnings;
use base qw(Exporter);
use Symbol qw(gensym);
use IO::Handle;
use PublicInbox::ProcessPipe;
our @EXPORT_OK = qw/which spawn popen_rd/;

my $vfork_spawn = <<'VFORK_SPAWN';
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <alloca.h>
#include <signal.h>
#include <assert.h>

#define AV_ALLOCA(av, max) alloca((max = (av_len((av)) + 1)) * sizeof(char *))

static void av2c_copy(char **dst, AV *src, I32 max)
{
	I32 i;

	for (i = 0; i < max; i++) {
		SV **sv = av_fetch(src, i, 0);
		dst[i] = sv ? SvPV_nolen(*sv) : 0;
	}
	dst[max] = 0;
}

static void *deconst(const char *s)
{
	union { const char *in; void *out; } u;
	u.in = s;
	return u.out;
}

/* needs to be safe inside a vfork'ed process */
static void xerr(const char *msg)
{
	struct iovec iov[3];
	const char *err = strerror(errno); /* should be safe in practice */

	iov[0].iov_base = deconst(msg);
	iov[0].iov_len = strlen(msg);
	iov[1].iov_base = deconst(err);
	iov[1].iov_len = strlen(err);
	iov[2].iov_base = deconst("\n");
	iov[2].iov_len = 1;
	writev(2, iov, 3);
	_exit(1);
}

#define REDIR(var,fd) do { \
	if (var != fd && dup2(var, fd) < 0) \
		xerr("error redirecting std"#var ": "); \
} while (0)

/*
 * unstable internal API.  This was easy to implement but does not
 * support arbitrary redirects.  It'll be updated depending on
 * whatever we'll need in the future.
 * Be sure to update PublicInbox::SpawnPP if this changes
 */
int public_inbox_fork_exec(int in, int out, int err,
			SV *file, SV *cmdref, SV *envref)
{
	AV *cmd = (AV *)SvRV(cmdref);
	AV *env = (AV *)SvRV(envref);
	const char *filename = SvPV_nolen(file);
	pid_t pid;
	char **argv, **envp;
	I32 max;
	sigset_t set, old;
	int ret, errnum;

	argv = AV_ALLOCA(cmd, max);
	av2c_copy(argv, cmd, max);

	envp = AV_ALLOCA(env, max);
	av2c_copy(envp, env, max);

	ret = sigfillset(&set);
	assert(ret == 0 && "BUG calling sigfillset");
	ret = sigprocmask(SIG_SETMASK, &set, &old);
	assert(ret == 0 && "BUG calling sigprocmask to block");
	pid = vfork();
	if (pid == 0) {
		int sig;

		REDIR(in, 0);
		REDIR(out, 1);
		REDIR(err, 2);
		for (sig = 1; sig < NSIG; sig++)
			signal(sig, SIG_DFL); /* ignore errors on signals */
		/*
		 * don't bother unblocking, we don't want signals
		 * to the group taking out a subprocess
		 */
		execve(filename, argv, envp);
		xerr("execve failed");
	}
	errnum = errno;
	ret = sigprocmask(SIG_SETMASK, &old, NULL);
	assert(ret == 0 && "BUG calling sigprocmask to restore");
	errno = errnum;

	return (int)pid;
}
VFORK_SPAWN

my $inline_dir = $ENV{PERL_INLINE_DIRECTORY};
$vfork_spawn = undef unless defined $inline_dir && -d $inline_dir && -w _;
if (defined $vfork_spawn) {
	# Inline 0.64 or later has locking in multi-process env,
	# but we support 0.5 on Debian wheezy
	use Fcntl qw(:flock);
	eval {
		my $f = "$inline_dir/.public-inbox.lock";
		open my $fh, '>', $f or die "failed to open $f: $!\n";
		flock($fh, LOCK_EX) or die "LOCK_EX failed on $f: $!\n";
		eval 'use Inline C => $vfork_spawn'; #, BUILD_NOISY => 1';
		my $err = $@;
		flock($fh, LOCK_UN) or die "LOCK_UN failed on $f: $!\n";
		die $err if $err;
	};
	if ($@) {
		warn "Inline::C failed for vfork: $@\n";
		$vfork_spawn = undef;
	}
}

unless (defined $vfork_spawn) {
	require PublicInbox::SpawnPP;
	no warnings 'once';
	*public_inbox_fork_exec = *PublicInbox::SpawnPP::public_inbox_fork_exec
}

# n.b. we never use absolute paths with this
sub which ($) {
	my ($file) = @_;
	foreach my $p (split(':', $ENV{PATH})) {
		$p .= "/$file";
		return $p if -x $p;
	}
	undef;
}

sub spawn ($;$$) {
	my ($cmd, $env, $opts) = @_;
	my $f = which($cmd->[0]);
	defined $f or die "$cmd->[0]: command not found\n";
	my @env;
	$opts ||= {};

	my %env = $opts->{-env} ? () : %ENV;
	if ($env) {
		foreach my $k (keys %$env) {
			my $v = $env->{$k};
			if (defined $v) {
				$env{$k} = $v;
			} else {
				delete $env{$k};
			}
		}
	}
	while (my ($k, $v) = each %env) {
		push @env, "$k=$v";
	}
	my $in = $opts->{0} || 0;
	my $out = $opts->{1} || 1;
	my $err = $opts->{2} || 2;
	my $pid = public_inbox_fork_exec($in, $out, $err, $f, $cmd, \@env);
	$pid < 0 ? undef : $pid;
}

sub popen_rd {
	my ($cmd, $env, $opts) = @_;
	pipe(my ($r, $w)) or die "pipe: $!\n";
	$opts ||= {};
	$opts->{1} = fileno($w);
	my $pid = spawn($cmd, $env, $opts);
	return unless defined $pid;
	return ($r, $pid) if wantarray;
	my $ret = gensym;
	tie *$ret, 'PublicInbox::ProcessPipe', $pid, $r;
	$ret;
}

1;
