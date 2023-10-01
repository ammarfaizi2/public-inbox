# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# forwards stderr from lei/store process to any lei clients using
# the same store, falls back to syslog if no matching clients exist.
package PublicInbox::LeiStoreErr;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN);
use Sys::Syslog qw(openlog syslog closelog);
use IO::Handle (); # ->blocking
use Time::HiRes ();
use autodie qw(open);
our $err_wr;

# We don't want blocked stderr on clients to block lei/store or lei-daemon.
# We can't make stderr non-blocking since it can break MUAs or anything
# lei might spawn.  So we setup a timer to wake us up after a second if
# printing to a user's stderr hasn't completed, yet.  Unfortunately,
# EINTR alone isn't enough since Perl auto-restarts writes on signals,
# so to interrupt writes to clients with blocked stderr, we dup the
# error output to $err_wr ahead-of-time and close $err_wr in the
# SIGALRM handler to ensure `print' gets aborted:

sub abort_err_wr { close($err_wr) if $err_wr; undef $err_wr }

sub emit ($@) {
	my ($efh, @msg) = @_;
	open(local $err_wr, '>&', $efh); # fdopen(dup(fileno($efh)), "w")
	local $SIG{ALRM} = \&abort_err_wr;
	Time::HiRes::alarm(1.0, 0.1);
	my $ret = print $err_wr @msg;
	Time::HiRes::alarm(0);
	$ret;
}

sub new {
	my ($cls, $rd, $lei) = @_;
	my $self = bless { sock => $rd, store_path => $lei->store_path }, $cls;
	$rd->blocking(0);
	$self->SUPER::new($rd, EPOLLIN); # level-trigger
}

sub event_step {
	my ($self) = @_;
	my $n = sysread($self->{sock}, my $buf, 8192);
	return ($!{EAGAIN} ? 0 : $self->close) if !defined($n);
	return $self->close if !$n;
	my $printed;
	for my $lei (values %PublicInbox::DS::DescriptorMap) {
		my $cb = $lei->can('store_path') // next;
		next if $cb->($lei) ne $self->{store_path};
		emit($lei->{2} // next, $buf) and $printed = 1;
	}
	if (!$printed) {
		openlog('lei/store', 'pid,nowait,nofatal,ndelay', 'user');
		for my $l (split(/\n/, $buf)) { syslog('warning', '%s', $l) }
		closelog(); # don't share across fork
	}
}

1;
