# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# a tied handle for auto reaping of children tied to a pipe or socket,
# see perltie(1) for details.
package PublicInbox::ProcessIO;
use v5.12;
use PublicInbox::DS qw(awaitpid);
use Symbol qw(gensym);
use bytes qw(length);

sub maybe_new {
	my ($cls, $pid, $fh, @cb_arg) = @_;
	return ($fh, $pid) if wantarray;
	my $s = gensym;
	tie *$s, $cls, $pid, $fh, @cb_arg;
	$s;
}

sub waitcb { # awaitpid callback
	my ($pid, $err_ref, $cb, @args) = @_;
	$$err_ref = $?; # sets >{pp_chld_err} for _close
	$cb->($pid, @args) if $cb;
}

sub TIEHANDLE {
	my ($cls, $pid, $fh, @cb_arg) = @_;
	my $self = bless { pid => $pid, fh => $fh, ppid => $$ }, $cls;
	# we share $err (and not $self) with awaitpid to avoid a ref cycle
	$self->{pp_chld_err} = \(my $err);
	awaitpid($pid, \&waitcb, \$err, @cb_arg);
	$self;
}

# for IO::Uncompress::Gunzip and PublicInbox::LeiRediff
sub BINMODE { @_ == 1 ? binmode($_[0]->{fh}) : binmode($_[0]->{fh}, $_[1]) }

sub READ { read($_[0]->{fh}, $_[1], $_[2], $_[3] || 0) }

sub READLINE { readline($_[0]->{fh}) }

sub WRITE { syswrite($_[0]->{fh}, $_[1], $_[2] // length($_[1]), $_[3] // 0) }

sub PRINT { print { $_[0]->{fh} } @_[1..$#_] }

sub FILENO { fileno($_[0]->{fh}) }

sub _close ($;$) {
	my ($self, $wait) = @_;
	my ($fh, $pid) = delete(@$self{qw(fh pid)});
	my $ret = (defined($fh) && $wait) ? close($fh) : ($fh = '');
	return $ret unless defined($pid) && $self->{ppid} == $$;
	if ($wait) { # caller cares about the exit status:
		# synchronous wait via defined(wantarray) on awaitpid:
		defined(${$self->{pp_chld_err}}) or $wait = awaitpid($pid);
		($? = ${$self->{pp_chld_err}}) and $ret = '';
	} else {
		awaitpid($pid); # depends on $in_loop or not
	}
	$ret;
}

# if caller uses close(), assume they want to check $? immediately so
# we'll waitpid() synchronously.  n.b. wantarray doesn't seem to
# propagate `undef' down to tied methods, otherwise I'd rely on that.
sub CLOSE { _close($_[0], 1) }

# if relying on DESTROY, assume the caller doesn't care about $? and
# we can let the event loop call waitpid() whenever it gets SIGCHLD
sub DESTROY {
	_close($_[0]);
	undef;
}

1;
