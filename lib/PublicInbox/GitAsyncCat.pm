# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# internal class used by PublicInbox::Git + PublicInbox::DS
# This parses the output pipe of "git cat-file --batch"
package PublicInbox::GitAsyncCat;
use v5.12;
use parent qw(PublicInbox::DS Exporter);
use PublicInbox::DS qw(awaitpid);
use POSIX qw(WNOHANG);
use PublicInbox::Syscall qw(EPOLLIN EPOLLET);
our @EXPORT = qw(ibx_async_cat ibx_async_prefetch async_check);
use PublicInbox::Git ();

our $GCF2C; # singleton PublicInbox::Gcf2Client

# close w/o aborting another git process
sub vanish {
	delete $_[0]->{git};
	$_[0]->close;
}

sub close {
	my ($self) = @_;
	if (my $git = delete $self->{git}) {
		$git->async_abort;
	}
	$self->SUPER::close; # PublicInbox::DS::close
}

sub aclose {
	my (undef, $self, $f) = @_; # ignore PID ($_[0])
	if (my $g = $self->{git}) {
		return vanish($self) if ($g->{$f} // 0) != ($self->{sock} // 1);
	}
	$self->close;
}

sub event_step {
	my ($self) = @_;
	my $git = $self->{git} or return;
	return vanish($self) if ($git->{in} // 0) != ($self->{sock} // 1);
	my $inflight = $git->{inflight};
	if ($inflight && @$inflight) {
		$git->cat_async_step($inflight);

		# child death?
		if (($git->{in} // 0) != ($self->{sock} // 1)) {
			vanish($self);
		} elsif (@$inflight || exists $git->{rbuf}) {
			# ok, more to do, requeue for fairness
			$self->requeue;
		}
	}
}

sub watch_cat {
	my ($git) = @_;
	$git->{async_cat} //= do {
		my $self = bless { git => $git }, __PACKAGE__;
		$git->{in}->blocking(0);
		$self->SUPER::new($git->{in}, EPOLLIN|EPOLLET);
		awaitpid($git->{pid}, \&aclose, $self, 'in');
		\undef; # this is a true ref()
	};
}

sub ibx_async_cat ($$$$) {
	my ($ibx, $oid, $cb, $arg) = @_;
	my $git = $ibx->{git} // $ibx->git;
	# {topdir} means ExtSearch (likely [extindex "all"]) with potentially
	# 100K alternates.  git(1) has a proposed patch for 100K alternates:
	# <https://lore.kernel.org/git/20210624005806.12079-1-e@80x24.org/>
	if (!defined($ibx->{topdir}) && !defined($git->{-tmp}) &&
		($GCF2C //= eval {
		require PublicInbox::Gcf2Client;
		PublicInbox::Gcf2Client::new();
	} // 0)) { # 0: do not retry if libgit2 or Inline::C are missing
		$GCF2C->gcf2_async(\"$oid $git->{git_dir}\n", $cb, $arg);
		\undef;
	} else { # read-only end of git-cat-file pipe
		$git->cat_async($oid, $cb, $arg);
		watch_cat($git);
	}
}

sub async_check ($$$$) {
	my ($ibx, $oidish, $cb, $arg) = @_; # $ibx may be $ctx
	my $git = $ibx->{git} // $ibx->git;
	$git->check_async($oidish, $cb, $arg);
	return watch_cat($git) if $git->{-bc}; # --batch-command
	$git->{async_chk} //= do {
		my $self = bless { git => $git }, 'PublicInbox::GitAsyncCheck';
		$git->{in_c}->blocking(0);
		$self->SUPER::new($git->{in_c}, EPOLLIN|EPOLLET);
		awaitpid($git->{pid_c}, \&aclose, $self, 'in_c');
		\undef; # this is a true ref()
	};
}

# this is safe to call inside $cb, but not guaranteed to enqueue
# returns true if successful, undef if not.  For fairness, we only
# prefetch if there's no in-flight requests.
sub ibx_async_prefetch {
	my ($ibx, $oid, $cb, $arg) = @_;
	my $git = $ibx->git;
	if (!defined($ibx->{topdir}) && $GCF2C) {
		if (!@{$GCF2C->{inflight} // []}) {
			$oid .= " $git->{git_dir}\n";
			return $GCF2C->gcf2_async(\$oid, $cb, $arg); # true
		}
	} elsif ($git->{async_cat}) {
		return $git->async_prefetch($oid, $cb, $arg);
	}
	undef;
}

1;
package PublicInbox::GitAsyncCheck;
use v5.12;
our @ISA = qw(PublicInbox::GitAsyncCat);
use POSIX qw(WNOHANG);
use PublicInbox::Syscall qw(EPOLLIN EPOLLET);

sub event_step {
	my ($self) = @_;
	my $git = $self->{git} or return;
	return $self->vanish if ($git->{in_c} // 0) != ($self->{sock} // 1);
	my $inflight = $git->{inflight_c};
	if ($inflight && @$inflight) {
		$git->check_async_step($inflight);

		# child death?
		if (($git->{in_c} // 0) != ($self->{sock} // 1)) {
			$self->vanish;
		} elsif (@$inflight || exists $git->{rbuf_c}) {
			# ok, more to do, requeue for fairness
			$self->requeue;
		}
	}
}

1;
