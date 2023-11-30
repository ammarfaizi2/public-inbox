# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::GitAsyncCat;
use v5.12;
use parent qw(Exporter);
our @EXPORT = qw(ibx_async_cat ibx_async_prefetch async_check);

our $GCF2C; # singleton PublicInbox::Gcf2Client

sub ibx_async_cat ($$$$) {
	my ($ibx, $oid, $cb, $arg) = @_;
	my $isrch = $ibx->{isrch};
	my $git = $isrch ? $isrch->{es}->git : ($ibx->{git} // $ibx->git);
	# {topdir} means ExtSearch (likely [extindex "all"]) with potentially
	# 100K alternates.  git v2.33+ can handle 100k alternates fairly well.
	if (!$isrch && !defined($ibx->{topdir}) && !defined($git->{-tmp}) &&
		($GCF2C //= eval {
		require PublicInbox::Gcf2Client;
		PublicInbox::Gcf2Client::new();
	} // 0)) { # 0: do not retry if libgit2 or Inline::C are missing
		$GCF2C->gcf2_async("$oid $git->{git_dir}\n", $cb, $arg);
		\undef;
	} else { # read-only end of git-cat-file pipe
		$git->cat_async($oid, $cb, $arg);
		$git->watch_async;
	}
}

sub async_check ($$$$) {
	my ($ibx, $oidish, $cb, $arg) = @_; # $ibx may be $ctx
	my $git = $ibx->{git} // $ibx->git;
	$git->check_async($oidish, $cb, $arg);
	($git->{ck} // $git)->watch_async;
}

# this is safe to call inside $cb, but not guaranteed to enqueue
# returns true if successful, undef if not.  For fairness, we only
# prefetch if there's no in-flight requests.
sub ibx_async_prefetch {
	my ($ibx, $oid, $cb, $arg) = @_;
	my $git = $ibx->git;
	if (!defined($ibx->{topdir}) && $GCF2C) {
		if (!@{$GCF2C->gcf_inflight // []}) {
			$oid .= " $git->{git_dir}\n";
			return $GCF2C->gcf2_async($oid, $cb, $arg); # true
		}
	} elsif ($git->{epwatch}) {
		return $git->async_prefetch($oid, $cb, $arg);
	}
	undef;
}

1;
