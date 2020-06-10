# Copyright (C) 2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::InboxIdle;
use strict;
use base qw(PublicInbox::DS);
use fields qw(pi_config inot pathmap);
use Cwd qw(abs_path);
use Symbol qw(gensym);
use PublicInbox::Syscall qw(EPOLLIN EPOLLET);
my $IN_CLOSE = 0x08 | 0x10; # match Linux inotify
my $ino_cls;
if ($^O eq 'linux' && eval { require Linux::Inotify2; 1 }) {
	$IN_CLOSE = Linux::Inotify2::IN_CLOSE();
	$ino_cls = 'Linux::Inotify2';
} elsif (eval { require PublicInbox::KQNotify }) {
	$IN_CLOSE = PublicInbox::KQNotify::IN_CLOSE();
	$ino_cls = 'PublicInbox::KQNotify';
}
require PublicInbox::In2Tie if $ino_cls;

sub in2_arm ($$) { # PublicInbox::Config::each_inbox callback
	my ($ibx, $self) = @_;
	my $dir = abs_path($ibx->{inboxdir});
	if (!defined($dir)) {
		warn "W: $ibx->{inboxdir} not watched: $!\n";
		return;
	}
	my $inot = $self->{inot};
	my $cur = $self->{pathmap}->{$dir} //= [];

	# transfer old subscriptions to the current inbox, cancel the old watch
	if (my $old_ibx = $cur->[0]) {
		$ibx->{unlock_subs} and
			die "BUG: $dir->{unlock_subs} should not exist";
		$ibx->{unlock_subs} = $old_ibx->{unlock_subs};
		$cur->[1]->cancel;
	}
	$cur->[0] = $ibx;

	my $lock = "$dir/".($ibx->version >= 2 ? 'inbox.lock' : 'ssoma.lock');
	$cur->[1] = $inot->watch($lock, $IN_CLOSE, sub { $ibx->on_unlock });

	# TODO: detect deleted packs (and possibly other files)
}

sub refresh {
	my ($self, $pi_config) = @_;
	$pi_config->each_inbox(\&in2_arm, $self);
}

sub new {
	my ($class, $pi_config) = @_;
	my $self = fields::new($class);
	my $inot;
	if ($ino_cls) {
		$inot = $ino_cls->new or die "E: $ino_cls->new: $!";
		my $sock = gensym;
		tie *$sock, 'PublicInbox::In2Tie', $inot;
		$inot->blocking(0);
		$inot->on_overflow(undef); # broadcasts everything on overflow
		$self->SUPER::new($sock, EPOLLIN | EPOLLET);
	} else {
		require PublicInbox::FakeInotify;
		$inot = PublicInbox::FakeInotify->new;
	}
	$self->{inot} = $inot;
	$self->{pathmap} = {}; # inboxdir => [ ibx, watch1, watch2, watch3...]
	refresh($self, $pi_config);
	$self;
}

sub event_step {
	my ($self) = @_;
	eval { $self->{inot}->poll }; # Linux::Inotify2::poll
	warn "$self->{inot}->poll err: $@\n" if $@;
}

1;