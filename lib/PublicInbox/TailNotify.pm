# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# only used for tests at the moment...
package PublicInbox::TailNotify;
use v5.12;
use parent qw(PublicInbox::DirIdle); # not optimal, maybe..
use PublicInbox::DS qw(now);

my ($TAIL_MOD, $ino_cls);
if ($^O eq 'linux' && eval { require PublicInbox::Inotify; 1 }) {
	$TAIL_MOD = Linux::Inotify2::IN_MOVED_TO() |
		Linux::Inotify2::IN_CREATE() |
		Linux::Inotify2::IN_MODIFY();
	$ino_cls = 'PublicInbox::Inotify';
} elsif (eval { require PublicInbox::KQNotify }) {
	$TAIL_MOD = PublicInbox::KQNotify::MOVED_TO_OR_CREATE();
	$ino_cls = 'PublicInbox::KQNotify';
} else {
	require PublicInbox::FakeInotify;
	$TAIL_MOD = PublicInbox::FakeInotify::MOVED_TO_OR_CREATE() |
		PublicInbox::FakeInotify::IN_MODIFY();
}
require IO::Poll if $ino_cls;

sub reopen_file ($) {
	my ($self) = @_;

	open my $fh, '<', $self->{fn} or return undef;
	my @st = stat $fh or die "fstat($self->{fn}): $!";
	$self->{ino_dev} = "@st[0, 1]";
	$self->{watch_fh} = $fh; # return value
}

sub new {
	my ($cls, $fn) = @_;
	my $self = bless { fn => $fn }, $cls;
	if ($ino_cls) {
		$self->{inot} = $ino_cls->new or die "E: $ino_cls->new: $!";
		$self->{inot}->blocking(0);
		my ($dn) = ($fn =~ m!\A(.+)/+[^/]+\z!);
		$self->{inot}->watch($dn // '.', $TAIL_MOD);
	} else {
		$self->{inot} = PublicInbox::FakeInotify->new;
	}
	$self->{inot}->watch($fn, $TAIL_MOD);
	reopen_file($self);
	$self->{inot}->watch($fn, $TAIL_MOD);
	$self;
}

sub getlines {
	my ($self, $timeo) = @_;
	my ($fh, $buf, $rfds, @ret, @events);
	my $end = defined($timeo) ? now + $timeo : undef;
again:
	while (1) {
		@events = $self->{inot}->read; # Linux::Inotify2::read
		last if @events;
		return () if defined($timeo) && (!$timeo || (now > $end));
		my $wait = 0.1;
		if ($ino_cls) {
			vec($rfds = '', $self->{inot}->fileno, 1) = 1;
			if (defined $end) {
				$wait = $end - now;
				$wait = 0 if $wait < 0;
			}
		}
		select($rfds, undef, undef, $wait);
	}
	# XXX do we care about @events contents?
	# use Data::Dumper; warn '# ',Dumper(\@events);
	if ($fh = $self->{watch_fh}) {
		sysread($fh, $buf, -s $fh) and
			push @ret, split(/^/sm, $buf);
		my @st = stat($self->{fn});
		if (!@st || "@st[0, 1]" ne $self->{ino_dev}) {
			delete @$self{qw(ino_dev watch_fh)};
		}
	}
	if ($fh = $self->{watch_fh} // reopen_file($self)) {
		sysread($fh, $buf, -s $fh) and
			push @ret, split(/^/sm, $buf);
	}
	goto again if (!@ret && (!defined($end) || now < $end));
	@ret;
}

1;
