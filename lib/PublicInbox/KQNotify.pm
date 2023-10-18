# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# implements the small subset of Linux::Inotify2 functionality we use
# using IO::KQueue on *BSD systems.
package PublicInbox::KQNotify;
use v5.12;
use parent qw(PublicInbox::FakeInotify);
use IO::KQueue;
use PublicInbox::DSKQXS; # wraps IO::KQueue for fork-safe DESTROY
use Errno qw(ENOENT);

# NOTE_EXTEND detects rename(2), NOTE_WRITE detects link(2)
sub MOVED_TO_OR_CREATE () { NOTE_EXTEND|NOTE_WRITE }

sub new {
	my ($class) = @_;
	bless { dskq => PublicInbox::DSKQXS->new }, $class;
}

sub watch {
	my ($self, $path, $mask) = @_;
	my $dir_delete = $mask & NOTE_DELETE ? 1 : 0;
	my $w = $self->watch_open($path, \$dir_delete) or return;
	$w->[2] = pop @$w; # ctime is unused by this subclass
	my $ident = fileno($w->[2]) // die "BUG: bad fileno $w->[2]: $!";
	$self->{dskq}->{kq}->EV_SET($ident, # ident (fd)
		EVFILT_VNODE, # filter
		EV_ADD | EV_CLEAR, # flags
		$mask, # fflags
		0, $dir_delete); # data, udata
	$self->{watch}->{$ident} = $w;
}

# emulate Linux::Inotify::fileno
sub fileno { ${$_[0]->{dskq}->{kq}} }

# noop for Linux::Inotify2 compatibility.  Unlike inotify,
# kqueue doesn't seem to overflow since it's limited by the number of
# open FDs the process has
sub on_overflow {}

# noop for Linux::Inotify2 compatibility, we use `0' timeout for ->kevent
sub blocking {}

# behave like Linux::Inotify2->read
sub read {
	my ($self) = @_;
	my $events = [];
	for my $kev ($self->{dskq}->{kq}->kevent(0)) {
		my $ident = $kev->[KQ_IDENT];
		my $w = $self->{watch}->{$ident} or next;
		if (!@$w) { # cancelled
			delete($self->{watch}->{$ident});
			next;
		}
		my $dir_delete = $kev->[KQ_UDATA];
		my ($old_dev, $old_ino, $fh, $path) = @$w;
		my @new_st = stat($path);
		warn "W: stat($path): $!\n" if !@new_st && $! != ENOENT;
		if (!@new_st || "$old_dev $old_ino" ne "@new_st[0,1]") {
			push(@$events, $self->gone($ident, $path));
			next;
		}
		if (-d _) {
			rewinddir($fh);
			$self->on_dir_change($events, $fh, $path, $dir_delete);
		} else {
			push @$events, bless(\$path,
					'PublicInbox::FakeInotify::Event');
		}
	}
	@$events;
}

1;
