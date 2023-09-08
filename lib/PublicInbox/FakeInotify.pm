# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# for systems lacking Linux::Inotify2 or IO::KQueue, just emulates
# enough of Linux::Inotify2 we use.
package PublicInbox::FakeInotify;
use v5.12;
use Time::HiRes qw(stat);
use PublicInbox::DS qw(add_timer);
use Errno qw(ENOTDIR ENOENT);
sub IN_MODIFY () { 0x02 } # match Linux inotify
# my $IN_MOVED_FROM	 0x00000040	/* File was moved from X.  */
# my $IN_MOVED_TO = 0x80;
# my $IN_CREATE = 0x100;
sub MOVED_TO_OR_CREATE () { 0x80 | 0x100 }
sub IN_DELETE () { 0x200 }
sub IN_DELETE_SELF () { 0x400 }
sub IN_MOVE_SELF () { 0x800 }

my $poll_intvl = 2; # same as Filesys::Notify::Simple

sub new { bless {}, __PACKAGE__ }

sub on_dir_change ($$$$$) { # used by KQNotify subclass
	my ($self, $events, $dh, $path, $dir_delete) = @_;
	my $old = $self->{dirlist}->{$path};
	my @cur = grep(!/\A\.\.?\z/, readdir($dh));
	$self->{dirlist}->{$path} = \@cur;

	# new files:
	my %tmp = map { $_ => undef } @cur;
	delete @tmp{@$old};
	push(@$events, map {
		bless \"$path/$_", 'PublicInbox::FakeInotify::Event'
	} keys %tmp);

	if ($dir_delete) {
		%tmp = map { $_ => undef } @$old;
		delete @tmp{@cur};
		push(@$events, map {
			bless \"$path/$_", 'PublicInbox::FakeInotify::GoneEvent'
		} keys %tmp);
	}
}

sub watch_open ($$$) { # used by KQNotify subclass
	my ($self, $path, $dir_delete) = @_;
	my ($fh, @st, @st0, $tries);
	do {
again:
		unless (@st0 = stat($path)) {
			warn "W: stat($path): $!" if $! != ENOENT;
			return;
		}
		if (!(-d _ ? opendir($fh, $path) : open($fh, '<', $path))) {
			goto again if $! == ENOTDIR && ++$tries < 10;
			warn "W: open($path): $!" if $! != ENOENT;
			return;
		}
		@st = stat($fh) or die "fstat($path): $!";
	} while ("@st[0,1]" ne "@st0[0,1]" &&
		((++$tries < 10) || (warn(<<EOM) && return)));
E: $path switching inodes too frequently to watch
EOM
	if (-d _) {
		$self->{dirlist}->{$path} = [];
		on_dir_change($self, [], $fh, $path, $$dir_delete);
	} else {
		$$dir_delete = 0;
	}
	bless [ @st[0, 1, 10], $path, $fh ], 'PublicInbox::FakeInotify::Watch'
}

# behaves like Linux::Inotify2->watch
sub watch {
	my ($self, $path, $mask) = @_; # mask is ignored
	my $dir_delete = $mask & IN_DELETE ? 1 : 0;
	my $w = watch_open($self, $path, \$dir_delete) or return;
	pop @$w; # no need to keep $fh open for non-kqueue
	$self->{watch}->{"$path\0$dir_delete"} = $w;
}

sub gone ($$$) { # used by KQNotify subclass
	my ($self, $ident, $path) = @_;
	delete $self->{watch}->{$ident};
	delete $self->{dirlist}->{$path};
	bless(\$path, 'PublicInbox::FakeInotify::SelfGoneEvent');
}

# fuzz the time for freshly modified directories for low-res VFS
sub dir_adj ($) {
	my ($old_ctime) = @_;
	my $now = Time::HiRes::time;
	my $diff = $now - $old_ctime;
	($diff > -1 && $diff < 1) ? 1 : 0;
}

# behaves like non-blocking Linux::Inotify2->read
sub read {
	my ($self) = @_;
	my $ret = [];
	while (my ($ident, $w) = each(%{$self->{watch}})) {
		if (!@$w) { # cancelled
			delete($self->{watch}->{$ident});
			next;
		}
		my $dir_delete = (split(/\0/, $ident, 2))[1];
		my ($old_dev, $old_ino, $old_ctime, $path) = @$w;
		my @new_st = stat($path);
		warn "W: stat($path): $!\n" if !@new_st && $! != ENOENT;
		if (!@new_st || "$old_dev $old_ino" ne "@new_st[0,1]") {
			push @$ret, gone($self, $ident, $path);
			next;
		}
		if (-d _ && $new_st[10] > ($old_ctime - dir_adj($old_ctime))) {
			opendir(my $fh, $path) or do {
				if ($! == ENOENT || $! == ENOTDIR) {
					push @$ret, gone($self, $ident, $path);
				} else {
					warn "W: opendir($path): $!";
				}
				next;
			};
			@new_st = stat($fh) or die "fstat($path): $!";
			if ("$old_dev $old_ino" ne "@new_st[0,1]") {
				push @$ret, gone($self, $ident, $path);
				next;
			}
			$w->[2] = $new_st[10];
			on_dir_change($self, $ret, $fh, $path, $dir_delete);
		} elsif ($new_st[10] > $old_ctime) { # regular files, etc
			$w->[2] = $new_st[10];
			push @$ret, bless(\$path,
					'PublicInbox::FakeInotify::Event');
		}
	}
	@$ret;
}

sub poll_once {
	my ($obj) = @_;
	$obj->event_step; # PublicInbox::InboxIdle::event_step
	add_timer($poll_intvl, \&poll_once, $obj);
}

package PublicInbox::FakeInotify::Watch;
use v5.12;

sub cancel { @{$_[0]} = () }

sub name { $_[0]->[3] }

package PublicInbox::FakeInotify::Event;
use v5.12;

sub fullname { ${$_[0]} }

sub IN_DELETE { 0 }
sub IN_MOVED_FROM { 0 }
sub IN_DELETE_SELF { 0 }

package PublicInbox::FakeInotify::GoneEvent;
use v5.12;
our @ISA = qw(PublicInbox::FakeInotify::Event);

sub IN_DELETE { 1 }
sub IN_MOVED_FROM { 0 }

package PublicInbox::FakeInotify::SelfGoneEvent;
use v5.12;
our @ISA = qw(PublicInbox::FakeInotify::GoneEvent);

sub IN_DELETE_SELF { 1 }

1;
