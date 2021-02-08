# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# for systems lacking Linux::Inotify2 or IO::KQueue, just emulates
# enough of Linux::Inotify2
package PublicInbox::FakeInotify;
use strict;
use Time::HiRes qw(stat);
use PublicInbox::DS qw(add_timer);
sub IN_MODIFY () { 0x02 } # match Linux inotify
# my $IN_MOVED_TO = 0x80;
# my $IN_CREATE = 0x100;
sub MOVED_TO_OR_CREATE () { 0x80 | 0x100 }

my $poll_intvl = 2; # same as Filesys::Notify::Simple

sub new { bless { watch => {} }, __PACKAGE__ }

# behaves like Linux::Inotify2->watch
sub watch {
	my ($self, $path, $mask) = @_;
	my @st = stat($path) or return;
	my $k = "$path\0$mask";
	$self->{watch}->{$k} = $st[10]; # 10 - ctime
	bless [ $self->{watch}, $k ], 'PublicInbox::FakeInotify::Watch';
}

sub on_new_files ($$$$) {
	my ($events, $dh, $path, $old_ctime) = @_;
	while (defined(my $base = readdir($dh))) {
		next if $base =~ /\A\.\.?\z/;
		my $full = "$path/$base";
		my @st = stat($full);
		if (@st && $st[10] > $old_ctime) {
			push @$events,
				bless(\$full, 'PublicInbox::FakeInotify::Event')
		}
	}
}

# behaves like non-blocking Linux::Inotify2->read
sub read {
	my ($self) = @_;
	my $watch = $self->{watch} or return ();
	my $events = [];
	for my $x (keys %$watch) {
		my ($path, $mask) = split(/\0/, $x, 2);
		my @now = stat($path) or next;
		my $old_ctime = $watch->{$x};
		$watch->{$x} = $now[10];
		next if $old_ctime == $now[10];
		if ($mask & IN_MODIFY) {
			push @$events,
				bless(\$path, 'PublicInbox::FakeInotify::Event')
		} elsif ($mask & MOVED_TO_OR_CREATE) {
			opendir(my $dh, $path) or do {
				warn "W: opendir $path: $!\n";
				next;
			};
			on_new_files($events, $dh, $path, $old_ctime);
		}
	}
	@$events;
}

sub poll_once {
	my ($obj) = @_;
	$obj->event_step; # PublicInbox::InboxIdle::event_step
	add_timer($poll_intvl, \&poll_once, $obj);
}

package PublicInbox::FakeInotify::Watch;
use strict;

sub cancel {
	my ($self) = @_;
	delete $self->[0]->{$self->[1]};
}

sub name {
	my ($self) = @_;
	(split(/\0/, $self->[1], 2))[0];
}

package PublicInbox::FakeInotify::Event;
use strict;

sub fullname { ${$_[0]} }
1;
