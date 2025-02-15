# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# common git alternates + all.git||ALL.git management code
package PublicInbox::MultiGit;
use strict;
use v5.10.1;
use PublicInbox::Spawn qw(run_die run_qx);
use PublicInbox::Import;
use PublicInbox::CfgWr;
use PublicInbox::Git qw(git_exe);
use File::Temp 0.19;
use List::Util qw(max);
use PublicInbox::IO qw(read_all);
use autodie qw(chmod close rename);

sub new {
	my ($cls, $topdir, $all, $epfx) = @_;
	bless {
		topdir => $topdir, # inboxdir || extindex.*.topdir
		all => $all, # all.git or ALL.git
		epfx => $epfx, # "git" (inbox) or "local" (lei/store)
	}, $cls;
}

sub read_alternates {
	my ($self, $moderef, $prune) = @_;
	my $objpfx = "$self->{topdir}/$self->{all}/objects/";
	my $f = "${objpfx}info/alternates";
	my %alt; # line => score
	my %seen; # $st_dev\0$st_ino => count
	my $other = 0;
	if (open(my $fh, '<', $f)) {
		my $is_edir = defined($self->{epfx}) ?
			qr!\A\Q../../$self->{epfx}\E/([0-9]+)\.git/objects\z! :
			undef;
		$$moderef = (stat($fh))[2] & 07777;
		for my $rel (read_all($fh, -s _)) {
			chomp(my $dir = $rel);
			my $score;
			if (defined($is_edir) && $dir =~ $is_edir) {
				$score = $1 + 0;
				substr($dir, 0, 0) = $objpfx;
			} else { # absolute paths, if any (extindex)
				$score = --$other;
			}
			if (my @st = stat($dir)) {
				next if $seen{"$st[0]\0$st[1]"}++;
				$alt{$rel} = $score;
			} else {
				warn "W: stat($dir) failed: $! ($f)";
				if ($prune) {
					++$$prune;
				} else {
					$alt{$rel} = $score;
				}
			}
		}
	} elsif (!$!{ENOENT}) {
		die "E: open($f): $!";
	}
	(\%alt, \%seen);
}

sub epoch_dir { "$_[0]->{topdir}/$_[0]->{epfx}" }

sub write_alternates {
	my ($self, $mode, $alt, @new) = @_;
	my $all_dir = "$self->{topdir}/$self->{all}";
	PublicInbox::Import::init_bare($all_dir);
	my @out = sort { $alt->{$b} <=> $alt->{$a} } keys %$alt;
	my $info_dir = "$all_dir/objects/info";
	my $fh = File::Temp->new(TEMPLATE => 'alt-XXXX', DIR => $info_dir);
	print $fh @out, @new;
	chmod($mode, $fh);
	close $fh;
	rename($fh->filename, "$info_dir/alternates");
	$fh->unlink_on_destroy(0);
}

# returns true if new epochs exist
sub merge_epochs {
	my ($self, $alt, $seen) = @_;
	my $epoch_dir = epoch_dir($self);
	if (opendir my $dh, $epoch_dir) {
		my $has_new;
		for my $bn (grep(/\A[0-9]+\.git\z/, readdir($dh))) {
			my $rel = "../../$self->{epfx}/$bn/objects\n";
			next if exists($alt->{$rel});
			if (my @st = stat("$epoch_dir/$bn/objects")) {
				next if $seen->{"$st[0]\0$st[1]"}++;
				$alt->{$rel} = substr($bn, 0, -4) + 0;
				$has_new = 1;
			} else {
				warn "E: stat($epoch_dir/$bn/objects): $!";
			}
		}
		$has_new;
	} else {
		$!{ENOENT} ? undef : die "opendir($epoch_dir): $!";
	}
}

sub fill_alternates {
	my ($self) = @_;
	my ($alt, $seen) = read_alternates($self, \(my $mode = 0644));
	merge_epochs($self, $alt, $seen) and
		write_alternates($self, $mode, $alt);
}

sub epoch_cfg_set {
	my ($self, $epoch_nr) = @_;
	my $f = epoch_dir($self)."/$epoch_nr.git/config";
	my $v = "../../$self->{all}/config";
	my @cmd = (git_exe, qw(config -f), $f, 'include.path');
	if (-r $f) {
		chomp(my $x = run_qx(\@cmd));
		return if $x eq $v;
		$? = 0; # don't influence _wq_done_wait in -clone
	}
	PublicInbox::CfgWr->new($f)->set('include.path', $v)->commit;
}

sub add_epoch {
	my ($self, $epoch_nr) = @_;
	my $git_dir = epoch_dir($self)."/$epoch_nr.git";
	my $f = "$git_dir/config";
	my $existing = -f $f;
	PublicInbox::Import::init_bare($git_dir);
	epoch_cfg_set($self, $epoch_nr) unless $existing;
	fill_alternates($self);
	$git_dir;
}

sub git_epochs  {
	my ($self) = @_;
	if (opendir(my $dh, epoch_dir($self))) {
		my @epochs = map {
			substr($_, 0, -4) + 0; # drop ".git" suffix
		} grep(/\A[0-9]+\.git\z/, readdir($dh));
		wantarray ? sort { $b <=> $a } @epochs : (max(@epochs) // 0);
	} elsif ($!{ENOENT}) {
		wantarray ? () : 0;
	} else {
		die(epoch_dir($self).": $!");
	}
}

1;
