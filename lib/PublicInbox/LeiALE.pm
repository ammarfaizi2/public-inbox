# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# All Locals Ever: track lei/store + externals ever used as
# long as they're on an accessible FS.  Includes "lei q" --include
# and --only targets that haven't been through "lei add-external".
# Typically: ~/.cache/lei/all_locals_ever.git
package PublicInbox::LeiALE;
use v5.12;
use parent qw(PublicInbox::LeiSearch PublicInbox::Lock);
use PublicInbox::Git;
use autodie qw(close open rename seek truncate);
use PublicInbox::Import;
use PublicInbox::LeiXSearch;
use Fcntl qw(SEEK_SET);

sub _new {
	my ($d) = @_;
	PublicInbox::Import::init_bare($d, 'ale');
	bless {
		git => PublicInbox::Git->new($d),
		lock_path => "$d/lei_ale.state", # dual-duty lock + state
		ibxish => [], # Inbox and ExtSearch (and LeiSearch) objects
	}, __PACKAGE__
}

sub new {
	my ($self, $lei) = @_;
	ref($self) or $self = _new($lei->cache_dir . '/all_locals_ever.git');
	my $lxs = PublicInbox::LeiXSearch->new;
	my $sto = $lei->_lei_store;
	$lxs->prepare_external($sto->search) if $sto;
	for my $loc ($lei->externals_each) { # locals only
		$lxs->prepare_external($loc) if -d $loc;
	}
	$self->refresh_externals($lxs, $lei);
	$self;
}

sub over {} # undef for xoids_for

sub overs_all { # for xoids_for (called only in lei workers?)
	my ($self) = @_;
	my $pid = $$;
	if (($self->{owner_pid} // $pid) != $pid) {
		delete($_->{over}) for @{$self->{ibxish}};
	}
	$self->{owner_pid} = $pid;
	grep(defined, map { $_->over } @{$self->{ibxish}});
}

sub refresh_externals {
	my ($self, $lxs, $lei) = @_;
	$self->git->cleanup;
	my $lk = $self->lock_for_scope;
	my $cur_lxs = ref($lxs)->new;
	my $orig = PublicInbox::IO::read_all $self->{lockfh};
	my $new = '';
	my $old = '';
	my $gone = 0;
	my %seen_ibxish; # $dir => any-defined value
	for my $dir (split(/\n/, $orig)) {
		if (-d $dir && -r _ && $cur_lxs->prepare_external($dir)) {
			$seen_ibxish{$dir} //= length($old .= "$dir\n");
		} else {
			++$gone;
		}
	}
	my @ibxish = $cur_lxs->locals;
	for my $x ($lxs->locals) {
		my $d = $lei->canonpath_harder($x->{inboxdir} // $x->{topdir});
		$seen_ibxish{$d} //= do {
			$new .= "$d\n";
			push @ibxish, $x;
		};
	}
	if ($new ne '' || $gone) {
		$self->{lockfh}->autoflush(1);
		if ($gone) {
			seek($self->{lockfh}, 0, SEEK_SET);
			truncate($self->{lockfh}, 0);
		} else {
			$old = '';
		}
		print { $self->{lockfh} } $old, $new or die "print: $!";
	}
	$new = '';
	my $f = $self->git->{git_dir}.'/objects/info/alternates';
	$old = PublicInbox::IO::try_cat $f;
	for my $x (@ibxish) {
		$new .= $lei->canonpath_harder($x->git->{git_dir})."/objects\n";
	}
	$self->{ibxish} = \@ibxish;
	return if $old eq $new;

	# this needs to be atomic since child processes may start
	# git-cat-file at any time
	my $tmp = "$f.$$.tmp";
	open my $fh, '>', $tmp;
	print $fh $new;
	close $fh;
	rename($tmp, $f)
}

1;
