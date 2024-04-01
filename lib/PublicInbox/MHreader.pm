# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# MH reader, based on Lib/mailbox.py in cpython source
package PublicInbox::MHreader;
use v5.12;
use PublicInbox::InboxWritable qw(eml_from_path);
use PublicInbox::OnDestroy;
use PublicInbox::IO qw(try_cat);
use PublicInbox::MdirSort;
use Carp qw(carp);
use autodie qw(chdir closedir opendir);

my %FL2OFF = ( # mh_sequences key => our keyword
	replied => 0,
	flagged => 1,
	unseen => 2, # negate
);
my @OFF2KW = qw(answered flagged); # [2] => unseen (negated)

sub new {
	my ($cls, $dir, $cwdfh) = @_;
	if (substr($dir, -1) ne '/') { # TODO: do this earlier
		carp "W: appending `/' to `$dir' (fix caller)\n";
		$dir .= '/';
	}
	bless { dir => $dir, cwdfh => $cwdfh }, $cls;
}

sub read_mh_sequences ($) { # caller must chdir($self->{dir})
	my ($self) = @_;
	my ($fl, $off, @n);
	my @seq = ('', '', '');
	for (split /\n+/s, try_cat('.mh_sequences')) {
		($fl, @n) = split /[: \t]+/;
		$off = $FL2OFF{$fl} // do { warn <<EOM;
W: unknown `$fl' in $self->{dir}.mh_sequences (ignoring)
EOM
			next;
		};
		@n = grep /\A[0-9]+\z/s, @n; # don't stat, yet
		if (@n) {
			@n = sort { $b <=> $a } @n; # to avoid resize
			my $buf = '';
			vec($buf, $_, 1) = 1 for @n;
			$seq[$off] = $buf;
		}
	}
	\@seq;
}

sub mh_each_file {
	my ($self, $efcb, @arg) = @_;
	opendir(my $dh, my $dir = $self->{dir});
	my $restore = on_destroy \&chdir, $self->{cwdfh};
	chdir($dh);
	my $sort = $self->{sort};
	if (defined $sort && "@$sort" ne 'none') {
		my @sort = map {
			my @tmp = $_ eq '' ? ('sequence') : split(/[, ]/);
			# sorting by name alphabetically makes no sense for MH:
			for my $k (@tmp) {
				s/\A(\-|\+|)(?:name|)\z/$1sequence/;
			}
			@tmp;
		} @$sort;
		my @n = grep /\A[0-9]+\z/s, readdir $dh;
		mdir_sort \@n, \@sort;
		$efcb->($dir, $_, $self, @arg) for @n;
	} else {
		while (readdir $dh) { # perl v5.12+ to set $_ on readdir
			$efcb->($dir, $_, $self, @arg) if /\A[0-9]+\z/s;
		}
	}
	closedir $dh; # may die
}

sub kw_for ($$) {
	my ($self, $n) = @_;
	my $seq = $self->{mh_seq} //= read_mh_sequences($self);
	my @kw = map { vec($seq->[$_], $n, 1) ? $OFF2KW[$_] : () } (0, 1);
	vec($seq->[2], $n, 1) or push @kw, 'seen';
	\@kw;
}

sub _file2eml { # mh_each_file / mh_read_one cb
	my ($dir, $n, $self, $ucb, @arg) = @_;
	my $eml = eml_from_path($n);
	$ucb->($dir, $n, kw_for($self, $n), $eml, @arg) if $eml;
}

sub mh_each_eml {
	my ($self, $ucb, @arg) = @_;
	mh_each_file($self, \&_file2eml, $ucb, @arg);
}

sub mh_read_one {
	my ($self, $n, $ucb, @arg) = @_;
	my $restore = on_destroy \&chdir, $self->{cwdfh};
	chdir(my $dir = $self->{dir});
	_file2eml($dir, $n, $self, $ucb, @arg);
}

1;
