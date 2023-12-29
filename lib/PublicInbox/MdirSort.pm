# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# used for sorting MH (and (TODO) Maildir) names
# TODO: consider sort(1) to parallelize sorting of gigantic directories
package PublicInbox::MdirSort;
use v5.12;
use Time::HiRes ();
use parent qw(Exporter);
use Fcntl qw(S_ISREG);
our @EXPORT = qw(mdir_sort);
my %ST = (sequence => 0, size => 1, atime => 2, mtime => 3, ctime => 4);

sub mdir_sort ($$;$) {
	my ($ent, $sort, $max) = @_;
	my @st;
	my @ent = map {
		@st = Time::HiRes::stat $_;
		# name, size, {a,m,c}time
		S_ISREG($st[2]) ? [ $_, @st[7..10] ] : ();
	} @$ent;
	@ent = grep { $_->[1] <= $max } @ent if $max;
	use sort 'stable';
	for my $s (@$sort) {
		if ($s =~ /\A(\-|\+|)name\z/) {
			if ($1 eq '-') {
				@ent = sort { $b->[0] cmp $a->[0] } @ent;
			} else {
				@ent = sort { $a->[0] cmp $b->[0] } @ent;
			}
		} elsif ($s =~ /\A(\-|\+|)
				(sequence|size|ctime|mtime|atime)\z/x) {
			my $key = $ST{$2};
			if ($1 eq '-') {
				@ent = sort { $b->[$key] <=> $a->[$key] } @ent;
			} else {
				@ent = sort { $a->[$key] <=> $b->[$key] } @ent;
			}
		} else {
			die "E: unrecognized sort parameter: `$s'";
		}
	}
	@$ent = map { $_->[0] } @ent;
}

1;
