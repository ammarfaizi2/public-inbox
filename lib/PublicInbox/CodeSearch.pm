# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# read-only external index for coderepos
# currently, it only indexes commits and repository metadata
# (pathname, root commits); not blob contents
package PublicInbox::CodeSearch;
use v5.12;
use parent qw(PublicInbox::Search);
use PublicInbox::Search qw(retry_reopen int_val xap_terms);
use constant {
	AT => 0, # author time YYYYMMDDHHMMSS, dt: for mail)
	CT => 1, # commit time (Unix time stamp, like TS/rt: in mail)
	CIDX_SCHEMA_VER => 1, # brand new schema for code search
	# for repos (`Tr'), CT(col=1) is used for the latest tip commit time
	# in refs/{heads,tags}.  AT(col=0) may be used to store disk usage
	# in the future, but disk usage calculation is espensive w/ alternates
};

# note: the non-X term prefix allocations are shared with Xapian omega,
# see xapian-applications/omega/docs/termprefixes.rst
# bool_pfx_internal:
#	type => 'T', # 'c' - commit, 'r' - repo GIT_DIR
#	tags are not indexed, only normal branches (refs/heads/*), not hidden
#	'P' # (pathname) GIT_DIR # uniq
#	'G' # (group) root commit (may have multiple roots)
my %bool_pfx_external = (
	oid => 'Q', # type:commit - git OID hex (40|64)-byte SHA-(1|256)
		# type:repo - rel2abs_collapsed(GIT_DIR)
	parent => 'XP',
	%PublicInbox::Search::PATCH_BOOL_COMMON,
);

my %prob_prefix = ( # copied from PublicInbox::Search
	# do we care about committer? or partial commit OID via Xapian?
	# o => 'XQ', # 'oid:' (bool) is exact, 'o:' (prob) can do partial
	%PublicInbox::Search::PATCH_PROB_COMMON,

	# default:
	'' => 'S A XQUOT XFN ' . $PublicInbox::Search::NON_QUOTED_BODY
);

sub new {
	my ($cls, $dir) = @_;
	bless { xpfx => "$dir/cidx".CIDX_SCHEMA_VER }, $cls;
}

sub cqparse_new ($) {
	my ($self) = @_;
	my $qp = $self->qp_init_common;
	my $cb = $qp->can('add_valuerangeprocessor') //
		$qp->can('add_rangeprocessor'); # Xapian 1.5.0+
	$cb->($qp, $PublicInbox::Search::NVRP->new(AT, 'd:')); # mairix compat
	$cb->($qp, $PublicInbox::Search::NVRP->new(AT, 'dt:')); # mail compat
	$cb->($qp, $PublicInbox::Search::NVRP->new(CT, 'ct:'));

	while (my ($name, $pfx) = each %bool_pfx_external) {
		$qp->add_boolean_prefix($name, $_) for split(/ /, $pfx);
	}
	while (my ($name, $pfx) = each %prob_prefix) {
		$qp->add_prefix($name, $_) for split(/ /, $pfx);
	}
	$qp;
}

# returns a Xapian::Query to filter by roots
sub roots_filter { # retry_reopen callback
	my ($self, $git_dir) = @_;
	my $xdb = $self->xdb;
	my $P = 'P'.$git_dir;
	my ($cur, $end) = ($xdb->postlist_begin($P), $xdb->postlist_end($P));
	if ($cur == $end) {
		warn "W: $git_dir not indexed?\n";
		return;
	}
	my @roots = xap_terms('G', $xdb, $cur->get_docid);
	if (!@roots) {
		warn "W: $git_dir has no root commits?\n";
		return;
	}
	my $q = $PublicInbox::Search::X{Query}->new('G'.shift(@roots));
	for my $r (@roots) {
		$q = $PublicInbox::Search::X{Query}->new(
					PublicInbox::Search::OP_OR(),
					$q, 'G'.$r);
	}
	$q;
}

sub mset {
	my ($self, $qry_str, $opt) = @_;
	my $qp = $self->{qp} //= cqparse_new($self);
	my $qry = $qp->parse_query($qry_str, $self->{qp_flags});

	# limit to commits with shared roots
	if (defined(my $git_dir = $opt->{git_dir})) {
		my $rf = retry_reopen($self, \&roots_filter, $git_dir)
			or return;

		$qry = $PublicInbox::Search::X{Query}->new(
				PublicInbox::Search::OP_FILTER(),
				$qry, $rf);
	}

	# we only want commits:
	$qry = $PublicInbox::Search::X{Query}->new(
				PublicInbox::Search::OP_FILTER(),
				$qry, 'T'.'c');
	$self->do_enquire($qry, $opt, CT);
}

1;
