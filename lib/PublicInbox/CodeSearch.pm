# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# read-only external index for coderepos
# currently, it only indexes commits and repository metadata
# (pathname, root commits); not blob contents
package PublicInbox::CodeSearch;
use v5.12;
use parent qw(PublicInbox::Search);
use PublicInbox::Config;
use PublicInbox::Search qw(retry_reopen int_val xap_terms);
use Compress::Zlib qw(uncompress);
use constant {
	AT => 0, # author time YYYYMMDDHHMMSS, dt: for mail)
	CT => 1, # commit time (Unix time stamp, like TS/rt: in mail)
	CIDX_SCHEMA_VER => 1, # brand new schema for code search
	# for repos (`Tr'), CT(col=1) is used for the latest tip commit time
	# in refs/{heads,tags}.  AT(col=0) may be used to store disk usage
	# in the future, but disk usage calculation is espensive w/ alternates
};
our @CODE_NRP;
our @CODE_VMAP = (
	[ AT, 'd:' ], # mairix compat
	[ AT, 'dt:' ], # mail compat
	[ CT, 'ct:' ],
);

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
	my ($cls, $dir, $cfg) = @_;
	# can't have a PublicInbox::Config here due to circular refs
	bless { xpfx => "$dir/cidx".CIDX_SCHEMA_VER,
		-cfg_f => $cfg->{-f} }, $cls;
}

sub join_data_key ($) { "join:$_[0]->{-cfg_f}" }

sub join_data {
	my ($self) = @_;
	my $key = join_data_key($self);
	my $cur = $self->xdb->get_metadata($key) or return;
	$cur = eval { PublicInbox::Config::json()->decode(uncompress($cur)) };
	warn "E: $@ (corrupt metadata in `$key' key?)" if $@;
	$cur;
}

sub qparse_new ($) {
	my ($self) = @_;
	my $qp = $self->qp_init_common;
	my $cb = $qp->can('add_valuerangeprocessor') //
		$qp->can('add_rangeprocessor'); # Xapian 1.5.0+
	if (!@CODE_NRP) {
		@CODE_NRP = map {
			$PublicInbox::Search::NVRP->new(@$_)
		} @CODE_VMAP;
	}
	$cb->($qp, $_) for @CODE_NRP;
	while (my ($name, $pfx) = each %bool_pfx_external) {
		$qp->add_boolean_prefix($name, $_) for split(/ /, $pfx);
	}
	while (my ($name, $pfx) = each %prob_prefix) {
		$qp->add_prefix($name, $_) for split(/ /, $pfx);
	}
	$qp;
}

sub generate_cxx () { # generates snippet for xap_helper.h
	my ($line, $file) = (__LINE__ + 2, __FILE__);
	my $ret = <<EOM;
# line ${\__LINE__} "${\__FILE__}"
static NRP *code_nrp[${\scalar(@CODE_VMAP)}];
static void code_nrp_init(void)
{
EOM
	for (0..$#CODE_VMAP) {
		my $x = $CODE_VMAP[$_];
		$ret .= qq{\tcode_nrp[$_] = new NRP($x->[0], "$x->[1]");\n}
	}
$ret .= <<EOM;
}

# line ${\__LINE__} "${\__FILE__}"
static void qp_init_code_search(Xapian::QueryParser *qp)
{
	for (size_t i = 0; i < MY_ARRAY_SIZE(code_nrp); i++)
		qp->ADD_RP(code_nrp[i]);
EOM
	for my $name (sort keys %bool_pfx_external) {
		for (split(/ /, $bool_pfx_external{$name})) {
			$ret .= qq{\tqp->add_boolean_prefix("$name", "$_");\n}
		}
	}
	for my $name (sort keys %prob_prefix) {
		for (split(/ /, $prob_prefix{$name})) {
			$ret .= qq{\tqp->add_prefix("$name", "$_");\n}
		}
	}
	$ret .= "}\n";
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
	my $qp = $self->{qp} //= qparse_new($self);
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

sub roots2paths { # for diagnostics
	my ($self) = @_;
	my $cur = $self->xdb->allterms_begin('G');
	my $end = $self->{xdb}->allterms_end('G');
	my $qrepo = $PublicInbox::Search::X{Query}->new('T'.'r');
	my $enq = $PublicInbox::Search::X{Enquire}->new($self->{xdb});
	$enq->set_weighting_scheme($PublicInbox::Search::X{BoolWeight}->new);
	$enq->set_docid_order($PublicInbox::Search::ENQ_ASCENDING);
	my %ret;
	for (; $cur != $end; $cur++) {
		my $G_oidhex = $cur->get_termname;
		my $qry = $PublicInbox::Search::X{Query}->new(
				PublicInbox::Search::OP_FILTER(),
				$qrepo, $G_oidhex);
		$enq->set_query($qry);
		my ($size, $off, $lim) = (0, 0, 100000);
		my $dirs = $ret{substr($G_oidhex, 1)} = [];
		do {
			my $mset = $enq->get_mset($off += $size, $lim);
			for my $x ($mset->items) {
				my $tmp = xap_terms('P', $x->get_document);
				push @$dirs, keys %$tmp;
			}
			$size = $mset->size;
		} while ($size);
		@$dirs = sort @$dirs;
	}
	\%ret;
}

sub paths2roots { # for diagnostics
	my ($self) = @_;
	my %ret;
	my $tmp = roots2paths($self);
	for my $root_oidhex (keys %$tmp) {
		my $paths = delete $tmp->{$root_oidhex};
		push @{$ret{$_}}, $root_oidhex for @$paths;
	}
	@$_ = sort(@$_) for values %ret;
	\%ret;
}

1;
