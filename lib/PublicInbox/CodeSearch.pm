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
use PublicInbox::Compat qw(uniqstr);
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
	[ AT, 'dt:' ], # public-inbox mail compat
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
	bless { topdir => $dir, xpfx => "$dir/cidx".CIDX_SCHEMA_VER,
		-cfg_f => $cfg->{-f} }, $cls;
}

sub join_data_key ($) { "join:$_[0]->{-cfg_f}" }

sub join_data {
	my ($self) = @_;
	my $key = join_data_key($self);
	my $cur = $self->xdb->get_metadata($key) or return;
	$cur = eval { PublicInbox::Config::json()->decode(uncompress($cur)) };
	warn "E: $@ (corrupt metadata in `$key' key?)" if $@;
	my @m = grep { ref($cur->{$_}) ne 'ARRAY' } qw(ekeys roots ibx2root);
	if (@m) {
		warn <<EOM;
W: $self->{topdir} join data for $self->{-cfg_f} missing: @m
EOM
		undef;
	} elsif (@{$cur->{ekeys}} < @{$cur->{ibx2root}}) {
		warn <<EOM;
W: $self->{topdir} join data for $self->{-cfg_f} mismatched ekeys and ibx2root
EOM
		undef;
	} else {
		$cur;
	}
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
				push @$dirs, xap_terms('P', $x->get_document);
			}
			$size = $mset->size;
		} while ($size);
		@$dirs = sort(uniqstr(@$dirs));
	}
	\%ret;
}

sub docids_of_git_dir ($$) {
	my ($self, $git_dir) = @_;
	my @ids = $self->docids_by_postlist('P'.$git_dir);
	warn <<"" if @ids > 1;
BUG: (non-fatal) $git_dir indexed multiple times in $self->{topdir}

	@ids;
}

sub root_oids ($$) {
	my ($self, $git_dir) = @_;
	my @ids = docids_of_git_dir $self, $git_dir or warn <<"";
BUG? (non-fatal) `$git_dir' not indexed in $self->{topdir}

	my @ret = map { xap_terms('G', $self->xdb, $_) } @ids;
	@ret = uniqstr(@ret) if @ids > 1;
	@ret;
}

sub paths2roots {
	my ($self, $paths) = @_;
	my %ret;
	if ($paths) {
		for my $p (keys %$paths) { @{$ret{$p}} = root_oids($self, $p) }
	} else {
		my $tmp = roots2paths($self);
		for my $root_oidhex (keys %$tmp) {
			my $paths = delete $tmp->{$root_oidhex};
			push @{$ret{$_}}, $root_oidhex for @$paths;
		}
		@$_ = sort(@$_) for values %ret;
	}
	\%ret;
}

sub load_ct { # retry_reopen cb
	my ($self, $git_dir) = @_;
	my @ids = docids_of_git_dir $self, $git_dir or return;
	for (@ids) {
		my $doc = $self->get_doc($_) // next;
		return int_val($doc, CT);
	}
}

sub load_commit_times { # each_cindex callback
	my ($self, $todo) = @_; # todo = [ [ time, git ], [ time, git ] ...]
	my (@pending, $rec, $ct);
	while ($rec = shift @$todo) {
		$ct = $self->retry_reopen(\&load_ct, $rec->[1]->{git_dir});
		if (defined $ct) {
			$rec->[0] = $ct;
		} else { # may be in another cindex:
			push @pending, $rec;
		}
	}
	@$todo = @pending;
}

sub load_coderepos { # each_cindex callback
	my ($self, $pi_cfg) = @_;
	my $name = $self->{name};
	my $cfg_f = $pi_cfg->{-f};
	my $lpfx = $self->{localprefix} or return warn <<EOM;
W: cindex.$name.localprefix unset in $cfg_f, ignoring cindex.$name
EOM
	my $lre = join('|', map { $_ .= '/'; tr!/!/!s; quotemeta } @$lpfx);
	$lre = qr!\A(?:$lre)!;
	my $coderepos = $pi_cfg->{-coderepos};
	my $nick_pfx = $name eq '' ? '' : "$name/";
	my %dir2cr;
	for my $p ($self->all_terms('P')) {
		my $nick = $p;
		$nick =~ s!$lre!$nick_pfx!s or next;
		$dir2cr{$p} = $coderepos->{$nick} //= do {
			my $git = PublicInbox::Git->new($p);
			my %dedupe = ($nick => undef);
			($git->{nick}) = keys %dedupe; # for git->pub_urls
			$git;
		};
	}
	my $jd = $self->retry_reopen(\&join_data, $self) or return warn <<EOM;
W: cindex.$name.topdir=$self->{topdir} has no usable join data for $cfg_f
EOM
	my ($ekeys, $roots, $ibx2root) = @$jd{qw(ekeys roots ibx2root)};
	my $roots2paths = roots2paths($self);
	my %dedupe; # 50x alloc reduction w/ lore + gko mirror (Mar 2024)
	for my $root_offs (@$ibx2root) {
		my $ekey = shift(@$ekeys) // die 'BUG: {ekeys} empty';
		scalar(@$root_offs) or next;
		my $ibx = $pi_cfg->lookup_eidx_key($ekey) // do {
			warn "W: `$ekey' gone from $cfg_f\n";
			next;
		};
		my $gits = $ibx->{-repo_objs} //= [];
		my $cr_score = $ibx->{-cr_score} //= {};
		my %ibx_p2g = map { $_->{git_dir} => $_ } @$gits;
		my $ibx2self; # cindex has an association w/ inbox?
		for (@$root_offs) { # sorted by $nr descending
			my ($nr, $root_off) = @$_;
			my $root_oid = $roots->[$root_off] // do {
				warn <<EOM;
BUG: root #$root_off invalid in join data for `$ekey' with $cfg_f
EOM
				next;
			};
			my $git_dirs = $roots2paths->{$root_oid};
			my @gits = map { $dir2cr{$_} // () } @$git_dirs;
			$cr_score->{$_->{nick}} //= $nr for @gits;
			@$git_dirs = grep { !$ibx_p2g{$_} } @$git_dirs;
			# @$git_dirs or warn "W: no matches for $root_oid\n";
			for (@$git_dirs) {
				if (my $git = $dir2cr{$_}) {
					$ibx_p2g{$_} = $git;
					$ibx2self = 1;
					if (!$ibx->{-hide_www}) {
						# don't stringify $nr directly
						# to avoid long-lived PV
						my $k = ($nr + 0)."\0".
							($ibx + 0);
						my $s = $dedupe{$k} //=
							[ $nr, $ibx->{name} ];
						push @{$git->{ibx_score}}, $s;
					}
					push @$gits, $git;
				} else {
					warn <<EOM;
W: no coderepo available for $_ (localprefix=@$lpfx)
EOM
				}
			}
		}
		if (@$gits) {
			push @{$ibx->{-csrch}}, $self if $ibx2self;
		} else {
			delete $ibx->{-repo_objs};
			delete $ibx->{-cr_score};
		}
	}
	for my $git (values %dir2cr) {
		my $s = $git->{ibx_score};
		@$s = sort { $b->[0] <=> $a->[0] } @$s if $s;
	}
	my $ALL = $pi_cfg->ALL or return;
	my @alls_gits = sort {
		scalar @{$b->{ibx_score} // []} <=>
			scalar @{$a->{ibx_score} // []}
	} values %$coderepos;
	my $gits = $ALL->{-repo_objs} //= [];
	push @$gits, @alls_gits;
	my $cr_score = $ALL->{-cr_score} //= {};
	$cr_score->{$_->{nick}} //= scalar(@{$_->{ibx_score}//[]}) for @$gits;
}

sub repos_sorted {
	my $pi_cfg = shift;
	my @recs = map { [ 0, $_ ] } @_; # PublicInbox::Git objects
	my @todo = @recs;
	$pi_cfg->each_cindex(\&load_commit_times, \@todo);
	@recs = sort { $b->[0] <=> $a->[0] } @recs; # sort by commit time
}

1;
