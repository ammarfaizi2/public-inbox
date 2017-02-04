# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Read-only search interface for use by the Repobrowse web interface
# RepoGitSearchIdx builds upon this for writing a Xapian DB.
package PublicInbox::RepoGitSearch;
use strict;
use warnings;
use Search::Xapian qw/:standard/;

# values for ranges and sorting
use constant {
	CD => 0, # commit date stamp (YYYYMMDD)
	AD => 1, # author date stamp (YYYYMMDD)

	REPO_SCHEMA_VERSION => 1,
	# n.b. FLAG_PURE_NOT is expensive not suitable for a public website
	# as it could become a denial-of-service vector
	QP_FLAGS => FLAG_PHRASE|FLAG_BOOLEAN|FLAG_LOVEHATE|FLAG_WILDCARD,
};
our $LANG = 'english';

my %bool_pfx_internal = (
	type => 'T', # "commit", "tag", or "ref"
);

my %bool_pfx_external = (
	ref => 'XREF', # refname (belongs to)
);

my %prob_prefix = (
	id => 'Q', # git object ID, partial matches supported
	p => 'XP', # parent commit (partial)
	s => 'S', # subject
	a => 'A', # Author name + email
	c => 'XC', # Committer name + email
	ac => 'A XC', # Author and Committer name + email
	b => 'XBODY', # commit message body
	bs => 'S XBODY', # commit message (subject + body)
	diff_fn => 'XDFN', # changed filenames
	diff_hdr => 'XDHH', # diff hunk header
	diff_ctx => 'XDCTX', # diff context
	diff_a => 'XDFA', # diff a/ file (before)
	diff_b => 'XDFB', # diff b/ file (after)
	diff => 'XDFN XDHH XDCTX XDFA XDFB', # entire diff
	preimg => 'XPRE', # blob pre-image (full)
	postimg => 'XPOST', # blob post-image (full)
	# default:
	'' => 'Q XP S A XC XBODY XDFN XDHH XDCTX XDFA XDFB XPRE XPOST',
);

our @HELP = (
	's:' => 'match within message subject e.g. s:"a quick brown fox"',
	'ad:' => <<EOF,
Author date range as YYYYMMDD  e.g. ad:19931002..20101002
Open-ended ranges such as ad:19931002.. and ad:..20101002
are also supported
EOF
	'cd:' => 'Committer date range as YYYYMMDD, see ad: above',
	'b:' => 'match within commit message body',
	'bs:' => 'match within the commit message subject and body',
);
chomp @HELP;

my %all_pfx = (%bool_pfx_internal, %bool_pfx_external, %prob_prefix);

sub new {
	my ($class, $git_dir, $repo_dir) = @_;
	$repo_dir ||= "$git_dir/public-inbox";
	my $xdir = "$repo_dir/xr".REPO_SCHEMA_VERSION;
	bless { git_dir => $git_dir, xdir => $xdir }, $class;
}

# overriden by RepoGitSearchIdx
sub xdb ($) { $_[0]->{xdb} ||= Search::Xapian::Database->new($_[0]->{xdir}) }

sub retry_reopen ($$) {
	my ($self, $cb) = @_;
	my $ret;
	for (1..3) {
		eval { $ret = $cb->() };
		return $ret unless $@;
		# Exception: The revision being read has been discarded -
		# you should call Xapian::Database::reopen()
		if (ref($@) eq 'Search::Xapian::DatabaseModifiedError') {
			$self->{xdb}->reopen;
		} else {
			die;
		}
	}
}

sub _enquire_once ($$$) {
	my ($self, $query, $opts) = @_;
	my $enq = $self->{enquire} ||= Search::Xapian::Enquire->new($self->xdb);
	$enq->set_query($query);
	$opts ||= {};
        my $desc = !$opts->{asc};
	if ($opts->{relevance}) {
		$enq->set_sort_by_relevance_then_value(AD, $desc);
	} else {
		$enq->set_sort_by_value_then_relevance(AD, $desc);
	}
	my $offset = $opts->{offset} || 0;
	my $limit = $opts->{limit} || 50;
	$enq->get_mset($offset, $limit);
}

sub _do_enquire ($$$) {
	my ($self, $query, $opts) = @_;
	retry_reopen($self, sub { _enquire_once($self, $query, $opts) });
}

sub stemmer () { Search::Xapian::Stem->new($LANG) }

# read-only
sub qp ($) {
	my ($self) = @_;

	my $qp = $self->{query_parser};
	return $qp if $qp;

	# new parser
	$qp = Search::Xapian::QueryParser->new;
	$qp->set_default_op(OP_AND);
	$qp->set_database($self->xdb);
	$qp->set_stemmer(stemmer());
	$qp->set_stemming_strategy(STEM_SOME);

	$qp->add_valuerangeprocessor(
		Search::Xapian::NumberValueRangeProcessor->new(AD, 'ad:'));
	$qp->add_valuerangeprocessor(
		Search::Xapian::NumberValueRangeProcessor->new(CD, 'cd:'));

	while (my ($name, $prefix) = each %bool_pfx_external) {
		$qp->add_boolean_prefix($name, $prefix);
	}

	while (my ($name, $prefix) = each %prob_prefix) {
		$qp->add_prefix($name, $_) foreach split(/ /, $prefix);
	}

	$self->{query_parser} = $qp;
}

# returns begin and end PostingIterator
sub find_docids ($$) {
	my ($self, $termval) = @_;
	my $db = $self->xdb;
	($db->postlist_begin($termval), $db->postlist_end($termval));
}

sub find_unique_docid ($$$) {
	my ($self, $termval) = @_;
	my ($begin, $end) = find_docids($self, $termval);
	return undef if $begin->equal($end); # not found
	my $rv = $begin->get_docid;
	# sanity check
	$begin->inc;
	$begin->equal($end) or die "Term '$termval' is not unique\n";
	$rv;
}

sub help ($) {
	my ($self) = @_;
	\@HELP;
}

# read-only
sub query {
	my ($self, $query_string, $opts) = @_;
	my $query;

	$opts ||= {};
	unless ($query_string eq '') {
		$query = qp($self)->parse_query($query_string, QP_FLAGS);
		$opts->{relevance} = 1 unless exists $opts->{relevance};
	}

	_do_enquire($self, $query, $opts);
}

1;
