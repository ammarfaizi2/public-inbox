# Copyright (C) 2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# read-only counterpart to MiscIdx
package PublicInbox::MiscSearch;
use strict;
use v5.10.1;
use PublicInbox::Search qw(retry_reopen int_val);
my $json;

# Xapian value columns:
our $MODIFIED = 0;
our $UIDVALIDITY = 1; # (created time)

# avoid conflicting with message Search::prob_prefix for UI/UX reasons
my %PROB_PREFIX = (
	description => 'S', # $INBOX_DIR/description
	address => 'A',
	listid => 'XLISTID',
	url => 'XURL',
	infourl => 'XINFOURL',
	name => 'XNAME',
	'' => 'S A XLISTID XNAME XURL XINFOURL'
);

sub new {
	my ($class, $dir) = @_;
	PublicInbox::Search::load_xapian();
	$json //= PublicInbox::Config::json();
	bless {
		xdb => $PublicInbox::Search::X{Database}->new($dir)
	}, $class;
}

# read-only
sub mi_qp_new ($) {
	my ($self) = @_;
	my $xdb = $self->{xdb};
	my $qp = $PublicInbox::Search::X{QueryParser}->new;
	$qp->set_default_op(PublicInbox::Search::OP_AND());
	$qp->set_database($xdb);
	$qp->set_stemmer(PublicInbox::Search::stemmer($self));
	$qp->set_stemming_strategy(PublicInbox::Search::STEM_SOME());
	my $cb = $qp->can('set_max_wildcard_expansion') //
		$qp->can('set_max_expansion'); # Xapian 1.5.0+
	$cb->($qp, 100);
	$cb = $qp->can('add_valuerangeprocessor') //
		$qp->can('add_rangeprocessor'); # Xapian 1.5.0+
	while (my ($name, $prefix) = each %PROB_PREFIX) {
		$qp->add_prefix($name, $_) for split(/ /, $prefix);
	}
	$qp->add_boolean_prefix('type', 'T');
	$qp;
}

sub misc_enquire_once { # retry_reopen callback
	my ($self, $qr, $opt) = @_;
	my $eq = $PublicInbox::Search::X{Enquire}->new($self->{xdb});
	$eq->set_query($qr);
        my $desc = !$opt->{asc};
	my $rel = $opt->{relevance} // 0;
	if ($rel == -1) { # ORDER BY docid/UID
		$eq->set_docid_order($PublicInbox::Search::ENQ_ASCENDING);
		$eq->set_weighting_scheme($PublicInbox::Search::X{BoolWeight}->new);
	} elsif ($rel) {
		$eq->set_sort_by_relevance_then_value($MODIFIED, $desc);
	} else {
		$eq->set_sort_by_value_then_relevance($MODIFIED, $desc);
	}
	$eq->get_mset($opt->{offset} || 0, $opt->{limit} || 200);
}

sub mset {
	my ($self, $qs, $opt) = @_;
	$opt ||= {};
	my $qp = $self->{qp} //= mi_qp_new($self);
	$qs = 'type:inbox' if $qs eq '';
	my $qr = $qp->parse_query($qs, $PublicInbox::Search::QP_FLAGS);
	$opt->{relevance} = 1 unless exists $opt->{relevance};
	retry_reopen($self, \&misc_enquire_once, $qr, $opt);
}

sub ibx_matches_once { # retry_reopen callback
	my ($self, $qr, $by_newsgroup) = @_;
	# double in case no newsgroups are configured:
	my $limit = scalar(keys %$by_newsgroup) * 2;
	my $opt = { limit => $limit, offset => 0, relevance => -1 };
	my $ret = {}; # newsgroup => $ibx of matches
	while (1) {
		my $mset = misc_enquire_once($self, $qr, $opt);
		for my $mi ($mset->items) {
			my $doc = $mi->get_document;
			my $end = $doc->termlist_end;
			my $cur = $doc->termlist_begin;
			$cur->skip_to('Q');
			if ($cur != $end) {
				my $ng = $cur->get_termname; # eidx_key
				$ng =~ s/\AQ// or warn "BUG: no `Q': $ng";
				if (my $ibx = $by_newsgroup->{$ng}) {
					$ret->{$ng} = $ibx;
				}
			} else {
				warn <<EOF;
W: docid=${\$mi->get_docid} has no `Q' (eidx_key) term
EOF
			}
		}
		my $nr = $mset->size;
		return $ret if $nr < $limit;
		$opt->{offset} += $nr;
	}
}

# returns a newsgroup => PublicInbox::Inbox mapping
sub newsgroup_matches {
	my ($self, $qs, $pi_cfg) = @_;
	my $qp = $self->{qp} //= mi_qp_new($self);
	$qs .= ' type:inbox';
	my $qr = $qp->parse_query($qs, $PublicInbox::Search::QP_FLAGS);
	retry_reopen($self, \&ibx_matches_once, $qr, $pi_cfg->{-by_newsgroup});
}

sub ibx_data_once {
	my ($self, $ibx) = @_;
	my $xdb = $self->{xdb};
	my $term = 'Q'.$ibx->eidx_key; # may be {inboxdir}, so private
	my $head = $xdb->postlist_begin($term);
	my $tail = $xdb->postlist_end($term);
	if ($head != $tail) {
		my $doc = $xdb->get_document($head->get_docid);
		$ibx->{uidvalidity} //= int_val($doc, $UIDVALIDITY);
		$ibx->{-modified} = int_val($doc, $MODIFIED);
		$doc->get_data;
	} else {
		undef;
	}
}

sub inbox_data {
	my ($self, $ibx) = @_;
	retry_reopen($self, \&ibx_data_once, $ibx);
}

sub ibx_cache_load {
	my ($doc, $cache) = @_;
	my $end = $doc->termlist_end;
	my $cur = $doc->termlist_begin;
	$cur->skip_to('Q');
	return if $cur == $end;
	my $eidx_key = $cur->get_termname;
	$eidx_key =~ s/\AQ// or return; # expired
	my $ce = $cache->{$eidx_key} = {};
	$ce->{uidvalidity} = int_val($doc, $UIDVALIDITY);
	$ce->{-modified} = int_val($doc, $MODIFIED);
	$ce->{description} = do {
		# extract description from manifest.js.gz epoch description
		my $d;
		my $data = $json->decode($doc->get_data);
		for (values %$data) {
			$d = $_->{description} // next;
			$d =~ s/ \[epoch [0-9]+\]\z// or next;
			last;
		}
		$d;
	}
}

sub _nntpd_cache_load { # retry_reopen callback
	my ($self) = @_;
	my $opt = { limit => $self->{xdb}->get_doccount * 10, relevance => -1 };
	my $mset = mset($self, 'type:newsgroup type:inbox', $opt);
	my $cache = {};
	for my $it ($mset->items) {
		ibx_cache_load($it->get_document, $cache);
	}
	$cache
}

# returns { newsgroup => $cache_entry } mapping, $cache_entry contains
# anything which may trigger seeks at startup, currently: description,
# -modified, and uidvalidity.
sub nntpd_cache_load {
	my ($self) = @_;
	retry_reopen($self, \&_nntpd_cache_load);
}

1;
