# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Intended for PublicInbox::DS::event_loop for -cindex --associate
# Iterating through mset->items is slow in Perl due to method dispatch
# and that loop may implemented in C++ using Xapian directly
package PublicInbox::CidxDumpIbx;
use v5.12;
use PublicInbox::Search qw(xap_terms);
use PublicInbox::DS;
use Socket qw(MSG_EOR);

sub start {
	my ($rcvibx, $ibx_id) = @_;
	my $cidx = $rcvibx->{cidx};
	my $ibx = $cidx->{IBX}->[$ibx_id] // die "BUG: no IBX[$ibx_id]";
	my $self = bless { rcvibx => $rcvibx, ekey => $ibx->eidx_key,
		ibx_id => $ibx_id }, __PACKAGE__;
	$self->{srch} = $ibx->isrch // do {
		warn("W: $self->{ekey} has no search index (ignoring)\n");
		return undef;
	};
	my $opt = { limit => $cidx->assoc_max_init, relevance => -2 };
	$self->{mset} = $self->{srch}->mset($rcvibx->{qry_str}, $opt);
	$self->{iter} = 0;
	event_step($self);
}

sub event_step {
	my ($self) = @_;
	my $rcvibx = $self->{rcvibx} // die 'BUG: no rcvibx';
	return if $rcvibx->{cidx}->do_quit;
	my $last = $self->{mset}->size - 1;
	my $cur = $self->{iter};
	my $end = $cur + 9999;
	if ($end >= $last) {
		send($rcvibx->{op_p}, 'index_next', MSG_EOR);
		$end = $last;
	}
	$self->{iter} = $end + 1;
	local $0 = "dumping $self->{ekey} $cur..$end";

	my $sort_w = $rcvibx->{sort_w};
	my $ibx_id = $self->{ibx_id};
	local $0 = "dumping $self->{ekey} $cur..$end";
	$rcvibx->{cidx}->progress($0);
	for my $x (($self->{mset}->items)[$cur..$end]) { # FIXME: slow loop
		my $doc = $x->get_document;
		for my $p (@{$rcvibx->{cidx}->{ASSOC_PFX}}) {
			for (xap_terms($p, $doc)) {
				print $sort_w "$_ $ibx_id\n" or die "print: $!";
			}
		}
	}
	$end < $last && !$rcvibx->{cidx}->do_quit and
		PublicInbox::DS::requeue($self);
}

1;
