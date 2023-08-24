# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Intended for PublicInbox::DS::event_loop for -cindex --associate
# Iterating through mset->items is slow in Perl due to method dispatch
# and that loop may implemented in C++ using Xapian directly
package PublicInbox::CidxDumpShardRoots;
use v5.12;
use PublicInbox::Lock;
use PublicInbox::Search qw(xap_terms);
use Socket qw(MSG_EOR);

sub start {
	my ($cidx, $root2id, $qry_str) = @_;
	my $op_p = delete($cidx->{0}) // die 'BUG: no {0} op_p';
	my $sort_w = delete($cidx->{1}) // die 'BUG: no {1} $w sort pipe';
	# sort lock is necessary if we have may root ids which cause a
	# row length to exceed POSIX PIPE_BUF (via `$G' below)
	my $sort_lk = bless { lock_path => $cidx->tmpdir.'/to_root_id.lock' },
		'PublicInbox::Lock';
	$sort_w->autoflush(1);
	$cidx->begin_txn_lazy; # only using txn to simplify writer subs
	my $opt = { limit => $cidx->assoc_max_init, relevance => -2 };
	my $self = bless {
		cidx => $cidx,
		op_p => $op_p,
		iter => 0,
		mset => $cidx->mset($qry_str, $opt),
		root2id => $root2id,
		sort_w => $sort_w,
		sort_lk => $sort_lk,
	}, __PACKAGE__;
	event_step($self);
}

sub event_step {
	my ($self) = @_;
	my $cidx = $self->{cidx};
	return if $cidx->do_quit;
	my $last = $self->{mset}->size - 1;
	my $cur = $self->{iter};
	my $end = $cur + 9999;
	$end = $last if $end > $last;
	$self->{iter} = $end + 1;
	local $0 = "dumping shard [$cidx->{shard}] $cur..$end";
	$cidx->progress($0);

	my $root2id = $self->{root2id};
	my $buf = '';
	for my $x (($self->{mset}->items)[$cur..$end]) { # FIXME: slow loop
		my $doc = $x->get_document;
		my $G = join(' ', map {
			$root2id->{pack('H*', $_)};
		} xap_terms('G', $doc));
		for my $p (@{$cidx->{ASSOC_PFX}}) {
			$buf .= "$_ $G\n" for (xap_terms($p, $doc));
		}
	}
	$self->{sort_lk}->lock_acquire_fast;
	print { $self->{sort_w} } $buf or die "print: $!";
	$self->{sort_lk}->lock_release_fast;
	$end < $last && !$cidx->do_quit and
		PublicInbox::DS::requeue($self);
}

sub DESTROY {
	my ($self) = @_;
	return if $self->{cidx}->do_quit;
	send($self->{op_p},
		"dump_shard_roots_done $self->{cidx}->{shard}", MSG_EOR);
}

1;
