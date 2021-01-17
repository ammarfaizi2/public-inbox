# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Combine any combination of PublicInbox::Search,
# PublicInbox::ExtSearch, and PublicInbox::LeiSearch objects
# into one Xapian DB
package PublicInbox::LeiXSearch;
use strict;
use v5.10.1;
use parent qw(PublicInbox::LeiSearch PublicInbox::IPC);
use PublicInbox::DS qw(dwaitpid);
use PublicInbox::OpPipe;
use PublicInbox::Import;
use File::Temp 0.19 (); # 0.19 for ->newdir
use File::Spec ();

sub new {
	my ($class) = @_;
	PublicInbox::Search::load_xapian();
	bless {
		qp_flags => $PublicInbox::Search::QP_FLAGS |
				PublicInbox::Search::FLAG_PURE_NOT(),
	}, $class
}

sub attach_external {
	my ($self, $ibxish) = @_; # ibxish = ExtSearch or Inbox

	if (!$ibxish->can('over') || !$ibxish->over) {
		return push(@{$self->{remotes}}, $ibxish)
	}
	my $desc = $ibxish->{inboxdir} // $ibxish->{topdir};
	my $srch = $ibxish->search or
		return warn("$desc not indexed for Xapian\n");
	my @shards = $srch->xdb_shards_flat or
		return warn("$desc has no Xapian shardsXapian\n");

	if (delete $self->{xdb}) { # XXX: do we need this?
		# clobber existing {xdb} if amending
		my $expect = delete $self->{nshard};
		my $shards = delete $self->{shards_flat};
		scalar(@$shards) == $expect or die
			"BUG: {nshard}$expect != shards=".scalar(@$shards);

		my $prev = {};
		for my $old_ibxish (@{$self->{shard2ibx}}) {
			next if $prev == $old_ibxish;
			$prev = $old_ibxish;
			my @shards = $old_ibxish->search->xdb_shards_flat;
			push @{$self->{shards_flat}}, @shards;
		}
		my $nr = scalar(@{$self->{shards_flat}});
		$nr == $expect or die
			"BUG: reloaded $nr shards, expected $expect"
	}
	push @{$self->{shards_flat}}, @shards;
	push(@{$self->{shard2ibx}}, $ibxish) for (@shards);
}

# returns a list of local inboxes (or count in scalar context)
sub locals {
	my %uniq = map {; "$_" => $_ } @{$_[0]->{shard2ibx} // []};
	values %uniq;
}

# called by PublicInbox::Search::xdb
sub xdb_shards_flat { @{$_[0]->{shards_flat} // []} }

# like over->get_art
sub smsg_for {
	my ($self, $mitem) = @_;
	# cf. https://trac.xapian.org/wiki/FAQ/MultiDatabaseDocumentID
	my $nshard = $self->{nshard};
	my $docid = $mitem->get_docid;
	my $shard = ($docid - 1) % $nshard;
	my $num = int(($docid - 1) / $nshard) + 1;
	my $smsg = $self->{shard2ibx}->[$shard]->over->get_art($num);
	$smsg->{docid} = $docid;
	$smsg;
}

sub recent {
	my ($self, $qstr, $opt) = @_;
	$opt //= {};
	$opt->{relevance} //= -2;
	$self->mset($qstr //= 'bytes:1..', $opt);
}

sub over {}

sub _mset_more ($$) {
	my ($mset, $mo) = @_;
	my $size = $mset->size;
	$size && (($mo->{offset} += $size) < ($mo->{limit} // 10000));
}

sub query_thread_mset { # for --thread
	my ($self, $lei, $ibxish) = @_;
	my %sig = $lei->atfork_child_wq($self);
	local @SIG{keys %sig} = values %sig;

	my ($srch, $over) = ($ibxish->search, $ibxish->over);
	unless ($srch && $over) {
		my $desc = $ibxish->{inboxdir} // $ibxish->{topdir};
		warn "$desc not indexed by Xapian\n";
		return;
	}
	my $mo = { %{$lei->{mset_opt}} };
	my $mset;
	my $each_smsg = $lei->{ovv}->ovv_each_smsg_cb($lei, $ibxish);
	my $dedupe = $lei->{dedupe} // die 'BUG: {dedupe} missing';
	$dedupe->prepare_dedupe;
	do {
		$mset = $srch->mset($mo->{qstr}, $mo);
		my $ids = $srch->mset_to_artnums($mset, $mo);
		my $ctx = { ids => $ids };
		my $i = 0;
		my %n2item = map { ($ids->[$i++], $_) } $mset->items;
		while ($over->expand_thread($ctx)) {
			for my $n (@{$ctx->{xids}}) {
				my $smsg = $over->get_art($n) or next;
				next if $dedupe->is_smsg_dup($smsg);
				my $mitem = delete $n2item{$smsg->{num}};
				$each_smsg->($smsg, $mitem);
			}
			@{$ctx->{xids}} = ();
		}
	} while (_mset_more($mset, $mo));
	$lei->{ovv}->ovv_atexit_child($lei);
}

sub query_mset { # non-parallel for non-"--thread" users
	my ($self, $lei, $srcs) = @_;
	my %sig = $lei->atfork_child_wq($self);
	local @SIG{keys %sig} = values %sig;
	my $mo = { %{$lei->{mset_opt}} };
	my $mset;
	$self->attach_external($_) for @$srcs;
	my $each_smsg = $lei->{ovv}->ovv_each_smsg_cb($lei, $self);
	my $dedupe = $lei->{dedupe} // die 'BUG: {dedupe} missing';
	$dedupe->prepare_dedupe;
	do {
		$mset = $self->mset($mo->{qstr}, $mo);
		for my $it ($mset->items) {
			my $smsg = smsg_for($self, $it) or next;
			next if $dedupe->is_smsg_dup($smsg);
			$each_smsg->($smsg, $it);
		}
	} while (_mset_more($mset, $mo));
	$lei->{ovv}->ovv_atexit_child($lei);
}

sub git {
	my ($self) = @_;
	my (%seen, @dirs);
	my $tmp = File::Temp->newdir('lei_xsrch_git-XXXXXXXX', TMPDIR => 1);
	for my $ibx (@{$self->{shard2ibx} // []}) {
		my $d = File::Spec->canonpath($ibx->git->{git_dir});
		$seen{$d} //= push @dirs, "$d/objects\n"
	}
	my $git_dir = $tmp->dirname;
	PublicInbox::Import::init_bare($git_dir);
	my $f = "$git_dir/objects/info/alternates";
	open my $alt, '>', $f or die "open($f): $!";
	print $alt @dirs or die "print $f: $!";
	close $alt or die "close $f: $!";
	my $git = PublicInbox::Git->new($git_dir);
	$git->{-tmp} = $tmp;
	$git;
}

sub query_done { # EOF callback
	my ($lei) = @_;
	$lei->{ovv}->ovv_end($lei);
	if (my $l2m = $lei->{l2m}) {
		$lei->start_mua unless $l2m->lock_free;
	}
	$lei->dclose;
}

sub start_query { # always runs in main (lei-daemon) process
	my ($self, $io, $lei, $srcs) = @_;
	if (my $l2m = $lei->{l2m}) {
		$lei->{1} = $io->[1];
		$l2m->post_augment($lei);
		$io->[1] = delete $lei->{1};
		$lei->start_mua($io->[3]) if $l2m->lock_free;
	}
	my $remotes = $self->{remotes} // [];
	if ($lei->{opt}->{thread}) {
		$lei->{-parallel} = scalar(@$remotes) + scalar(@$srcs) - 1;
		for my $ibxish (@$srcs) {
			$self->wq_do('query_thread_mset', $io, $lei, $ibxish);
		}
	} else {
		$lei->{-parallel} = scalar(@$remotes);
		$self->wq_do('query_mset', $io, $lei, $srcs);
	}
	# TODO
	for my $rmt (@$remotes) {
		$self->wq_do('query_thread_mbox', $io, $lei, $rmt);
	}
	close $io->[0]; # qry_status_wr
	@$io = ();
}

sub query_prepare { # wq_do
	my ($self, $lei) = @_;
	my %sig = $lei->atfork_child_wq($self);
	local @SIG{keys %sig} = values %sig;
	if (my $l2m = $lei->{l2m}) {
		eval { $l2m->do_augment($lei) };
		return $lei->fail($@) if $@;
	}
	# trigger PublicInbox::OpPipe->event_step
	my $qry_status_wr = $lei->{0} or
		return $lei->fail('BUG: qry_status_wr missing');
	$qry_status_wr->autoflush(1);
	print $qry_status_wr '.' or # this should never fail...
		return $lei->fail("BUG? print qry_status_wr: $!");
}

sub do_query {
	my ($self, $lei_orig, $srcs) = @_;
	my ($lei, @io) = $lei_orig->atfork_parent_wq($self);
	$io[0] = undef;
	pipe(my $qry_status_rd, $io[0]) or die "pipe $!";

	$lei_orig->{lxs} = $self;
	$lei_orig->event_step_init; # wait for shutdowns
	my $op_map = { '' => [ \&query_done, $lei_orig ] };
	my $in_loop = exists $lei_orig->{sock};
	my $opp = PublicInbox::OpPipe->new($qry_status_rd, $op_map, $in_loop);
	if (my $l2m = $lei->{l2m}) {
		$l2m->pre_augment($lei_orig); # may redirect $lei->{1} for mbox
		$io[1] = $lei_orig->{1};
		$op_map->{'.'} = [ \&start_query, $self, \@io, $lei, $srcs ];
		$self->wq_do('query_prepare', \@io, $lei);
		$opp->event_step if !$in_loop;
	} else {
		start_query($self, \@io, $lei, $srcs);
	}
	unless ($in_loop) {
		my @pids = $self->wq_close;
		# for the $lei->atfork_child_wq PIPE handler:
		$op_map->{'!'} = [ \&CORE::kill, 'TERM', @pids ];
		$opp->event_step;
		my $ipc_worker_reap = $self->can('ipc_worker_reap');
		dwaitpid($_, $ipc_worker_reap, $self) for @pids;
	}
}

sub ipc_atfork_prepare {
	my ($self) = @_;
	$self->wq_set_recv_modes(qw[+<&= >&= >&= +<&=]);
	$self->SUPER::ipc_atfork_prepare; # PublicInbox::IPC
}

1;
