# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Combine any combination of PublicInbox::Search,
# PublicInbox::ExtSearch, and PublicInbox::LeiSearch objects
# into one Xapian DB
package PublicInbox::LeiXSearch;
use strict;
use v5.10.1;
use parent qw(PublicInbox::LeiSearch PublicInbox::IPC);
use PublicInbox::DS qw(now);
use PublicInbox::PktOp qw(pkt_do);
use File::Temp 0.19 (); # 0.19 for ->newdir
use File::Spec ();
use PublicInbox::Search qw(xap_terms);
use PublicInbox::Spawn qw(popen_rd spawn which);
use PublicInbox::MID qw(mids);
use PublicInbox::Smsg;
use PublicInbox::Eml;
use Fcntl qw(SEEK_SET F_SETFL O_APPEND O_RDWR);
use PublicInbox::ContentHash qw(git_sha);

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
	my $desc = $ibxish->{inboxdir} // $ibxish->{topdir};
	my $srch = $ibxish->search or
		return warn("$desc not indexed for Xapian\n");
	my @shards = $srch->xdb_shards_flat or
		return warn("$desc has no Xapian shards\n");

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
sub locals { @{$_[0]->{locals} // []} }

sub remotes { @{$_[0]->{remotes} // []} }

# called by PublicInbox::Search::xdb (usually via ->mset)
sub xdb_shards_flat { @{$_[0]->{shards_flat} // []} }

sub mitem_kw ($$;$) {
	my ($smsg, $mitem, $flagged) = @_;
	my $kw = xap_terms('K', my $doc = $mitem->get_document);
	$kw->{flagged} = 1 if $flagged;
	# we keep the empty {kw} array here to prevent expensive work in
	# ->xsmsg_vmd, _unbless_smsg will clobber it iff it's empty
	$smsg->{kw} = [ sort keys %$kw ];
	my $L = xap_terms('L', $doc);
	$smsg->{L} = [ sort keys %$L ] if scalar(keys %$L);
}

# like over->get_art
sub smsg_for {
	my ($self, $mitem) = @_;
	# cf. https://trac.xapian.org/wiki/FAQ/MultiDatabaseDocumentID
	my $nshard = $self->{nshard};
	my $docid = $mitem->get_docid;
	my $shard = ($docid - 1) % $nshard;
	my $num = int(($docid - 1) / $nshard) + 1;
	my $ibx = $self->{shard2ibx}->[$shard];
	my $smsg = $ibx->over->get_art($num);
	return if $smsg->{bytes} == 0; # external message
	if ($ibx->can('msg_keywords')) {
		mitem_kw($smsg, $mitem);
	}
	$smsg;
}

sub recent {
	my ($self, $qstr, $opt) = @_;
	$opt //= {};
	$opt->{relevance} //= -2;
	$self->mset($qstr //= 'z:1..', $opt);
}

sub over {}

sub _mset_more ($$) {
	my ($mset, $mo) = @_;
	my $size = $mset->size;
	$size >= $mo->{limit} && (($mo->{offset} += $size) < $mo->{limit});
}

# $startq will EOF when do_augment is done augmenting and allow
# query_combined_mset and query_thread_mset to proceed.
sub wait_startq ($) {
	my ($lei) = @_;
	my $startq = delete $lei->{startq} or return;
	while (1) {
		my $n = sysread($startq, my $do_augment_done, 1);
		if (defined $n) {
			return if $n == 0; # no MUA
			if ($do_augment_done eq 'q') {
				$lei->{opt}->{quiet} = 1;
				delete $lei->{opt}->{verbose};
				delete $lei->{-progress};
			} else {
				$lei->fail("$$ WTF `$do_augment_done'");
			}
			return;
		}
		return $lei->fail("$$ wait_startq: $!") unless $!{EINTR};
	}
}

sub mset_progress {
	my $lei = shift;
	return if $lei->{early_mua} || !$lei->{-progress};
	if ($lei->{pkt_op_p}) {
		pkt_do($lei->{pkt_op_p}, 'mset_progress', @_);
	} else { # single lei-daemon consumer
		my ($desc, $mset_size, $mset_total_est) = @_;
		$lei->{-mset_total} += $mset_size if $mset_total_est ne '?';
		$lei->qerr("# $desc $mset_size/$mset_total_est");
	}
}

sub l2m_progress {
	my ($lei, $nr) = @_;
	$lei->{-nr_write} += $nr;
}

sub query_one_mset { # for --threads and l2m w/o sort
	my ($self, $ibxish) = @_;
	local $0 = "$0 query_one_mset";
	my $lei = $self->{lei};
	my ($srch, $over) = ($ibxish->search, $ibxish->over);
	my $dir = $ibxish->{inboxdir} // $ibxish->{topdir};
	return warn("$dir not indexed by Xapian\n") unless ($srch && $over);
	my $mo = { %{$lei->{mset_opt}} }; # copy
	my $mset;
	my $each_smsg = $lei->{ovv}->ovv_each_smsg_cb($lei);
	my $can_kw = !!$ibxish->can('msg_keywords');
	my $threads = $lei->{opt}->{threads} // 0;
	my $fl = $threads > 1 ? 1 : undef;
	my $lss = $lei->{dedupe};
	$lss = undef unless $lss && $lss->can('cfg_set'); # saved search
	my $maxk = "external.$dir.maxuid";
	my $stop_at = $lss ? $lss->{-cfg}->{$maxk} : undef;
	if (defined $stop_at) {
		die "$maxk=$stop_at has multiple values" if ref $stop_at;
		my @e;
		local $SIG{__WARN__} = sub { push @e, @_ };
		$stop_at += 0;
		return warn("$maxk=$stop_at: @e") if @e;
	}
	my $first_ids;
	do {
		$mset = $srch->mset($mo->{qstr}, $mo);
		mset_progress($lei, $dir, $mset->size,
				$mset->get_matches_estimated);
		wait_startq($lei); # wait for keyword updates
		my $ids = $srch->mset_to_artnums($mset, $mo);
		@$ids = grep { $_ > $stop_at } @$ids if defined($stop_at);
		my $i = 0;
		if ($threads) {
			# copy $ids if $lss since over->expand_thread
			# shifts @{$ctx->{ids}}
			$first_ids = [ @$ids ] if $lss;
			my $ctx = { ids => $ids };
			my %n2item = map { ($ids->[$i++], $_) } $mset->items;
			while ($over->expand_thread($ctx)) {
				for my $n (@{$ctx->{xids}}) {
					my $smsg = $over->get_art($n) or next;
					my $mitem = delete $n2item{$n};
					next if $smsg->{bytes} == 0;
					if ($mitem && $can_kw) {
						mitem_kw($smsg, $mitem, $fl);
					} elsif ($mitem && $fl) {
						# call ->xsmsg_vmd, later
						$smsg->{lei_q_tt_flagged} = 1;
					}
					$each_smsg->($smsg, $mitem);
				}
				@{$ctx->{xids}} = ();
			}
		} else {
			$first_ids = $ids;
			my @items = $mset->items;
			for my $n (@$ids) {
				my $mitem = $items[$i++];
				my $smsg = $over->get_art($n) or next;
				next if $smsg->{bytes} == 0;
				mitem_kw($smsg, $mitem, $fl) if $can_kw;
				$each_smsg->($smsg, $mitem);
			}
		}
	} while (_mset_more($mset, $mo));
	if ($lss && scalar(@$first_ids)) {
		undef $stop_at;
		my $max = $first_ids->[0];
		$lss->cfg_set($maxk, $max);
		undef $lss;
	}
	undef $each_smsg; # may commit
	$lei->{ovv}->ovv_atexit_child($lei);
}

sub query_combined_mset { # non-parallel for non-"--threads" users
	my ($self) = @_;
	local $0 = "$0 query_combined_mset";
	my $lei = $self->{lei};
	my $mo = { %{$lei->{mset_opt}} };
	my $mset;
	for my $loc (locals($self)) {
		attach_external($self, $loc);
	}
	my $each_smsg = $lei->{ovv}->ovv_each_smsg_cb($lei);
	do {
		$mset = $self->mset($mo->{qstr}, $mo);
		mset_progress($lei, 'xsearch', $mset->size,
				$mset->get_matches_estimated);
		wait_startq($lei); # wait for keyword updates
		for my $mitem ($mset->items) {
			my $smsg = smsg_for($self, $mitem) or next;
			$each_smsg->($smsg, $mitem);
		}
	} while (_mset_more($mset, $mo));
	undef $each_smsg; # may commit
	$lei->{ovv}->ovv_atexit_child($lei);
}

sub each_remote_eml { # callback for MboxReader->mboxrd
	my ($eml, $self, $lei, $each_smsg) = @_;
	my $xoids = $lei->{ale}->xoids_for($eml, 1);
	if ($self->{import_sto} && !$xoids) {
		$self->{import_sto}->ipc_do('add_eml', $eml);
	}
	my $smsg = bless {}, 'PublicInbox::Smsg';
	$smsg->{blob} = $xoids ? (keys(%$xoids))[0]
				: git_sha(1, $eml)->hexdigest;
	$smsg->populate($eml);
	$smsg->parse_references($eml, mids($eml));
	$smsg->{$_} //= '' for qw(from to cc ds subject references mid);
	delete @$smsg{qw(From Subject -ds -ts)};
	wait_startq($lei);
	if ($lei->{-progress}) {
		++$lei->{-nr_remote_eml};
		my $now = now();
		my $next = $lei->{-next_progress} //= ($now + 1);
		if ($now > $next) {
			$lei->{-next_progress} = $now + 1;
			my $nr = $lei->{-nr_remote_eml};
			mset_progress($lei, $lei->{-current_url}, $nr, '?');
		}
	}
	$each_smsg->($smsg, undef, $eml);
}

sub query_remote_mboxrd {
	my ($self, $uris) = @_;
	local $0 = "$0 query_remote_mboxrd";
	local $SIG{TERM} = sub { exit(0) }; # for DESTROY (File::Temp, $reap)
	my $lei = $self->{lei};
	my $opt = $lei->{opt};
	my @qform = (q => $lei->{mset_opt}->{qstr}, x => 'm');
	push(@qform, t => 1) if $opt->{threads};
	my $verbose = $opt->{verbose};
	my ($reap_tail, $reap_curl);
	my $cerr = File::Temp->new(TEMPLATE => 'curl.err-XXXX', TMPDIR => 1);
	fcntl($cerr, F_SETFL, O_APPEND|O_RDWR) or warn "set O_APPEND: $!";
	my $rdr = { 2 => $cerr, pgid => 0 };
	my $sigint_reap = $lei->can('sigint_reap');
	if ($verbose) {
		# spawn a process to force line-buffering, otherwise curl
		# will write 1 character at-a-time and parallel outputs
		# mmmaaayyy llloookkk llliiikkkeee ttthhhiiisss
		my $o = { 1 => $lei->{2}, 2 => $lei->{2}, pgid => 0 };
		my $pid = spawn(['tail', '-f', $cerr->filename], undef, $o);
		$reap_tail = PublicInbox::OnDestroy->new($sigint_reap, $pid);
	}
	my $curl = PublicInbox::LeiCurl->new($lei, $self->{curl}) or return;
	push @$curl, '-s', '-d', '';
	my $each_smsg = $lei->{ovv}->ovv_each_smsg_cb($lei);
	$self->{import_sto} = $lei->{sto} if $lei->{opt}->{'import-remote'};
	for my $uri (@$uris) {
		$lei->{-current_url} = $uri->as_string;
		$lei->{-nr_remote_eml} = 0;
		$uri->query_form(@qform);
		my $cmd = $curl->for_uri($lei, $uri);
		$lei->qerr("# $cmd");
		my ($fh, $pid) = popen_rd($cmd, undef, $rdr);
		$reap_curl = PublicInbox::OnDestroy->new($sigint_reap, $pid);
		$fh = IO::Uncompress::Gunzip->new($fh, MultiStream => 1);
		PublicInbox::MboxReader->mboxrd($fh, \&each_remote_eml, $self,
						$lei, $each_smsg);
		my $err = waitpid($pid, 0) == $pid ? undef
						: "BUG: waitpid($cmd): $!";
		@$reap_curl = (); # cancel OnDestroy
		die $err if $err;
		my $nr = $lei->{-nr_remote_eml};
		if ($nr && $lei->{sto}) {
			my $wait = $lei->{sto}->ipc_do('done');
		}
		if ($? == 0) {
			mset_progress($lei, $lei->{-current_url}, $nr, $nr);
			next;
		}
		$err = '';
		if (-s $cerr) {
			seek($cerr, 0, SEEK_SET) or
					$lei->err("seek($cmd stderr): $!");
			$err = do { local $/; <$cerr> } //
					"read($cmd stderr): $!";
			truncate($cerr, 0) or
					$lei->err("truncate($cmd stderr): $!");
		}
		next if (($? >> 8) == 22 && $err =~ /\b404\b/);
		$uri->query_form(q => $lei->{mset_opt}->{qstr});
		$lei->child_error($?, "E: <$uri> $err");
	}
	undef $each_smsg;
	$lei->{ovv}->ovv_atexit_child($lei);
}

sub git { $_[0]->{git} // die 'BUG: git uninitialized' }

sub xsearch_done_wait { # dwaitpid callback
	my ($arg, $pid) = @_;
	my ($wq, $lei) = @$arg;
	$lei->child_error($?, 'non-fatal error from '.ref($wq)) if $?;
}

sub query_done { # EOF callback for main daemon
	my ($lei) = @_;
	my $l2m = delete $lei->{l2m};
	$l2m->wq_wait_old(\&xsearch_done_wait, $lei) if $l2m;
	if (my $lxs = delete $lei->{lxs}) {
		$lxs->wq_wait_old(\&xsearch_done_wait, $lei);
	}
	my $wait = $lei->{sto} ? $lei->{sto}->ipc_do('done') : undef;
	$lei->{ovv}->ovv_end($lei);
	my $start_mua;
	if ($l2m) { # close() calls LeiToMail reap_compress
		if (my $out = delete $lei->{old_1}) {
			if (my $mbout = $lei->{1}) {
				close($mbout) or return $lei->fail(<<"");
Error closing $lei->{ovv}->{dst}: $!

			}
			$lei->{1} = $out;
		}
		if ($l2m->lock_free) {
			$l2m->poke_dst;
			$lei->poke_mua;
		} else { # mbox users
			delete $l2m->{mbl}; # drop dotlock
			$start_mua = 1;
		}
	}
	if ($lei->{-progress}) {
		my $tot = $lei->{-mset_total} // 0;
		my $nr = $lei->{-nr_write} // 0;
		$lei->qerr($l2m ?
			"# $nr written to $lei->{ovv}->{dst} ($tot matches)" :
			"# $tot matches");
	}
	$lei->start_mua if $start_mua;
	$lei->dclose;
}

sub do_post_augment {
	my ($lei) = @_;
	my $l2m = $lei->{l2m} or return; # client disconnected
	my $err;
	eval { $l2m->post_augment($lei) };
	$err = $@;
	if ($err) {
		if (my $lxs = delete $lei->{lxs}) {
			$lxs->wq_kill;
			$lxs->wq_close(0, undef, $lei);
		}
		$lei->fail("$err");
	}
	if (!$err && delete $lei->{early_mua}) { # non-augment case
		$lei->start_mua;
	}
	close(delete $lei->{au_done}); # triggers wait_startq in lei_xsearch
}

sub incr_post_augment { # called whenever an l2m shard finishes augment
	my ($lei) = @_;
	my $l2m = $lei->{l2m} or return; # client disconnected
	return if ++$lei->{nr_post_augment} != $l2m->{-wq_nr_workers};
	do_post_augment($lei);
}

my $MAX_PER_HOST = 4;

sub concurrency {
	my ($self, $opt) = @_;
	my $nl = $opt->{threads} ? locals($self) : 1;
	my $nr = remotes($self);
	$nr = $MAX_PER_HOST if $nr > $MAX_PER_HOST;
	$nl + $nr;
}

sub start_query ($;$) { # always runs in main (lei-daemon) process
	my ($self, $l2m) = @_;
	if ($self->{opt_threads} || ($l2m && !$self->{opt_sort})) {
		for my $ibxish (locals($self)) {
			$self->wq_io_do('query_one_mset', [], $ibxish);
		}
	} elsif (locals($self)) {
		$self->wq_io_do('query_combined_mset', []);
	}
	my $i = 0;
	my $q = [];
	for my $uri (remotes($self)) {
		push @{$q->[$i++ % $MAX_PER_HOST]}, $uri;
	}
	for my $uris (@$q) {
		$self->wq_io_do('query_remote_mboxrd', [], $uris);
	}
	$self->wq_close(1); # lei_xsearch workers stop when done
}

sub incr_start_query { # called whenever an l2m shard starts do_post_auth
	my ($self, $l2m) = @_;
	return if ++$self->{nr_start_query} != $l2m->{-wq_nr_workers};
	start_query($self, $l2m);
}

sub ipc_atfork_child {
	my ($self) = @_;
	$self->{lei}->_lei_atfork_child;
	$self->SUPER::ipc_atfork_child;
}

sub do_query {
	my ($self, $lei) = @_;
	my $l2m = $lei->{l2m};
	my $ops = {
		'|' => [ $lei->can('sigpipe_handler'), $lei ],
		'!' => [ $lei->can('fail_handler'), $lei ],
		'.' => [ \&do_post_augment, $lei ],
		'+' => [ \&incr_post_augment, $lei ],
		'' => [ \&query_done, $lei ],
		'mset_progress' => [ \&mset_progress, $lei ],
		'l2m_progress' => [ \&l2m_progress, $lei ],
		'x_it' => [ $lei->can('x_it'), $lei ],
		'child_error' => [ $lei->can('child_error'), $lei ],
		'incr_start_query' => [ \&incr_start_query, $self, $l2m ],
	};
	$lei->{auth}->op_merge($ops, $l2m) if $l2m && $lei->{auth};
	my $end = $lei->pkt_op_pair;
	$lei->{1}->autoflush(1);
	$lei->start_pager if delete $lei->{need_pager};
	$lei->{ovv}->ovv_begin($lei);
	die 'BUG: xdb|over open' if $lei->{lse}->{xdb} || $lei->{lse}->{over};
	if ($l2m) {
		$l2m->pre_augment($lei);
		if ($lei->{opt}->{augment} && delete $lei->{early_mua}) {
			$lei->start_mua;
		}
		my $F_SETPIPE_SZ = $^O eq 'linux' ? 1031 : undef;
		if ($l2m->{-wq_nr_workers} > 1 &&
				$l2m->{base_type} =~ /\A(?:maildir|mbox)\z/) {
			# setup two barriers to coordinate dedupe_nr
			# between l2m workers
			pipe(my ($a_r, $a_w)) or die "pipe: $!";
			fcntl($a_r, $F_SETPIPE_SZ, 4096) if $F_SETPIPE_SZ;
			pipe(my ($b_r, $b_w)) or die "pipe: $!";
			fcntl($b_r, $F_SETPIPE_SZ, 4096) if $F_SETPIPE_SZ;
			$l2m->{au_peers} = [ $a_r, $a_w, $b_r, $b_w ];
		}
		$l2m->wq_workers_start('lei2mail', undef,
					$lei->oldset, { lei => $lei });
		pipe($lei->{startq}, $lei->{au_done}) or die "pipe: $!";
		fcntl($lei->{startq}, $F_SETPIPE_SZ, 4096) if $F_SETPIPE_SZ;
		delete $l2m->{au_peers};
	}
	$self->wq_workers_start('lei_xsearch', undef,
				$lei->oldset, { lei => $lei });
	my $op_c = delete $lei->{pkt_op_c};
	delete $lei->{pkt_op_p};
	@$end = ();
	$self->{opt_threads} = $lei->{opt}->{threads};
	$self->{opt_sort} = $lei->{opt}->{'sort'};
	if ($l2m) {
		$l2m->net_merge_all_done unless $lei->{auth};
	} else {
		start_query($self);
	}
	$lei->event_step_init; # wait for shutdowns
	$op_c->op_wait_event($ops);
}

sub add_uri {
	my ($self, $uri) = @_;
	if (my $curl = $self->{curl} //= which('curl') // 0) {
		require PublicInbox::MboxReader;
		require IO::Uncompress::Gunzip;
		require PublicInbox::LeiCurl;
		push @{$self->{remotes}}, $uri;
	} else {
		warn "curl missing, ignoring $uri\n";
	}
}

sub prepare_external {
	my ($self, $loc, $boost) = @_; # n.b. already ordered by boost
	if (ref $loc) { # already a URI, or PublicInbox::Inbox-like object
		return add_uri($self, $loc) if $loc->can('scheme');
	} elsif ($loc =~ m!\Ahttps?://!) {
		require URI;
		return add_uri($self, URI->new($loc));
	} elsif (-f "$loc/ei.lock") {
		require PublicInbox::ExtSearch;
		die "`\\n' not allowed in `$loc'\n" if index($loc, "\n") >= 0;
		$loc = PublicInbox::ExtSearch->new($loc);
	} elsif (-f "$loc/inbox.lock" || -d "$loc/public-inbox") {
		die "`\\n' not allowed in `$loc'\n" if index($loc, "\n") >= 0;
		require PublicInbox::Inbox; # v2, v1
		$loc = bless { inboxdir => $loc }, 'PublicInbox::Inbox';
	} else {
		warn "W: ignoring $loc, unable to determine type\n";
		return;
	}
	push @{$self->{locals}}, $loc;
}


1;
