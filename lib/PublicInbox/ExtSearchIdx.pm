# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Detached/external index cross inbox search indexing support
# read-write counterpart to PublicInbox::ExtSearch
#
# It's based on the same ideas as public-inbox-v2-format(5) using
# over.sqlite3 for dedupe and sharded Xapian.  msgmap.sqlite3 is
# missing, so there is no Message-ID conflict resolution, meaning
# no NNTP support for now.
#
# v2 has a 1:1 mapping of index:inbox or msgmap for NNTP support.
# This is intended to be an M:N index:inbox mapping, but it'll likely
# be 1:N in common practice (M==1)
#
# {-need_xapian} only applies to indexing for individual messages,
# the Xapian /misc/ index for indexing information about inboxes
# themselves is always created and maintained.

package PublicInbox::ExtSearchIdx;
use strict;
use v5.10.1;
use parent qw(PublicInbox::ExtSearch PublicInbox::Umask PublicInbox::Lock);
use autodie qw(mkdir);
use Carp qw(croak carp);
use Scalar::Util qw(blessed);
use Sys::Hostname qw(hostname);
use File::Glob qw(bsd_glob GLOB_NOSORT);
use PublicInbox::SQLiteUtil;
use PublicInbox::Isearch;
use PublicInbox::MultiGit;
use PublicInbox::Spawn qw(run_wait);
use PublicInbox::Search;
use PublicInbox::SearchIdx qw(prepare_stack is_ancestor is_bad_blob
	update_checkpoint);
use PublicInbox::IPC qw(nproc_shards);
use PublicInbox::OverIdx;
use PublicInbox::MiscIdx;
use PublicInbox::MID qw(mids);
use PublicInbox::V2Writable;
use PublicInbox::InboxWritable;
use PublicInbox::ContentHash qw(content_hash);
use PublicInbox::Eml;
use PublicInbox::DS qw(now add_timer);
use DBI qw(:sql_types); # SQL_BLOB
use PublicInbox::Admin qw(fmt_localtime);
use PublicInbox::Config qw(rel2abs_collapsed);
use PublicInbox::IO qw(try_cat);

sub detect_indexlevel ($) {
	my ($self) = @_;
	my $l = 'full'; # the default for new extindices
	if ($self->xdb) {
		my $m = $self->xdb->get_metadata('indexlevel');
		if ($m eq 'medium') {
			$l = 'medium';
		} elsif ($m ne '') {
			warn <<EOM;
W: $self->{topdir} has unexpected indexlevel in Xapian: $l
EOM
		}
	# we're basic if over.sqlite3 is non-empty and there's nothing
	# in Xapian
	} elsif ($self->over && $self->over->dbh->selectrow_array(<<'') > 0) {
SELECT COUNT(*) FROM over

		$l = 'basic';
	}
	delete @$self{qw(xdb over)};
	$l;
}

sub new {
	my (undef, $dir, $opt) = @_;
	my $self = bless {
		xpfx => "$dir/ei".PublicInbox::Search::SCHEMA_VERSION,
		topdir => $dir,
		creat => $opt->{creat},
		ibx_map => {}, # (newsgroup//inboxdir) => $ibx
		ibx_active => [], # by config section order
		ibx_known => [], # by config section order
		transact_bytes => 0,
		total_bytes => 0,
		current_info => '',
		parallel => 1,
		lock_path => "$dir/ei.lock",
	}, __PACKAGE__;
	$self->{shards} = $self->count_shards ||
		nproc_shards { nproc => $opt->{jobs} };
	my $l = $opt->{indexlevel};
	my $over_file = "$self->{xpfx}/over.sqlite3";
	$l ||= detect_indexlevel $self;
	$l !~ $PublicInbox::SearchIdx::INDEXLEVELS and
		die "invalid indexlevel=$l\n";
	$self->{indexlevel} = $l;
	my $oidx = PublicInbox::OverIdx->new($over_file);
	$oidx->{journal_mode} = 'wal' if $opt->{wal};
	$self->{-no_fsync} = $oidx->{-no_fsync} = 1 if !$opt->{fsync};
	$self->{-dangerous} = 1 if $opt->{dangerous};
	$self->{oidx} = $oidx;
	$self
}

sub attach_inbox {
	my ($self, $ibx, $types) = @_;
	$self->{ibx_map}->{$ibx->eidx_key} //= do {
		delete $self->{-ibx_ary_known}; # invalidate cache
		delete $self->{-ibx_ary_active}; # invalidate cache
		delete $self->{id2pos};
		$types //= [ qw(active known) ];
		for my $t (@$types) {
			push @{$self->{"ibx_$t"}}, $ibx;
		}
		$ibx;
	}
}

sub _ibx_attach { # each_inbox callback
	my ($ibx, $self, $types) = @_;
	attach_inbox($self, $ibx, $types);
}

sub attach_config {
	my ($self, $cfg, $ibxs) = @_;
	$self->{cfg} = $cfg;
	my ($types, $ro);

	# lookup extindex.$NAME.<indexheader|altid>
	my $eidx_dir = rel2abs_collapsed($self->{topdir});
	for my $k (grep(/\Aextindex\.(?:.+)\.topdir\z/, keys %$cfg)) {
		next if rel2abs_collapsed($cfg->{$k}) ne $eidx_dir;
		my $n = substr($k, length('extindex.'), -length('.topdir'));
		$ro = $cfg->lookup_ei($n) and last;
	}

	# and copy from read-only to our read-write $self
	for my $f (qw(altid indexheader)) {
		$self->{$f} = $ro->{$f} if defined $ro->{$f};
	}

	if ($ibxs) {
		for my $ibx (@$ibxs) {
			$self->{ibx_map}->{$ibx->eidx_key} //= do {
				push @{$self->{ibx_active}}, $ibx;
				push @{$self->{ibx_known}}, $ibx;
				$ibx;
			}
		}
		# invalidate cache
		delete $self->{-ibx_ary_known};
		delete $self->{-ibx_ary_active};
		$types = [ 'known' ];
	}
	$types //= [ qw(known active) ];
	$cfg->each_inbox(\&_ibx_attach, $self, $types);
}

sub bad_ibx_id ($$;$) {
	my ($self, $ibx_id, $cb) = @_;
	my $msg = "E: bad/stale ibx_id=#$ibx_id encountered";
	my $ekey = $self->{oidx}->dbh->selectrow_array(<<EOM, undef, $ibx_id);
SELECT eidx_key FROM inboxes WHERE ibx_id = ? LIMIT 1
EOM
	$msg .= " (formerly `$ekey')" if defined $ekey;
	$cb //= \&carp;
	$cb->($msg, "\nE: running $0 --gc may be required");
}

sub check_xr3 ($$$) {
	my ($self, $id2pos, $xr3) = @_;
	@$xr3 = grep {
		defined($id2pos->{$_->[0]}) ? 1 : bad_ibx_id($self, $_->[0])
	} @$xr3;
}

sub apply_boost ($$) {
	my ($req, $smsg) = @_;
	my $id2pos = $req->{self}->{id2pos}; # index in ibx_sorted
	my $xr3 = $req->{self}->{oidx}->get_xref3($smsg->{num}, 1);
	check_xr3($req->{self}, $id2pos, $xr3);
	@$xr3 = sort { # sort ascending
		$id2pos->{$a->[0]} <=> $id2pos->{$b->[0]}
				||
		$a->[1] <=> $b->[1] # break ties with {xnum}
	} @$xr3;
	my $new_smsg = $req->{new_smsg};
	return if $xr3->[0]->[2] ne $new_smsg->oidbin; # loser

	# replace the old smsg with the more boosted one
	$new_smsg->{num} = $smsg->{num};
	$new_smsg->populate($req->{eml}, $req);
	$req->{self}->{oidx}->add_overview($req->{eml}, $new_smsg);
}

sub remove_doc ($$) {
	my ($self, $docid) = @_;
	$self->{oidx}->delete_by_num($docid);
	$self->{oidx}->eidxq_del($docid);
	$self->{-need_xapian} and
		idx_shard($self, $docid)->ipc_do('xdb_remove', $docid);
}

sub _unref_doc ($$$$$;$) {
	my ($self, $docid, $ibx, $xnum, $oidbin, $eml) = @_;
	my $smsg;
	if (ref($docid)) {
		$smsg = $docid;
		$docid = $smsg->{num};
	}
	if (defined($oidbin) && defined($xnum) && blessed($ibx) && $ibx->over) {
		my $smsg = $ibx->over->get_art($xnum);
		if ($smsg && $smsg->oidbin eq $oidbin) {
			carp("BUG: (non-fatal) ".$ibx->eidx_key.
				" #$xnum $smsg->{blob} still valid");
			return;
		}
	}
	my $s = 'DELETE FROM xref3 WHERE oidbin = ?';
	$s .= ' AND ibx_id = ?' if defined($ibx);
	$s .= ' AND xnum = ?' if defined($xnum);
	my $del = $self->{oidx}->dbh->prepare_cached($s);
	my $col = 0;
	$del->bind_param(++$col, $oidbin, SQL_BLOB);
	$del->bind_param(++$col, $ibx->{-ibx_id}) if $ibx;
	$del->bind_param(++$col, $xnum) if defined($xnum);
	$del->execute;
	my $xr3 = $self->{oidx}->get_xref3($docid);
	if (scalar(@$xr3) == 0) { # all gone
		remove_doc($self, $docid);
	} else { # enqueue for reindex of remaining messages
		if ($ibx && $self->{-need_xapian}) {
			my $ekey = $ibx->{-gc_eidx_key} // $ibx->eidx_key;
			my $idx = $self->idx_shard($docid);
			my @list_ids = $eml->header_raw('List-Id');
			$idx->ipc_do('remove_eidx_info_raw', $docid, $ekey,
					@list_ids);
		} # else: we can't remove_eidx_info_raw in reindex-only path

		# replace invalidated blob ASAP with something which should be
		# readable since we may commit the transaction on checkpoint.
		# eidxq processing will re-apply boost
		$smsg //= $self->{oidx}->get_art($docid);
		my $hex = unpack('H*', $oidbin);
		if ($smsg && $smsg->{blob} eq $hex) {
			$xr3->[0] =~ /:([a-f0-9]{40,}+)\z/ or
				die "BUG: xref $xr3->[0] has no OID";
			$self->{oidx}->update_blob($smsg, $1);
		}
		# yes, add, we'll need to re-apply boost
		$self->{oidx}->eidxq_add($docid);
	}
	@$xr3
}

sub do_xpost ($$) {
	my ($req, $smsg) = @_;
	my $self = $req->{self};
	my $docid = $smsg->{num};
	my $oid = $req->{oid};
	my $xibx = $req->{ibx} // $self->{ibx};
	my $eml = $req->{eml};
	if (my $new_smsg = $req->{new_smsg}) { # 'm' on cross-posted message
		my $eidx_key = $xibx->eidx_key;
		my $xnum = $req->{xnum};
		$self->{oidx}->add_xref3($docid, $xnum, $oid, $eidx_key);
		$self->{-need_xapian} and
			idx_shard($self, $docid)->
					add_eidx_info($docid, $eidx_key, $eml);
		apply_boost($req, $smsg) if $self->{boost_in_use};
	} else { # 'd' no {xnum}
		$self->git->async_wait_all;
		$oid = pack('H*', $oid);
		_unref_doc $self, $docid, $xibx, undef, $oid, $eml;
	}
}

# called by V2Writable::sync_prepare
sub artnum_max { $_[0]->{oidx}->eidx_max }

sub index_unseen ($) {
	my ($req) = @_;
	my $new_smsg = $req->{new_smsg} or die 'BUG: {new_smsg} unset';
	my $eml = delete $req->{eml};
	$new_smsg->populate($eml, $req);
	my $self = $req->{self};
	my $docid = $self->{oidx}->adj_counter('eidx_docid', '+');
	$new_smsg->{num} = $docid;
	$self->{oidx}->add_overview($eml, $new_smsg);
	my $oid = $new_smsg->{blob};
	my $ibx = ($req->{ibx} // $self->{ibx}) or die 'BUG: {ibx} unset';
	my $ekey = $new_smsg->{eidx_key} = $ibx->eidx_key;
	$self->{oidx}->add_xref3($docid, $req->{xnum}, $oid, $ekey);
	$self->{-need_xapian} and
		idx_shard($self, $docid)->index_eml($eml, $new_smsg);
	update_checkpoint $self, $new_smsg->{bytes};
}

sub do_finalize ($) {
	my ($req) = @_;
	if (my $indexed = $req->{indexed}) { # duplicated messages
		do_xpost($req, $_) for @$indexed;
	} elsif (exists $req->{new_smsg}) { # totally unseen messsage
		index_unseen($req);
	} else {
		# `d' message was already unindexed in the v1/v2 inboxes,
		# so it's too noisy to warn, here.
	}
	# cur_cmt may be undef for unindex_oid, set by V2Writable::index_todo
	if (defined(my $cur_cmt = $req->{cur_cmt})) {
		$req->{self}->{latest_cmt} = $cur_cmt;
	}
}

sub do_step ($) { # main iterator for adding messages to the index
	my ($req) = @_;
	my $self = $req->{self} // die 'BUG: {self} missing';
	while (1) {
		if (my $next_arg = $req->{next_arg}) {
			if (my $smsg = $self->{oidx}->next_by_mid(@$next_arg)) {
				$req->{cur_smsg} = $smsg;
				$self->git->cat_async($smsg->{blob},
							\&ck_existing, $req);
				return; # ck_existing calls do_step
			}
			delete $req->{next_arg};
		}
		die "BUG: {cur_smsg} still set" if $req->{cur_smsg};
		my $mid = shift(@{$req->{mids}}) // last;
		my ($id, $prev);
		$req->{next_arg} = [ $mid, \$id, \$prev ];
		# loop again
	}
	do_finalize($req);
}

sub _blob_missing ($$) { # called when a known $smsg->{blob} is gone
	my ($req, $smsg) = @_;
	# xnum and ibx are unknown, we only call this when an entry from
	# /ei*/over.sqlite3 is bad, not on entries from xap*/over.sqlite3
	$req->{self}->git->async_wait_all;
	_unref_doc $req->{self}, $smsg, undef, undef, $smsg->oidbin;
}

sub ck_existing { # git->cat_async callback
	my ($bref, $oid, $type, $size, $req) = @_;
	my $smsg = delete $req->{cur_smsg} or die 'BUG: {cur_smsg} missing';
	if ($type eq 'missing') {
		_blob_missing($req, $smsg);
	} elsif (!is_bad_blob($oid, $type, $size, $smsg->{blob})) {
		my $self = $req->{self} // die 'BUG: {self} missing';
		local $self->{current_info} = "$self->{current_info} $oid";
		my $cur = PublicInbox::Eml->new($bref);
		if (content_hash($cur) eq $req->{chash}) {
			push @{$req->{indexed}}, $smsg; # for do_xpost
		} # else { index_unseen later }
	}
	do_step($req);
}

# is the messages visible in the inbox currently being indexed?
# return the number if so
sub cur_ibx_xnum ($$;$) {
	my ($req, $bref, $mismatch) = @_;
	my $ibx = ($req->{ibx} // $req->{self}->{ibx}) or
		die 'BUG: current {ibx} missing';
	$req->{eml} = PublicInbox::Eml->new($bref);
	$req->{chash} = content_hash($req->{eml});
	$req->{mids} = mids($req->{eml});
	for my $mid (@{$req->{mids}}) {
		my ($id, $prev);
		while (my $x = $ibx->over->next_by_mid($mid, \$id, \$prev)) {
			return $x->{num} if $x->{blob} eq $req->{oid};
			push @$mismatch, $x if $mismatch;
		}
	}
	undef;
}

sub index_oid { # git->cat_async callback for 'm'
	my ($bref, $oid, $type, $size, $req) = @_;
	my $self = $req->{self} // die 'BUG: {self} missing';
	local $self->{current_info} = "$self->{current_info} $oid";
	return if is_bad_blob($oid, $type, $size, $req->{oid});
	my $new_smsg = $req->{new_smsg} = bless {
		blob => $oid,
	}, 'PublicInbox::Smsg';
	$new_smsg->set_bytes($$bref, $size);
	++$self->{nrec};
	my $mismatch = [];
	$req->{xnum} = cur_ibx_xnum($req, $bref, $mismatch) // do {
		warn "# deleted\n";
		warn "# mismatch $_->{blob}\n" for @$mismatch;
		$self->{latest_cmt} = $req->{cur_cmt} //
			die "BUG: {cur_cmt} unset ($oid)\n";
		return;
	};
	do_step($req);
}

sub unindex_oid { # git->cat_async callback for 'd'
	my ($bref, $oid, $type, $size, $req) = @_;
	my $self = $req->{self};
	local $self->{current_info} = "$self->{current_info} $oid";
	return if is_bad_blob($oid, $type, $size, $req->{oid});
	return if defined(cur_ibx_xnum($req, $bref)); # was re-added
	do_step($req);
}

# overrides V2Writable::last_commits, called by sync_ranges via sync_prepare
sub last_commits {
	my ($self) = @_;
	my $heads = [];
	my $ekey = $self->{ibx}->eidx_key;
	my $uv = $self->{ibx}->uidvalidity;
	for my $i (0..$self->{epoch_max}) {
		$heads->[$i] = $self->{oidx}->eidx_meta("lc-v2:$ekey//$uv;$i");
	}
	$heads;
}

sub _ibx_index_reject ($) {
	my ($ibx) = @_;
	$ibx->mm // return 'unindexed, no msgmap.sqlite3';
	$ibx->uidvalidity // return 'no UIDVALIDITY';
	$ibx->over // return 'unindexed, no over.sqlite3';
	undef;
}

sub _sync_inbox ($$) {
	my ($self, $ibx) = @_;
	my $ekey = $ibx->eidx_key;
	if (defined(my $err = _ibx_index_reject($ibx))) {
		return "W: skipping $ekey ($err)";
	}
	local $self->{ibx} = $ibx;
	$self->{nrec} = 0;
	local $self->{epoch_max};
	my $v = $ibx->version;
	local $self->{todo}; # set by sync_prepare
	local $self->{ranges};
	local $self->{unindexed};
	if ($v == 2) {
		$self->{epoch_max} = $ibx->max_git_epoch // return;
		sync_prepare($self); # or return # TODO: once MiscIdx is stable
	} elsif ($v == 1) {
		my $uv = $ibx->uidvalidity;
		my $lc = $self->{oidx}->eidx_meta("lc-v1:$ekey//$uv");
		my $head = $ibx->mm->last_commit //
			return "E: $ibx->{inboxdir} is not indexed";
		my $stk = prepare_stack $self, $lc ? "$lc..$head" : $head;
		my $unit = { stack => $stk, git => $ibx->git };
		push @{$self->{todo}}, $unit;
	} else {
		return "E: $ekey unsupported inbox version (v$v)";
	}
	PublicInbox::V2Writable::process_todo $self;
	$self->{midx}->index_ibx($ibx) unless $self->{quit};
	$ibx->git->cleanup; # done with this inbox, now
	undef;
}

sub eidx_gc_scan_inboxes ($) {
	my ($self) = @_;
	my ($x3_doc, $ibx_ck);
restart:
	$x3_doc = $self->{oidx}->dbh->prepare(<<EOM);
SELECT docid,xnum,oidbin FROM xref3 WHERE ibx_id = ?
EOM
	$ibx_ck = $self->{oidx}->dbh->prepare(<<EOM);
SELECT ibx_id,eidx_key FROM inboxes
EOM
	$ibx_ck->execute;
	while (my ($ibx_id, $eidx_key) = $ibx_ck->fetchrow_array) {
		next if $self->{ibx_map}->{$eidx_key};
		$self->{midx}->remove_eidx_key($eidx_key);
		warn "# deleting messages for $eidx_key...\n";
		$x3_doc->execute($ibx_id);
		my $ibx = { -ibx_id => $ibx_id, -gc_eidx_key => $eidx_key };
		while (my ($docid, $xnum, $oid) = $x3_doc->fetchrow_array) {
			my $r = _unref_doc $self, $docid, $ibx, $xnum, $oid;
			$oid = unpack('H*', $oid);
			$r = $r ? 'unref' : 'remove';
			warn "# $r #$docid $eidx_key $oid\n";
			if (update_checkpoint $self) {
				$x3_doc = $ibx_ck = undef;
				reindex_checkpoint($self);
				goto restart;
			}
		}
		$self->{oidx}->dbh->do(<<'', undef, $ibx_id);
DELETE FROM inboxes WHERE ibx_id = ?

		# drop last_commit info
		# We use GLOB in addition to REGEXP since GLOB can use indices
		my $lc_i = $self->{oidx}->dbh->prepare(<<'');
SELECT key FROM eidx_meta WHERE key GLOB ? AND key REGEXP ?

		my $ekg = 'lc-v[1-9]*:'.
			PublicInbox::SQLiteUtil::escape_glob($eidx_key).'//*';
		$lc_i->execute($ekg, qr!\Alc-v[1-9]+:\Q$eidx_key\E//!);
		while (my ($key) = $lc_i->fetchrow_array) {
			warn "# removing $key\n";
			$self->{oidx}->dbh->do(<<'', undef, $key);
DELETE FROM eidx_meta WHERE key = ?

		}
		warn "# $eidx_key removed\n";
	}
}

sub eidx_gc_scan_shards ($) { # TODO: use for lei/store
	my ($self) = @_;
	my $nr = $self->{oidx}->dbh->do(<<'');
DELETE FROM xref3 WHERE docid NOT IN (SELECT num FROM over)

	warn "# eliminated $nr stale xref3 entries\n" if $nr != 0;
	reindex_checkpoint($self) if update_checkpoint $self;

	# fixup from old bugs:
	$nr = $self->{oidx}->dbh->do(<<'');
DELETE FROM over WHERE num > 0 AND num NOT IN (SELECT docid FROM xref3)

	warn "# eliminated $nr stale over entries\n" if $nr != 0;
	reindex_checkpoint($self) if update_checkpoint $self;

	$nr = $self->{oidx}->dbh->do(<<'');
DELETE FROM eidxq WHERE docid NOT IN (SELECT num FROM over)

	warn "# eliminated $nr stale reindex queue entries\n" if $nr != 0;
	reindex_checkpoint($self) if update_checkpoint $self;

	my ($cur) = $self->{oidx}->dbh->selectrow_array(<<EOM);
SELECT MIN(num) FROM over WHERE num > 0
EOM
	$cur // return; # empty
	return unless $self->{-need_xapian};
	my ($r, $n, %active_shards);
	$nr = 0;
	while (1) {
		$r = $self->{oidx}->dbh->selectcol_arrayref(<<"", undef, $cur);
SELECT num FROM over WHERE num >= ? ORDER BY num ASC LIMIT 10000

		last unless scalar(@$r);
		while (defined($n = shift @$r)) {
			for my $i ($cur..($n - 1)) {
				my $idx = idx_shard($self, $i);
				$idx->ipc_do('xdb_remove_quiet', $i);
				$active_shards{$idx} = $idx;
			}
			$cur = $n + 1;
		}
		if (update_checkpoint $self) {
			for my $idx (values %active_shards) {
				$nr += $idx->ipc_do('nr_quiet_rm')
			}
			%active_shards = ();
			reindex_checkpoint($self);
		}
	}
	warn "# eliminated $nr stale Xapian documents\n" if $nr != 0;
}

sub eidx_gc { # top-level entry point
	my ($self, $opt) = @_;
	$self->{cfg} or die "E: GC requires ->attach_config\n";
	$opt->{-idx_gc} = 1;
	local $self->{checkpoint_unlocks} = 1;
	local $self->{need_checkpoint} = 0;
	local $self->{nrec};
	local $self->{-opt} = $opt;
	local $self->{latest_cmt};
	$self->idx_init($opt); # acquire lock via V2Writable::_idx_init
	eidx_gc_scan_inboxes $self;
	eidx_gc_scan_shards $self;
	done($self);
}

sub _ibx_for ($$) {
	my ($self, $smsg) = @_;
	my $ibx_id = delete($smsg->{ibx_id}) // die 'BUG: {ibx_id} unset';
	my $pos = $self->{id2pos}->{$ibx_id} //
		bad_ibx_id($self, $ibx_id, \&croak);
	$self->{-ibx_ary_known}->[$pos] //
		die "BUG: ibx for $smsg->{blob} not mapped"
}

sub _fd_constrained ($) {
	my ($self) = @_;
	$self->{-fd_constrained} //= do {
		my $soft = PublicInbox::Search::ulimit_n;
		if (defined($soft)) {
			# $want is an estimate
			my $want = scalar(@{$self->{ibx_active}}) + 64;
			my $ret = $want > $soft;
			if ($ret) {
				warn <<EOF;
RLIMIT_NOFILE=$soft insufficient (want: $want), will close DB handles early
EOF
			}
			$ret;
		} else {
			warn "Unable to determine RLIMIT_NOFILE: $@\n";
			1;
		}
	};
}

sub _reindex_finalize ($$$) {
	my ($req, $smsg, $eml) = @_;
	my $self = $req->{self};
	my $by_chash = delete $req->{by_chash} or die 'BUG: no {by_chash}';
	my $nr = scalar(keys(%$by_chash)) or die 'BUG: no content hashes';
	my $orig_smsg = $req->{orig_smsg} // die 'BUG: no {orig_smsg}';
	my $docid = $smsg->{num} = $orig_smsg->{num};
	$self->{oidx}->add_overview($eml, $smsg); # may rethread
	update_checkpoint $self, $smsg->{bytes};
	my $chash0 = $smsg->{chash} // die "BUG: $smsg->{blob} no {chash}";
	my $stable = delete($by_chash->{$chash0}) //
				die "BUG: $smsg->{blob} chash missing";
	my $top_smsg = pop @$stable;
	$top_smsg == $smsg or die 'BUG: top_smsg != smsg';
	my $ibx = _ibx_for $self, $smsg;
	$smsg->{eidx_key} = $ibx->eidx_key;
	if ($self->{-need_xapian}) {
		my $idx = idx_shard($self, $docid);
		$idx->index_eml($eml, $smsg);
		for my $x (reverse @$stable) {
			my $lid = delete $x->{lid} // die 'BUG: no {lid}';
			@$lid and $idx->ipc_do('add_eidx_info_raw', $docid,
						_ibx_for($self, $x)->eidx_key,
						@$lid);
		}
	}
	return if $nr == 1; # likely, all good

	$self->git->async_wait_all;
	warn "W: #$docid split into $nr due to deduplication change\n";
	my @todo;
	for my $ary (values %$by_chash) {
		for my $x (reverse @$ary) {
			warn "removing #$docid xref3 $x->{blob}\n";
			my $bin = $x->oidbin;
			my $n = _unref_doc $self, $docid, undef, undef, $bin;
			die "BUG: $x->{blob} invalidated #$docid" if $n == 0;
		}
		my $x = pop(@$ary) // die "BUG: #$docid {by_chash} empty";
		$x->{num} = delete($x->{xnum}) // die '{xnum} unset';
		$ibx = _ibx_for $self, $x;
		if (my $over = $ibx->over) {
			my $e = $over->get_art($x->{num});
			$e->{blob} eq $x->{blob} or die <<EOF;
$x->{blob} != $e->{blob} (${\$ibx->eidx_key}:$e->{num});
EOF
			push @todo, $ibx, $e;
			$over->dbh_close if _fd_constrained($self);
		} else {
			die "$ibx->{inboxdir}: over.sqlite3 unusable: $!\n";
		}
	}
	undef $by_chash;
	while (my ($ibx, $e) = splice(@todo, 0, 2)) {
		reindex_unseen($self, $ibx, $e);
	}
}

sub _reindex_oid { # git->cat_async callback
	my ($bref, $oid, $type, $size, $req) = @_;
	my $self = $req->{self};
	my $orig_smsg = $req->{orig_smsg} // die 'BUG: no {orig_smsg}';
	my $expect_oid = $req->{xr3r}->[$req->{ix}]->[2];
	my $docid = $orig_smsg->{num};
	if (is_bad_blob($oid, $type, $size, $expect_oid)) {
		my $oidbin = pack('H*', $expect_oid);
		my $remain = _unref_doc $self, $docid, undef, undef, $oidbin;
		if ($remain == 0) {
			warn "W: #$docid ($oid) gone or corrupt\n";
		} elsif (my $next_oid = $req->{xr3r}->[++$req->{ix}]->[2]) {
			$self->git->cat_async($next_oid, \&_reindex_oid, $req);
		} else {
			warn "BUG: #$docid ($oid) gone (UNEXPECTED)\n";
		}
		return;
	}
	my $ci = $self->{current_info};
	local $self->{current_info} = "$ci #$docid $oid";
	my $re_smsg = bless { blob => $oid }, 'PublicInbox::Smsg';
	$re_smsg->set_bytes($$bref, $size);
	my $eml = PublicInbox::Eml->new($bref);
	$re_smsg->populate($eml, { autime => $orig_smsg->{ds},
				cotime => $orig_smsg->{ts} });
	my $chash = content_hash($eml);
	$re_smsg->{chash} = $chash;
	$re_smsg->{xnum} = $req->{xr3r}->[$req->{ix}]->[1];
	$re_smsg->{ibx_id} = $req->{xr3r}->[$req->{ix}]->[0];
	@{$re_smsg->{lid}} = $eml->header_raw('List-Id');
	push @{$req->{by_chash}->{$chash}}, $re_smsg;
	if (my $next_oid = $req->{xr3r}->[++$req->{ix}]->[2]) {
		$self->git->cat_async($next_oid, \&_reindex_oid, $req);
	} else { # last $re_smsg is the highest priority xref3
		local $self->{current_info} = "$ci #$docid";
		_reindex_finalize($req, $re_smsg, $eml);
	}
}

sub _reindex_smsg ($$) {
	my ($self, $smsg) = @_;
	my $docid = $smsg->{num};
	my $xr3 = $self->{oidx}->get_xref3($docid, 1);
	if (scalar(@$xr3) == 0) { # _reindex_check_stale should've covered this
		warn <<"";
BUG? #$docid $smsg->{blob} is not referenced by inboxes during reindex

		remove_doc($self, $docid);
		return;
	}

	# we sort {xr3r} in the reverse order of ibx_sorted so we can
	# hit the common case in _reindex_finalize without rereading
	# from git (or holding multiple messages in memory).
	my $id2pos = $self->{id2pos}; # index in ibx_sorted
	check_xr3($self, $id2pos, $xr3);
	@$xr3 = sort { # sort descending
		$id2pos->{$b->[0]} <=> $id2pos->{$a->[0]}
				||
		$b->[1] <=> $a->[1] # break ties with {xnum}
	} @$xr3;
	@$xr3 = map { [ $_->[0], $_->[1], unpack('H*', $_->[2]) ] } @$xr3;
	my $req = { orig_smsg => $smsg, self => $self, xr3r => $xr3, ix => 0 };
	$self->git->cat_async($xr3->[$req->{ix}]->[2], \&_reindex_oid, $req);
}

sub host_ident () {
	# I've copied FS images and only changed the hostname before,
	# so prepend hostname.  Use `state' since these a BOFH can change
	# these while this process is running and we always want to be
	# able to release locks taken by this process.
	state $retval = hostname . '-' . do {
		my $m = try_cat '/etc/machine-id'; # machine-id(5) is systemd
		# (g)hostid(1) is in GNU coreutils, kern.hostid is most BSDs
		chomp($m ||= `{ sysctl -n kern.hostid ||
				hostid || ghostid; } 2>/dev/null`
			|| "no-machine-id-or-hostid-on-$^O");
		$m;
	};
}

sub eidxq_release ($) {
	my ($self) = @_;
	my $expect = delete($self->{-eidxq_locked}) or return;
	my ($owner_pid, undef) = split(/-/, $expect);
	return if $owner_pid != $$; # shards may fork
	my $oidx = $self->{oidx};
	$oidx->begin_lazy;
	my $cur = $oidx->eidx_meta('eidxq_lock') // '';
	if ($cur eq $expect) {
		$oidx->eidx_meta('eidxq_lock', '');
		return 1;
	} elsif ($cur ne '') {
		warn "E: eidxq_lock($expect) stolen by $cur\n";
	} else {
		warn "E: eidxq_lock($expect) released by another process\n";
	}
	undef;
}

sub DESTROY {
	my ($self) = @_;
	eidxq_release $self and $self->{oidx}->commit_lazy;
}

sub _eidxq_take ($) {
	my ($self) = @_;
	my $val = "$$-${\time}-$>-".host_ident;
	$self->{oidx}->eidx_meta('eidxq_lock', $val);
	$self->{-eidxq_locked} = $val;
}

sub eidxq_lock_acquire ($) {
	my ($self) = @_;
	my $oidx = $self->{oidx};
	$oidx->begin_lazy;
	my $cur = $oidx->eidx_meta('eidxq_lock') || return _eidxq_take($self);
	if (my $locked = $self->{-eidxq_locked}) { # be lazy
		return $locked if $locked eq $cur;
	}
	my ($pid, $time, $euid, $ident) = split(/-/, $cur, 4);
	my $t = fmt_localtime($time);
	local $self->{current_info} = 'eidxq';
	if ($euid == $> && $ident eq host_ident) {
		kill(0, $pid) and warn <<EOM and return;
# PID:$pid (re)indexing since $t, it will continue our work
EOM
		if ($!{ESRCH}) {
			warn "# eidxq_lock is stale ($cur), clobbering\n";
			return _eidxq_take($self);
		}
		warn "E: kill(0, $pid) failed: $!\n"; # fall-through:
	}
	my $fn = $oidx->dbh->sqlite_db_filename;
	warn <<EOF;
W: PID:$pid, UID:$euid on $ident is indexing Xapian since $t
W: If this is unexpected, delete `eidxq_lock' from the `eidx_meta' table:
W:	sqlite3 $fn 'DELETE FROM eidx_meta WHERE key = "eidxq_lock"'
EOF
	undef;
}

sub ibx_sorted ($$) {
	my ($self, $type) = @_;
	$self->{"-ibx_ary_$type"} //= do {
		# highest boost first, stable for config-ordering tiebreaker
		use sort 'stable';
		[ sort {
			($b->{boost} // 0) <=> ($a->{boost} // 0)
		  } @{$self->{'ibx_'.$type} // die "BUG: $type unknown"} ];
	}
}

sub prep_id2pos ($) {
	my ($self) = @_;
	my %id2pos;
	my $pos = 0;
	$id2pos{$_->{-ibx_id}} = $pos++ for (@{ibx_sorted($self, 'known')});
	\%id2pos;
}

sub eidxq_process ($) { # for reindexing
	my ($self) = @_;
	local $self->{current_info} = 'eidxq process';
	return unless ($self->{cfg} && eidxq_lock_acquire($self));
	my $dbh = $self->{oidx}->dbh;
	my $tot = $dbh->selectrow_array('SELECT COUNT(*) FROM eidxq') or return;
	$self->{nrec} = 0;
	local $self->{-regen_fmt} = "%u/$tot\n";
	my $pr = $self->{-opt}->{-progress};
	if ($pr) {
		my $min = $dbh->selectrow_array('SELECT MIN(docid) FROM eidxq');
		my $max = $dbh->selectrow_array('SELECT MAX(docid) FROM eidxq');
		$pr->("Xapian indexing $min..$max (total=$tot)\n");
	}
	$self->{id2pos} //= prep_id2pos $self;
	my ($del, $iter);
restart:
	$del = $dbh->prepare('DELETE FROM eidxq WHERE docid = ?');
	$iter = $dbh->prepare('SELECT docid FROM eidxq ORDER BY docid ASC');
	$iter->execute;
	while (defined(my $docid = $iter->fetchrow_array)) {
		last if $self->{quit};
		if (my $smsg = $self->{oidx}->get_art($docid)) {
			_reindex_smsg($self, $smsg);
		} else {
			warn "E: #$docid does not exist in over\n";
		}
		$del->execute($docid);
		++$self->{nrec};

		if (update_checkpoint $self) {
			$dbh = $del = $iter = undef;
			reindex_checkpoint($self); # release lock
			$dbh = $self->{oidx}->dbh;
			goto restart;
		}
	}
	$self->git->async_wait_all;
	$pr->("reindexed $self->{nrec}/$tot\n") if $pr;
}

sub _reindex_unseen { # git->cat_async callback
	my ($bref, $oid, $type, $size, $req) = @_;
	return if is_bad_blob($oid, $type, $size, $req->{oid});
	my $self = $req->{self} // die 'BUG: {self} unset';
	local $self->{current_info} = "$self->{current_info} $oid";
	my $new_smsg = bless { blob => $oid, }, 'PublicInbox::Smsg';
	$new_smsg->set_bytes($$bref, $size);
	my $eml = $req->{eml} = PublicInbox::Eml->new($bref);
	$req->{new_smsg} = $new_smsg;
	$req->{chash} = content_hash($eml);
	$req->{mids} = mids($eml); # do_step iterates through this
	do_step($req); # enter the normal indexing flow
}

# --reindex may catch totally unseen messages, this handles them
sub reindex_unseen ($$$) {
	my ($self, $ibx, $xsmsg) = @_;
	my $req = {
		self => $self,
		autime => $xsmsg->{ds},
		cotime => $xsmsg->{ts},
		oid => $xsmsg->{blob},
		ibx => $ibx,
		xnum => $xsmsg->{num},
		# {mids} and {chash} will be filled in at _reindex_unseen
	};
	warn "# reindex_unseen ${\$ibx->eidx_key}:$req->{xnum}:$req->{oid}\n";
	$self->git->cat_async($xsmsg->{blob}, \&_reindex_unseen, $req);
}

sub _unref_stale_range ($$$) {
	my ($self, $ibx, $lt_or_gt) = @_;
	my $r;
	my $lim = 10000;
	do {
		$r = $self->{oidx}->dbh->selectall_arrayref(
			<<EOS, undef, $ibx->{-ibx_id});
SELECT docid,xnum,oidbin FROM xref3
WHERE ibx_id = ? AND $lt_or_gt LIMIT $lim
EOS
		return if $self->{quit};
		for (@$r) { # hopefully rare, not worth optimizing:
			my ($docid, $xnum, $oidbin) = @$_;
			my $hex = unpack('H*', $oidbin);
			warn("# $xnum:$hex (#$docid): stale\n");
			_unref_doc $self, $docid, $ibx, $xnum, $oidbin;
		}
	} while (scalar(@$r) == $lim);
	1;
}

sub _reindex_check_ibx ($$) {
	my ($self, $ibx) = @_;
	my $ibx_id = $ibx->{-ibx_id};
	my $slice = 10000;
	my $opt = { limit => $slice };
	my ($beg, $end) = (1, $slice);
	my $ekey = $ibx->eidx_key;
	my ($max, $max0);
	do {
		$max0 = $ibx->mm->num_highwater;
		sync_inbox($self, $ibx) and return; # warned
		$max = $ibx->mm->num_highwater;
		return if $self->{quit};
	} while ($max > $max0 &&
		warn("# $ekey moved $max0..$max, resyncing..\n"));
	$end = $max if $end > $max;

	# first, check if we missed any messages in target $ibx
	my $msgs;
	my $pr = $self->{-opt}->{-progress};
	local $self->{-regen_fmt} = "$ekey checking %u/$max\n";
	$self->{nrec} = 0;
	my $fast = $self->{-opt}->{fast};
	my $usr; # _unref_stale_range (< $lo) called
	my ($lo, $hi);
	while (scalar(@{$msgs = $ibx->over->query_xover($beg, $end, $opt)})) {
		$self->{nrec} = $beg;
		$beg = $msgs->[-1]->{num} + 1;
		$end = $beg + $slice;
		$end = $max if $end > $max;
		update_checkpoint $self and
			reindex_checkpoint($self); # release lock
		($lo, $hi) = ($msgs->[0]->{num}, $msgs->[-1]->{num});
		$usr //= _unref_stale_range($self, $ibx, "xnum < $lo");
		my $x3a = $self->{oidx}->dbh->selectall_arrayref(
			<<"", undef, $ibx_id, $lo, $hi);
SELECT xnum,oidbin,docid FROM xref3 WHERE
ibx_id = ? AND xnum >= ? AND xnum <= ?

		my %x3m;
		for (@$x3a) {
			my $k = pack('J', $_->[0]) . $_->[1];
			push @{$x3m{$k}}, $_->[2];
		}
		undef $x3a;
		for my $xsmsg (@$msgs) {
			my $k = pack('JH*', $xsmsg->{num}, $xsmsg->{blob});
			my $docids = delete($x3m{$k});
			if (!defined($docids)) {
				reindex_unseen($self, $ibx, $xsmsg);
			} elsif (!$fast) {
				for my $num (@$docids) {
					$self->{oidx}->eidxq_add($num);
				}
			}
			return if $self->{quit};
		}
		next unless scalar keys %x3m;
		$self->git->async_wait_all; # wait for reindex_unseen

		# eliminate stale/mismatched entries
		my %mismatch = map { $_->{num} => $_->{blob} } @$msgs;
		while (my ($k, $docids) = each %x3m) {
			my ($xnum, $hex) = unpack('JH*', $k);
			my $bin = pack('H*', $hex);
			my $exp = $mismatch{$xnum};
			if (defined $exp) {
				my $smsg = $ibx->over->get_art($xnum) // next;
				# $xnum may be expired by another process
				if ($smsg->{blob} eq $hex) {
					warn <<"";
BUG: (non-fatal) $ekey #$xnum $smsg->{blob} still matches (old exp: $exp)

					next;
				} # else: continue to unref
			}
			my $m = defined($exp) ? "mismatch (!= $exp)" : 'stale';
			warn("# $xnum:$hex (#@$docids): $m\n");
			for my $i (@$docids) {
				_unref_doc $self, $i, $ibx, $xnum, $bin;
			}
			return if $self->{quit};
		}
	}
	defined($hi) and ($hi < $max) and
		_unref_stale_range($self, $ibx, "xnum > $hi AND xnum <= $max");
}

sub _reindex_inbox ($$) {
	my ($self, $ibx) = @_;
	my $ekey = $ibx->eidx_key;
	local $self->{current_info} = $ekey;
	if (defined(my $err = _ibx_index_reject($ibx))) {
		warn "W: cannot reindex $ekey ($err)\n";
	} else {
		_reindex_check_ibx($self, $ibx);
	}
	delete @$ibx{qw(over mm search git)}; # won't need these for a bit
}

sub eidx_reindex ($) {
	my ($self) = @_;
	return unless $self->{cfg};

	# acquire eidxq_lock early because full reindex takes forever
	# and incremental -extindex processes can run during our checkpoints
	if (!eidxq_lock_acquire($self)) {
		warn "E: aborting --reindex\n";
		return;
	}
	for my $ibx (@{ibx_sorted($self, 'active')}) {
		_reindex_inbox($self, $ibx);
		last if $self->{quit};
	}
	$self->git->async_wait_all; # ensure eidxq gets filled completely
	eidxq_process $self unless $self->{quit};
}

sub sync_inbox ($$) {
	my ($self, $ibx) = @_;
	my $err = _sync_inbox $self, $ibx;
	delete @$ibx{qw(mm over)};
	warn $err, "\n" if defined($err);
	$err;
}

sub dd_smsg { # git->cat_async callback
	my ($bref, $oid, $type, $size, $dd) = @_;
	my $smsg = $dd->{smsg} // die 'BUG: dd->{smsg} missing';
	my $self = $dd->{self} // die 'BUG: {self} missing';
	if ($type eq 'missing') {
		_blob_missing($dd, $smsg);
	} elsif (!is_bad_blob($oid, $type, $size, $smsg->{blob})) {
		local $self->{current_info} = "$self->{current_info} $oid";
		my $chash = content_hash(PublicInbox::Eml->new($bref));
		push @{$self->{dd_chash}->{$chash}}, $smsg;
	}
	return if $self->{last_smsg} != $smsg;
	while (my ($chash, $ary) = each %{$self->{dd_chash}}) {
		my $keep = shift @$ary;
		next if !scalar(@$ary);
		$self->{dedupe_cull} += scalar(@$ary);
		print STDERR
			"# <$keep->{mid}> keeping #$keep->{num}, dropping ",
			join(', ', map { "#$_->{num}" } @$ary),"\n";
		next if $self->{-opt}->{'dry-run'};
		my $oidx = $self->{oidx};
		for my $smsg (@$ary) {
			my $gone = $smsg->{num};
			$oidx->merge_xref3($keep->{num}, $gone, $smsg->oidbin);
			remove_doc($self, $gone);
		}
	}
}

sub eidx_dedupe ($$) {
	my ($self, $msgids) = @_;
	local $self->{dedupe_cull} = 0;
	my $candidates = 0;
	my $nr_mid = 0;
	return unless eidxq_lock_acquire($self);
	my ($iter, $cur_mid);
	my $min_id = 0;
	my $idx = 0;
	my ($max_id) = $self->{oidx}->dbh->selectrow_array(<<EOS);
SELECT MAX(id) FROM msgid
EOS
	local $self->{-regen_fmt} = "dedupe %u/$max_id\n";

	# note: we could write this query more intelligently,
	# but that causes lock contention with read-only processes
dedupe_restart:
	$cur_mid = $msgids->[$idx];
	if ($cur_mid eq '') { # all Message-IDs
		$iter = $self->{oidx}->dbh->prepare(<<EOS);
SELECT mid,id FROM msgid WHERE id > ? ORDER BY id ASC
EOS
		$iter->execute($min_id);
	} else {
		$iter = $self->{oidx}->dbh->prepare(<<EOS);
SELECT mid,id FROM msgid WHERE mid = ? AND id > ? ORDER BY id ASC
EOS
		$iter->execute($cur_mid, $min_id);
	}
	while (my ($mid, $id) = $iter->fetchrow_array) {
		last if $self->{quit};
		$self->{current_info} = "dedupe $mid";
		$self->{nrec} = $min_id = $id;
		my ($prv, @smsg);
		while (my $x = $self->{oidx}->next_by_mid($mid, \$id, \$prv)) {
			push @smsg, $x;
		}
		next if scalar(@smsg) < 2;
		# per-msgid data:
		local $self->{dd_chash} = {}; # chash => [ary of smsgs]
		local $self->{last_smsg} = $smsg[-1];
		$nr_mid++;
		$candidates += scalar(@smsg) - 1;
		for my $smsg (@smsg) {
			my $dd = { smsg => $smsg, self => $self };
			$self->git->cat_async($smsg->{blob}, \&dd_smsg, $dd);
		}
		# need to wait on every single one @smsg contents can get
		# invalidated inside dd_smsg for messages with multiple
		# Message-IDs.
		$self->git->async_wait_all;

		if (update_checkpoint $self) {
			undef $iter;
			reindex_checkpoint($self);
			goto dedupe_restart;
		}
	}
	goto dedupe_restart if defined($msgids->[++$idx]);

	my $n = delete $self->{dedupe_cull};
	if (my $pr = $self->{-opt}->{-progress}) {
		$pr->("culled $n/$candidates candidates ($nr_mid msgids)\n");
	}
	$self->{nrec} = 0;
}

sub eidx_sync ($$) { # main entry point
	my ($self, $opt) = @_;

	local $self->{current_info} = '';
	local $SIG{__WARN__} = PublicInbox::Admin::warn_cb $self;
	$self->idx_init($opt); # acquire lock via V2Writable::_idx_init
	$self->{oidx}->rethread_prepare($opt);
	local $self->{need_checkpoint} = 0;
	local $self->{nrec} = 0;
	local $self->{-opt} = $opt;
	local $self->{-regen_fmt} = "%u/?\n";
	local $self->{latest_cmt};
	local $SIG{USR1} = sub { $self->{need_checkpoint} = 1 };
	my $quit = PublicInbox::SearchIdx::quit_cb $self;
	local $SIG{QUIT} = $quit;
	local $SIG{INT} = $quit;
	local $SIG{TERM} = $quit;
	for my $ibx (@{ibx_sorted($self, 'known')}) {
		$ibx->{-ibx_id} //= $self->{oidx}->ibx_id($ibx->eidx_key);
	}
	if (grep { defined($_->{boost}) } @{$self->{ibx_known}}) {
		$self->{id2pos} //= prep_id2pos $self;
		$self->{boost_in_use} = 1;
	}

	if (my $msgids = delete($opt->{dedupe})) {
		local $self->{checkpoint_unlocks} = 1;
		eidx_dedupe $self, $msgids;
	}
	if (delete($opt->{reindex})) {
		local $self->{checkpoint_unlocks} = 1;
		eidx_reindex $self;
	}

	# don't use $_ here, it'll get clobbered by reindex_checkpoint
	if ($opt->{scan} // 1) {
		for my $ibx (@{ibx_sorted($self, 'active')}) {
			last if $self->{quit};
			sync_inbox $self, $ibx;
		}
	}
	$self->{oidx}->rethread_done($opt) unless $self->{quit};
	eidxq_process $self unless $self->{quit};

	eidxq_release $self;
	done($self);
}

sub update_last_commit { # overrides V2Writable
	my ($self, $stk) = @_;
	my $unit = $self->{unit} // return;
	my $latest_cmt = $stk ? $stk->{latest_cmt} : $self->{latest_cmt};
	defined($latest_cmt) or return;
	my $ibx = $self->{ibx} or die 'BUG: {ibx} missing';
	my $ekey = $ibx->eidx_key;
	my $uv = $ibx->uidvalidity;
	my $epoch = $unit->{epoch};
	my $meta_key;
	my $v = $ibx->version;
	if ($v == 2) {
		die 'No {epoch} for v2 unit' unless defined $epoch;
		$meta_key = "lc-v2:$ekey//$uv;$epoch";
	} elsif ($v == 1) {
		die 'Unexpected {epoch} for v1 unit' if defined $epoch;
		$meta_key = "lc-v1:$ekey//$uv";
	} else {
		die "Unsupported inbox version: $v";
	}
	my $last = $self->{oidx}->eidx_meta($meta_key);
	if (defined $last && is_ancestor($self->git, $last, $latest_cmt)) {
		my @cmd = (qw(rev-list --count), "$last..$latest_cmt");
		chomp(my $n = $unit->{git}->qx(@cmd));
		return if $n ne '' && $n == 0;
	}
	$self->{oidx}->eidx_meta($meta_key, $latest_cmt);
}

sub symlink_packs ($$) {
	my ($ibx, $pd) = @_;
	my $ret = 0;
	my $glob = "$ibx->{inboxdir}/git/*.git/objects/pack/*.idx";
	for my $idx (bsd_glob($glob, GLOB_NOSORT)) {
		my $src = substr($idx, 0, -length('.idx'));
		my $dst = $pd . substr($src, rindex($src, '/'));
		if (-f "$src.pack" and
				symlink("$src.pack", "$dst.pack") and
				symlink($idx, "$dst.idx") and
				-f $idx) {
			++$ret;
			# .promisor, .bitmap, .rev and .keep are optional
			# XXX should we symlink .keep here?
			for my $s (qw(promisor bitmap rev)) {
				symlink("$src.$s", "$dst.$s") if -f "$src.$s";
			}
		} elsif (!$!{EEXIST}) {
			warn "W: ln -s $src.{pack,idx} => $dst.*: $!\n";
			unlink "$dst.pack", "$dst.idx";
		}
	}
	$ret;
}

sub idx_init { # similar to V2Writable
	my ($self, $opt) = @_;
	return if $self->{idx_shards};

	$self->git->cleanup;
	my $mode = 0644;
	my $ALL = $self->git->{git_dir}; # topdir/ALL.git
	my ($has_new, $alt, $seen, $prune, $prune_nr);
	if ($opt->{-private}) { # LeiStore
		my $local = "$self->{topdir}/local"; # lei/store
		$self->{mg} //= PublicInbox::MultiGit->new($self->{topdir},
							'ALL.git', 'local');
		$mode = 0600;
		unless (-d $ALL) {
			umask 077; # don't bother restoring for lei
			PublicInbox::Import::init_bare($ALL);
			$self->git->qx(qw(config core.sharedRepository 0600));
		}
		($alt, $seen) = $self->{mg}->read_alternates(\$mode);
		$has_new = $self->{mg}->merge_epochs($alt, $seen);
	} else { # extindex has no epochs
		$self->{mg} //= PublicInbox::MultiGit->new($self->{topdir},
							'ALL.git');
		$prune = $opt->{-idx_gc} ? \$prune_nr : undef;
		($alt, $seen) = $self->{mg}->read_alternates(\$mode, $prune);
		PublicInbox::Import::init_bare($ALL);
	}

	# git-multi-pack-index(1) can speed up "git cat-file" startup slightly
	my $git_midx = 0;
	my $pd = "$ALL/objects/pack";
	if (opendir(my $dh, $pd)) { # drop stale symlinks
		while (defined(my $dn = readdir($dh))) {
			if ($dn =~ /\.(?:idx|pack|promisor|bitmap|rev)\z/) {
				my $f = "$pd/$dn";
				unlink($f) if -l $f && !-e $f;
			}
		}
	} elsif ($!{ENOENT}) {
		mkdir $pd;
	} else {
		die "opendir($pd): $!";
	}
	my $new = '';
	for my $ibx (@{ibx_sorted($self, 'active')}) {
		# create symlinks for multi-pack-index
		$git_midx += symlink_packs($ibx, $pd);
		# add new lines to our alternates file
		my $d = $ibx->git->{git_dir} . '/objects';
		next if exists $alt->{$d};
		if (my @st = stat($d)) {
			next if $seen->{"$st[0]\0$st[1]"}++;
		} else {
			warn "W: stat($d) failed (from $ibx->{inboxdir}): $!\n";
			next if $opt->{-idx_gc};
		}
		$new .= "$d\n";
	}
	($has_new || $prune_nr || $new ne '') and
		$self->{mg}->write_alternates($mode, $alt, $new);
	my $restore = $self->with_umask;
	if ($git_midx && ($opt->{'multi-pack-index'} // 1)) {
		my $cmd = $self->git->cmd('multi-pack-index');
		push @$cmd, '--no-progress' if ($opt->{quiet}//0) > 1;
		push @$cmd, 'write';
		my $lk = $self->lock_for_scope;
		run_wait $cmd;
		# ignore errors, fairly new command, may not exist
	}
	$self->parallel_init($self->{indexlevel});
	PublicInbox::V2Writable::_idx_init($self, $opt); # acquires ei.lock
	$self->{midx} = PublicInbox::MiscIdx->new($self);
	$self->{oidx}->begin_lazy;
	$self->{oidx}->eidx_prep;
	$self->{midx}->create_xdb if $new ne '';
}

sub _watch_commit { # PublicInbox::DS::add_timer callback
	my ($self) = @_;
	delete $self->{-commit_timer};
	eidxq_process $self;
	eidxq_release $self;
	my $fmt = delete $self->{-regen_fmt};
	reindex_checkpoint($self);
	$self->{-regen_fmt} = $fmt;

	# call event_step => done unless commit_timer is armed
	PublicInbox::DS::requeue($self);
}

sub on_inbox_unlock { # called by PublicInbox::InboxIdle
	my ($self, $ibx) = @_;
	my $opt = $self->{-opt};
	my $pr = $opt->{-progress};
	my $ekey = $ibx->eidx_key;
	local $0 = "sync $ekey";
	$pr->("indexing $ekey\n") if $pr;
	$self->idx_init($opt);
	sync_inbox $self, $ibx;
	$self->{-commit_timer} //= add_timer($opt->{'commit-interval'} // 10,
					\&_watch_commit, $self);
}

sub eidx_reload { # -extindex --watch SIGHUP handler
	my ($self, $idler) = @_;
	if ($self->{cfg}) {
		my $pr = $self->{-opt}->{-progress};
		$pr->('reloading ...') if $pr;
		delete $self->{-resync_queue};
		delete $self->{-ibx_ary_known};
		delete $self->{-ibx_ary_active};
		delete $self->{id2pos};
		delete $self->{boost_in_use};
		$self->{ibx_known} = [];
		$self->{ibx_active} = [];
		%{$self->{ibx_map}} = ();
		my $cfg = PublicInbox::Config->new;
		attach_config($self, $cfg);
		$idler->refresh($cfg);
		$pr->(" done\n") if $pr;
	} else {
		warn "reload not supported without --all\n";
	}
}

sub eidx_resync_start ($) { # -extindex --watch SIGUSR1 handler
	my ($self) = @_;
	$self->{-resync_queue} //= [ @{ibx_sorted($self, 'active')} ];
	PublicInbox::DS::requeue($self); # trigger our ->event_step
}

sub event_step { # PublicInbox::DS::requeue callback
	my ($self) = @_;
	if (my $resync_queue = $self->{-resync_queue}) {
		if (my $ibx = shift(@$resync_queue)) {
			on_inbox_unlock($self, $ibx);
			PublicInbox::DS::requeue($self);
		} else {
			delete $self->{-resync_queue};
			_watch_commit($self);
		}
	} else {
		done($self) unless $self->{-commit_timer};
	}
}

# FIXME: totally untested and undocumented
sub eidx_watch { # public-inbox-extindex --watch main loop
	my ($self, $opt) = @_;
	local @SIG{keys %SIG} = values %SIG;
	for my $sig (qw(HUP USR1 TSTP QUIT INT TERM)) {
		$SIG{$sig} = sub { warn "SIG$sig ignored while scanning\n" };
	}
	require PublicInbox::InboxIdle;
	require PublicInbox::DS;
	require PublicInbox::Syscall;
	my $idler = PublicInbox::InboxIdle->new($self->{cfg});
	if (!$self->{cfg}) {
		$idler->watch_inbox($_) for (@{ibx_sorted($self, 'active')});
	}
	for my $ibx (@{ibx_sorted($self, 'active')}) {
		$ibx->subscribe_unlock(__PACKAGE__, $self)
	}
	my $pr = $opt->{-progress};
	$pr->("performing initial scan ...\n") if $pr;
	local $self->{-opt} = $opt;
	eidx_sync $self, $opt; # initial sync
	return if $self->{quit};
	my $oldset = PublicInbox::DS::block_signals();
	local $self->{current_info} = '';
	local $SIG{__WARN__} = PublicInbox::Admin::warn_cb $self;
	my $sig = {
		HUP => sub { eidx_reload($self, $idler) },
		USR1 => sub { eidx_resync_start($self) },
		TSTP => sub { kill('STOP', $$) },
	};
	my $quit = PublicInbox::SearchIdx::quit_cb $self;
	$sig->{QUIT} = $sig->{INT} = $sig->{TERM} = $quit;
	local @PublicInbox::DS::post_loop_do = (sub { !$self->{quit} });
	$pr->("initial scan complete, entering event loop\n") if $pr;
	# calls InboxIdle->event_step:
	PublicInbox::DS::event_loop($sig, $oldset);
	done($self);
}

no warnings 'once';
*done = \&PublicInbox::V2Writable::done;
*parallel_init = \&PublicInbox::V2Writable::parallel_init;
*sync_prepare = \&PublicInbox::V2Writable::sync_prepare;
*index_todo = \&PublicInbox::V2Writable::index_todo;
*count_shards = \&PublicInbox::V2Writable::count_shards;
*atfork_child = \&PublicInbox::V2Writable::atfork_child;
*idx_shard = \&PublicInbox::V2Writable::idx_shard;
*reindex_checkpoint = \&PublicInbox::V2Writable::reindex_checkpoint;
*checkpoint = \&PublicInbox::V2Writable::checkpoint;
*barrier = \&PublicInbox::V2Writable::barrier;

1;
