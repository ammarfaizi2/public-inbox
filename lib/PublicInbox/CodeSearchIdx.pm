# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# indexer for git coderepos, just commits and repo paths for now
# this stores normalized absolute paths of indexed GIT_DIR inside
# the DB itself and is designed to handle forks by designating roots
#
# Unlike mail search, docid isn't tied to NNTP artnum or IMAP UID,
# there's no serial number dependency at all.  The first 32-bits of
# the commit SHA-(1|256) is used to select a shard.
#
# We shard repos using the first 32-bits of sha256($ABS_GIT_DIR)
#
# --associate joins root commits of coderepos to inboxes based on prefixes.
#
# Internally, each inbox is assigned a non-negative integer index ($IBX_ID),
# and each root commit object ID (SHA-1/SHA-256 hex) is also assigned
# a non-negative integer index ($ROOT_COMMIT_OID_ID).
#
# associate dumps to 2 intermediate files in $TMPDIR:
#
# * to_root_id - each line is of the format:
#
#	$PFX $ROOT_COMMIT_OID_IDS
#
# * to_ibx_id - each line is of the format:
#
#	$PFX $IBX_IDS
#
# $IBX_IDS is a comma-delimited list of integers ($IBX_ID)
# $ROOT_COMMIT_OID_IDS is space-delimited
# In both cases, $PFX is typically the value of the patchid (XDFID) but it
# can be configured to use any combination of patchid, dfpre, dfpost or
# dfblob.
#
# See PublicInbox::CodeSearch (read-only API) for more
package PublicInbox::CodeSearchIdx;
use v5.12;
# parent order matters, we want ->DESTROY from IPC, not SearchIdx
use parent qw(PublicInbox::CodeSearch PublicInbox::IPC PublicInbox::SearchIdx);
use PublicInbox::DS qw(awaitpid);
use PublicInbox::PktOp;
use PublicInbox::IPC qw(nproc_shards);
use POSIX qw(WNOHANG SEEK_SET);
use File::Path ();
use File::Spec ();
use List::Util qw(max);
use PublicInbox::SHA qw(sha256_hex sha_all);
use PublicInbox::Search qw(xap_terms);
use PublicInbox::SearchIdx qw(add_val);
use PublicInbox::Config qw(glob2re rel2abs_collapsed);
use PublicInbox::Spawn qw(which spawn popen_rd);
use PublicInbox::OnDestroy;
use PublicInbox::CidxLogP;
use PublicInbox::CidxComm;
use PublicInbox::Git qw(%OFMT2HEXLEN);
use PublicInbox::Compat qw(uniqstr);
use PublicInbox::Aspawn qw(run_await);
use Carp ();
use autodie qw(close pipe open sysread seek sysseek send);
our $DO_QUIT = 15; # signal number
our (
	$LIVE_JOBS, # integer
	$GITS_NR, # number of coderepos
	$MY_SIG, # like %SIG
	$SIGSET,
	$TXN_BYTES, # number of bytes in current shard transaction
	$BATCH_BYTES,
	@RDONLY_XDB, # Xapian::Database
	@IDX_SHARDS, # clones of self
	$MAX_SIZE,
	$REINDEX, # PublicInbox::SharedKV
	@GIT_DIR_GONE, # [ git_dir1, git_dir2 ]
	$PRUNE_DONE, # marks off prune completions
	$NCHANGE, # current number of changes
	$NPROC,
	$XHC, # XapClient
	$REPO_CTX, # current repo being indexed in shards
	$IDX_TODO, # [ $git0, $root0, $git1, $root1, ...]
	$GIT_TODO, # [ GIT_DIR0, GIT_DIR1, ...]
	%ALT_FH, # hexlen => tmp IO for TMPDIR git alternates
	$TMPDIR, # File::Temp->newdir object for prune
	@PRUNE_QUEUE, # GIT_DIRs to prepare for pruning
	%TODO, @IBXQ, @IBX,
	@JOIN, # join(1) command for associate
	$CMD_ENV, # env for awk(1), comm(1), sort(1) commands during prune
	@AWK, @COMM, @SORT, # awk(1), comm(1), sort(1) commands
	@ASSOC_PFX, # any combination of XDFID, XDFPRE, XDFPOST
	$QRY_STR, # common query string for both code and inbox associations
	$DUMP_IBX_WPIPE, # goes to sort(1)
	@ID2ROOT,
);

# stop walking history if we see >$SEEN_MAX existing commits, this assumes
# branches don't diverge by more than this number of commits...
# git walks commits quickly if it doesn't have to read trees
our $SEEN_MAX = 100000;

# window for commits/emails to determine a inbox <-> coderepo association
my $ASSOC_MAX = 50000;

our @PRUNE_BATCH = qw(git _ cat-file --batch-all-objects --batch-check);

# TODO: do we care about committer name + email? or tree OID?
my @FMT = qw(H P ct an ae at s b); # (b)ody must be last

# git log --stdin buffers all commits before emitting, thus --reverse
# doesn't incur extra overhead.  We use --reverse to keep Xapian docids
# increasing so we may be able to avoid sorting results in some cases
my @LOG_STDIN = (qw(log --no-decorate --no-color --no-notes -p --stat -M
	--reverse --stdin --no-walk=unsorted), '--pretty=format:%n%x00'.
	join('%n', map { "%$_" } @FMT));

sub new {
	my (undef, $dir, $opt) = @_;
	my $l = $opt->{indexlevel} // 'full';
	$l !~ $PublicInbox::SearchIdx::INDEXLEVELS and
		die "invalid indexlevel=$l\n";
	$l eq 'basic' and die "E: indexlevel=basic not supported\n";
	my $self = bless {
		xpfx => "$dir/cidx".  PublicInbox::CodeSearch::CIDX_SCHEMA_VER,
		cidx_dir => $dir,
		creat => 1, # TODO: get rid of this, should be implicit
		transact_bytes => 0, # for checkpoint
		total_bytes => 0, # for lock_release
		current_info => '',
		parallel => 1,
		-opt => $opt,
		lock_path => "$dir/cidx.lock",
	}, __PACKAGE__;
	$self->{nshard} = count_shards($self) ||
		nproc_shards({nproc => $opt->{jobs}});
	$self->{-no_fsync} = 1 if !$opt->{fsync};
	$self->{-dangerous} = 1 if $opt->{dangerous};
	$self;
}

# This is similar to uniq(1) on the first column, but combines the
# contents of subsequent columns using $OFS.
our @UNIQ_FOLD = ($^X, $^W ? ('-w') : (), qw(-MList::Util=uniq -ane), <<'EOM');
BEGIN { $ofs = $ENV{OFS} // ','; $apfx = '' }
if ($F[0] eq $apfx) {
	shift @F;
	push @ids, @F;
} else {
	print $apfx.' '.join($ofs, uniq(@ids))."\n" if @ids;
	($apfx, @ids) = @F;
}
END { print $apfx.' '.join($ofs, uniq(@ids))."\n" if @ids }
EOM

# TODO: may be used for reshard/compact
sub count_shards { scalar($_[0]->xdb_shards_flat) }

sub update_commit ($$) {
	my ($self, $cmt) = @_; # fields from @FMT
	my $x = 'Q'.$cmt->{H};
	my ($docid, @extra) = sort { $a <=> $b } docids_by_postlist($self, $x);
	@extra and warn "W: $cmt->{H} indexed multiple times, pruning ",
			join(', ', map { "#$_" } @extra), "\n";
	$self->{xdb}->delete_document($_) for @extra;
	my $doc = $PublicInbox::Search::X{Document}->new;
	$doc->add_boolean_term($x);
	$doc->add_boolean_term('G'.$_) for @{$self->{roots}};
	$doc->add_boolean_term('XP'.$_) for split(/ /, $cmt->{P});
	$doc->add_boolean_term('T'.'c');

	# Author-Time is compatible with dt: for mail search schema_version=15
	add_val($doc, PublicInbox::CodeSearch::AT,
		POSIX::strftime('%Y%m%d%H%M%S', gmtime($cmt->{at})));

	# Commit-Time is the fallback used by rt: (TS) for mail search:
	add_val($doc, PublicInbox::CodeSearch::CT, $cmt->{ct});

	$self->term_generator->set_document($doc);

	# email address is always indexed with positional data for usability
	$self->index_phrase("$cmt->{an} <$cmt->{ae}>", 1, 'A');

	$x = $cmt->{'s'};
	$self->index_text($x, 1, 'S') if $x =~ /\S/s;
	$doc->set_data($x); # subject is the first (and currently only) line

	$x = delete $cmt->{b};
	$self->index_body_text($doc, \$x) if $x =~ /\S/s;
	defined($docid) ? $self->{xdb}->replace_document($docid, $doc) :
			$self->{xdb}->add_document($doc);
}

sub progress {
	my ($self, @msg) = @_;
	my $pr = $self->{-opt}->{-progress} or return;
	$pr->($self->{git} ? ("$self->{git}->{git_dir}: ") : (), @msg, "\n");
}

sub store_repo { # wq_io_do, sends docid back
	my ($self, $repo) = @_;
	my $op_p = delete($self->{0}) // die 'BUG: no {0} op_p';
	$self->begin_txn_lazy;
	$self->{xdb}->delete_document($_) for @{$repo->{to_delete}};
	my $doc = $PublicInbox::Search::X{Document}->new;
	add_val($doc, PublicInbox::CodeSearch::CT, $repo->{ct});
	$doc->add_boolean_term("P$repo->{git_dir}");
	$doc->add_boolean_term('T'.'r');
	$doc->add_boolean_term('G'.$_) for @{$repo->{roots}};
	$doc->set_data($repo->{fp}); # \n delimited
	my $did = $repo->{docid};
	$did ? $self->{xdb}->replace_document($did, $doc)
		: ($did = $self->{xdb}->add_document($doc));
	send($op_p, "repo_stored $did", 0);
}

sub cidx_ckpoint ($;$) {
	my ($self, $msg) = @_;
	progress($self, $msg) if defined($msg);
	$TXN_BYTES = $BATCH_BYTES; # reset
	return if $PublicInbox::Search::X{CLOEXEC_UNSET};
	$self->commit_txn_lazy;
	$self->begin_txn_lazy;
}

sub truncate_cmt ($$) {
	my ($cmt) = @_; # _[1] is $buf (giant)
	my ($orig_len, $len);
	$len = $orig_len = length($_[1]);
	@$cmt{@FMT} = split(/\n/, $_[1], scalar(@FMT));
	undef $_[1];
	$len -= length($cmt->{b});

	# try to keep the commit message body.
	# n.b. this diffstat split may be unreliable but it's not worth
	# perfection for giant commits:
	my ($bdy) = split(/^---\n/sm, delete($cmt->{b}), 2);
	if (($len + length($bdy)) <= $MAX_SIZE) {
		$len += length($bdy);
		$cmt->{b} = $bdy;
		warn <<EOM;
W: $cmt->{H}: truncated body ($orig_len => $len bytes)
W: to be under --max-size=$MAX_SIZE
EOM
	} else {
		$cmt->{b} = '';
		warn <<EOM;
W: $cmt->{H}: deleted body ($orig_len => $len bytes)
W: to be under --max-size=$MAX_SIZE
EOM
	}
	$len;
}

sub cidx_reap_log { # awaitpid cb
	my ($pid, $cmd, $self, $op_p) = @_;
	if (!$? || ($DO_QUIT && (($? & 127) == $DO_QUIT ||
				($? & 127) == POSIX::SIGPIPE))) {
		send($op_p, "shard_done $self->{shard}", 0);
	} else {
		warn "W: @$cmd (\$?=$?)\n";
		$self->{xdb}->cancel_transaction;
	}
}

sub shard_index { # via wq_io_do in IDX_SHARDS
	my ($self, $git, $roots) = @_;

	my $in = delete($self->{0}) // die 'BUG: no {0} input';
	my $op_p = delete($self->{1}) // die 'BUG: no {1} op_p';
	sysseek($in, 0, SEEK_SET);
	my $cmd = $git->cmd(@LOG_STDIN);
	my $rd = popen_rd($cmd, undef, { 0 => $in },
				\&cidx_reap_log, $cmd, $self, $op_p);
	PublicInbox::CidxLogP->new($rd, $self, $git, $roots);
	# CidxLogP->event_step will call cidx_read_log_p once there's input
}

# sharded reader for `git log --pretty=format: --stdin'
sub cidx_read_log_p {
	my ($self, $log_p, $rd) = @_;
	my $git = delete $log_p->{git} // die 'BUG: no {git}';
	local $self->{current_info} = "$git->{git_dir} [$self->{shard}]";
	local $self->{roots} = delete $log_p->{roots} // die 'BUG: no {roots}';

	local $MAX_SIZE = $self->{-opt}->{max_size};
	# local-ized in parent before fork
	$TXN_BYTES = $BATCH_BYTES;
	local $self->{git} = $git; # for patchid
	return if $DO_QUIT;
	my $nr = 0;

	# a patch may have \0, see c4201214cbf10636e2c1ab9131573f735b42c8d4
	# in linux.git, so we use $/ = "\n\0" to check end-of-patch
	my $FS = "\n\0";
	my $len;
	my $cmt = {};
	local $/ = $FS;
	my $buf = <$rd> // return; # leading $FS
	$buf eq $FS or die "BUG: not LF-NUL: $buf\n";
	$self->begin_txn_lazy;
	while (!$DO_QUIT && defined($buf = <$rd>)) {
		chomp($buf);
		$/ = "\n";
		$len = length($buf);
		if (defined($MAX_SIZE) && $len > $MAX_SIZE) {
			$len = truncate_cmt($cmt, $buf);
		} else {
			@$cmt{@FMT} = split(/\n/, $buf, scalar(@FMT));
		}
		if (($TXN_BYTES -= $len) <= 0) {
			cidx_ckpoint($self, "[$self->{shard}] $nr");
			$TXN_BYTES -= $len; # len may be huge, >TXN_BYTES;
		}
		update_commit($self, $cmt);
		++$nr;
		cidx_ckpoint($self, "[$self->{shard}] $nr") if $TXN_BYTES <= 0;
		$/ = $FS;
	}
	# return and wait for cidx_reap_log
}

sub shard_done { # called via PktOp on shard_index completion
	my ($self, $repo_ctx, $on_destroy, $n) = @_;
	$repo_ctx->{shard_ok}->{$n} = 1;
}

sub repo_stored {
	my ($self, $repo_ctx, $did) = @_;
	$did > 0 or die "BUG: $repo_ctx->{repo}->{git_dir}: docid=$did";
	my $next = PublicInbox::OnDestroy->new($$, \&next_repos, $repo_ctx);
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{shard_done} = [ $self, $repo_ctx, $next ];
	# shard_done fires when all shards are committed
	my @active = keys %{$repo_ctx->{active}};
	$IDX_SHARDS[$_]->wq_io_do('shard_commit', [ $p->{op_p} ]) for @active;
}

sub prune_done { # called via prune_do completion
	my ($self, $n) = @_;
	return if $DO_QUIT || !$PRUNE_DONE;
	die "BUG: \$PRUNE_DONE->[$n] already defined" if $PRUNE_DONE->[$n];
	$PRUNE_DONE->[$n] = 1;
	grep(defined, @$PRUNE_DONE) == @IDX_SHARDS and
		progress($self, 'prune done')
}

sub seen ($$) {
	my ($xdb, $q) = @_; # $q = "Q$COMMIT_HASH"
	for (1..100) {
		my $ret = eval {
			$xdb->postlist_begin($q) != $xdb->postlist_end($q);
		};
		return $ret unless $@;
		if (ref($@) =~ /\bDatabaseModifiedError\b/) {
			$xdb->reopen;
		} else {
			Carp::croak($@);
		}
	}
	Carp::croak('too many Xapian DB modifications in progress');
}

# used to select the shard for a GIT_DIR
sub git_dir_hash ($) { hex(substr(sha256_hex($_[0]), 0, 8)) }

sub docids_by_postlist ($$) { # consider moving to PublicInbox::Search
	my ($self, $q) = @_;
	my $cur = $self->{xdb}->postlist_begin($q);
	my $end = $self->{xdb}->postlist_end($q);
	my @ids;
	for (; $cur != $end; $cur++) { push(@ids, $cur->get_docid) };
	@ids;
}

sub _cb { # run_await cb
	my ($pid, $cmd, undef, $opt, $cb, $self, $git, @arg) = @_;
	return if $DO_QUIT;
	($git->{-cidx_err} = $?) ? warn("W: @$cmd (\$?=$?)\n") :
				$cb->($opt, $self, $git, @arg);
}

sub run_git {
	my ($cmd, $opt, $cb, $self, $git, @arg) = @_;
	run_await($git->cmd(@$cmd), undef, $opt, \&_cb, $cb, $self, $git, @arg)
}

# this is different from the grokmirror-compatible fingerprint since we
# only care about --heads (branches) and --tags, and not even their names
sub fp_start ($$$) {
	my ($self, $git, $prep_repo) = @_;
	return if $DO_QUIT;
	open my $refs, '+>', undef;
	$git->{-repo}->{refs} = $refs;
	run_git([qw(show-ref --heads --tags --hash)], { 1 => $refs },
		\&fp_fini, $self, $git, $prep_repo);
}

sub fp_fini { # run_git cb
	my (undef, $self, $git, $prep_repo) = @_;
	my $refs = $git->{-repo}->{refs} // die 'BUG: no {-repo}->{refs}';
	sysseek($refs, 0, SEEK_SET);
	$git->{-repo}->{fp} = sha_all(256, $refs)->hexdigest;
}

sub ct_start ($$$) {
	my ($self, $git, $prep_repo) = @_;
	return if $DO_QUIT;
	run_git([ qw[for-each-ref --sort=-committerdate
		--format=%(committerdate:raw) --count=1
		refs/heads/ refs/tags/] ], undef, # capture like qx
		\&ct_fini, $self, $git, $prep_repo);
}

sub ct_fini { # run_git cb
	my ($opt, $self, $git, $prep_repo) = @_;
	my ($ct) = split(/\s+/, ${$opt->{1}}); # drop TZ + LF
	$git->{-repo}->{ct} = $ct + 0;
}

# TODO: also index gitweb.owner and the full fingerprint for grokmirror?
sub prep_repo ($$) {
	my ($self, $git) = @_;
	return if $DO_QUIT;
	return index_next($self) if $git->{-cidx_err};
	my $repo = $git->{-repo} // die 'BUG: no {-repo}';
	if (!defined($repo->{ct})) {
		warn "W: $git->{git_dir} has no commits, skipping\n";
		delete $git->{-repo};
		return index_next($self);
	}
	my $n = git_dir_hash($git->{git_dir}) % $self->{nshard};
	my $shard = bless { %$self, shard => $n }, ref($self);
	$repo->{shard_n} = $n;
	delete @$shard{qw(lockfh lock_path)};
	local $shard->{xdb} = $RDONLY_XDB[$n] // die "BUG: shard[$n] undef";
	$shard->retry_reopen(\&check_existing, $self, $git);
}

sub check_existing { # retry_reopen callback
	my ($shard, $self, $git) = @_;
	my @docids = docids_by_postlist($shard, 'P'.$git->{git_dir});
	my $docid = shift(@docids) // return get_roots($self, $git);
	my $doc = $shard->get_doc($docid) //
			die "BUG: no #$docid ($git->{git_dir})";
	my $old_fp = $REINDEX ? "\0invalid" : $doc->get_data;
	if ($old_fp eq $git->{-repo}->{fp}) { # no change
		delete $git->{-repo};
		return index_next($self);
	}
	$git->{-repo}->{docid} = $docid;
	if (@docids) {
		warn "BUG: $git->{git_dir} indexed multiple times, culling\n";
		$git->{-repo}->{to_delete} = \@docids; # XXX needed?
	}
	get_roots($self, $git);
}

sub partition_refs ($$$) {
	my ($self, $git, $refs) = @_; # show-ref --heads --tags --hash output
	sysseek($refs, 0, SEEK_SET);
	my $rfh = $git->popen(qw(rev-list --stdin), undef, { 0 => $refs });
	my $seen = 0;
	my @shard_in = map {
		$_->reopen;
		open my $fh, '+>', undef;
		$fh;
	} @RDONLY_XDB;

	my $n0 = $NCHANGE;
	while (defined(my $cmt = <$rfh>)) {
		chomp $cmt;
		my $n = hex(substr($cmt, 0, 8)) % scalar(@RDONLY_XDB);
		if ($REINDEX && $REINDEX->set_maybe(pack('H*', $cmt), '')) {
			say { $shard_in[$n] } $cmt;
			++$NCHANGE;
		} elsif (seen($RDONLY_XDB[$n], 'Q'.$cmt)) {
			last if ++$seen > $SEEN_MAX;
		} else {
			say { $shard_in[$n] } $cmt;
			++$NCHANGE;
			$seen = 0;
		}
		if ($DO_QUIT) {
			$rfh->close;
			return ();
		}
	}
	$rfh->close;
	return () if $DO_QUIT;
	if (!$? || (($? & 127) == POSIX::SIGPIPE && $seen > $SEEN_MAX)) {
		my $n = $NCHANGE - $n0;
		progress($self, "$git->{git_dir}: $n commits") if $n;
		return @shard_in;
	}
	die "git --git-dir=$git->{git_dir} rev-list: \$?=$?\n";
}

sub shard_commit { # via wq_io_do
	my ($self) = @_;
	my $op_p = delete($self->{0}) // die 'BUG: no {0} op_p';
	$self->commit_txn_lazy;
	send($op_p, "shard_done $self->{shard}", 0);
}

sub assoc_max_init ($) {
	my ($self) = @_;
	my $max = $self->{-opt}->{'associate-max'} // $ASSOC_MAX;
	$max = $ASSOC_MAX if !$max;
	$max < 0 ? ((2 ** 31) - 1) : $max;
}

sub dump_roots_start {
	my ($self, $associate) = @_;
	($XHC, my $pid) = PublicInbox::XapClient::start_helper("-j$NPROC");
	awaitpid($pid, \&cmd_done, ['xap_helper', "-j$NPROC"]);
	$associate // die 'BUG: no $associate';
	$TODO{associating} = 1; # keep shards_active() happy
	progress($self, 'dumping IDs from coderepos');
	local $self->{xdb};
	@ID2ROOT = $self->all_terms('G');
	my $root2id = "$TMPDIR/root2id";
	open my $fh, '>', $root2id;
	my $nr = -1;
	for (@ID2ROOT) { print $fh $_, "\0", ++$nr, "\0" } # mmap-friendly
	close $fh;
	# dump_roots | sort -k1,1 | OFS=' ' uniq_fold >to_root_id
	my ($sort_opt, $fold_opt);
	pipe(local $sort_opt->{0}, my $sort_w);
	pipe(local $fold_opt->{0}, local $sort_opt->{1});
	my @sort = (@SORT, '-k1,1');
	my $dst = "$TMPDIR/to_root_id";
	open $fold_opt->{1}, '>', $dst;
	my $fold_env = { %$CMD_ENV, OFS => ' ' };
	run_await(\@sort, $CMD_ENV, $sort_opt, \&cmd_done, $associate);
	run_await(\@UNIQ_FOLD, $fold_env, $fold_opt, \&cmd_done, $associate);
	my @arg = ((map { ('-A', $_) } @ASSOC_PFX), '-c',
		'-m', assoc_max_init($self), $root2id, $QRY_STR);
	for my $d ($self->shard_dirs) {
		pipe(my $err_r, my $err_w);
		$XHC->mkreq([$sort_w, $err_w], qw(dump_roots -d), $d, @arg);
		my $desc = "dump_roots $d";
		$self->{PENDING}->{$desc} = $associate;
		PublicInbox::CidxXapHelperAux->new($err_r, $self, $desc);
	}
	progress($self, 'waiting on dump_roots sort');
}

sub dump_ibx { # sends to xap_helper.h
	my ($self, $ibx_id) = @_;
	my $ibx = $IBX[$ibx_id] // die "BUG: no IBX[$ibx_id]";
	my $ekey = $ibx->eidx_key;
	my $srch = $ibx->isrch or return warn <<EOM;
W: $ekey not indexed for search
EOM
	my @cmd = ('dump_ibx', $srch->xh_args,
			(map { ('-A', $_) } @ASSOC_PFX), $ibx_id, $QRY_STR);
	pipe(my $r, my $w);
	$XHC->mkreq([$DUMP_IBX_WPIPE, $w], @cmd);
	$self->{PENDING}->{$ekey} = $TODO{associate};
	PublicInbox::CidxXapHelperAux->new($r, $self, $ekey);
}

sub dump_ibx_start {
	my ($self, $associate) = @_;
	my ($sort_opt, $fold_opt);
	pipe(local $sort_opt->{0}, $DUMP_IBX_WPIPE);
	pipe(local $fold_opt->{0}, local $sort_opt->{1});
	my @sort = (@SORT, '-k1,1'); # sort only on ASSOC_PFX
	# pipeline: dump_ibx | sort -k1,1 | uniq_fold >to_ibx_id
	open $fold_opt->{1}, '>', "$TMPDIR/to_ibx_id";
	run_await(\@sort, $CMD_ENV, $sort_opt, \&cmd_done, $associate);
	run_await(\@UNIQ_FOLD, $CMD_ENV, $fold_opt, \&cmd_done, $associate);
}

sub index_next ($) {
	my ($self) = @_;
	return if $DO_QUIT;
	if ($IDX_TODO && @$IDX_TODO) {
		index_repo(undef, $self, shift @$IDX_TODO);
	} elsif ($GIT_TODO && @$GIT_TODO) {
		my $git = shift @$GIT_TODO;
		my $prep_repo = PublicInbox::OnDestroy->new($$, \&prep_repo,
							$self, $git);
		fp_start($self, $git, $prep_repo);
		ct_start($self, $git, $prep_repo);
	} elsif ($TMPDIR) {
		return if delete($TODO{dump_roots_start});
		delete $TODO{dump_ibx_start}; # runs OnDestroy once
		return dump_ibx($self, shift @IBXQ) if @IBXQ;
		undef $DUMP_IBX_WPIPE; # done dumping inboxes
		undef $XHC;
		delete $TODO{associate};
	}
	# else: wait for shards_active (post_loop_do) callback
}

sub next_repos { # OnDestroy cb
	my ($repo_ctx) = @_;
	my ($self, $repo, $active) = @$repo_ctx{qw(self repo active)};
	progress($self, "$repo->{git_dir}: done");
	return if $DO_QUIT || !$REPO_CTX;
	my $n = grep { ! $repo_ctx->{shard_ok}->{$_} } keys %$active;
	die "E: $repo->{git_dir} $n shards failed" if $n;
	$REPO_CTX == $repo_ctx or die "BUG: $REPO_CTX != $repo_ctx";
	$REPO_CTX = undef;
	index_next($self);
}

sub index_done { # OnDestroy cb called when done indexing each code repo
	my ($repo_ctx) = @_;
	my ($self, $repo, $active) = @$repo_ctx{qw(self repo active)};

	return if $DO_QUIT;
	my $n = grep { ! $repo_ctx->{shard_ok}->{$_} } keys %$active;
	die "E: $repo->{git_dir} $n shards failed" if $n;
	$repo_ctx->{shard_ok} = {}; # reset for future shard_done
	$n = $repo->{shard_n};
	$active->{$n} = undef;
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{repo_stored} = [ $self, $repo_ctx ];
	$IDX_SHARDS[$n]->wq_io_do('store_repo', [ $p->{op_p} ], $repo);
	# repo_stored will fire once store_repo is done
}

sub index_repo { # run_git cb
	my (undef, $self, $git) = @_;
	return if $DO_QUIT;
	return index_next($self) if $git->{-cidx_err};
	return push(@$IDX_TODO, $git) if $REPO_CTX; # busy
	my $repo = delete $git->{-repo} or return index_next($self);
	my $roots_fh = delete $repo->{roots_fh} // die 'BUG: no {roots_fh}';
	seek($roots_fh, 0, SEEK_SET);
	chomp(my @roots = PublicInbox::IO::read_all $roots_fh);
	if (!@roots) {
		warn("E: $git->{git_dir} has no root commits\n");
		return index_next($self);
	}
	$repo->{roots} = \@roots;
	local $self->{current_info} = $git->{git_dir};
	my @shard_in = partition_refs($self, $git, delete($repo->{refs}));
	$repo->{git_dir} = $git->{git_dir};
	my $repo_ctx = $REPO_CTX = { self => $self, repo => $repo };
	delete $git->{-cidx_gits_fini}; # may fire gits_fini
	my $index_done = PublicInbox::OnDestroy->new($$, \&index_done,
							$repo_ctx);
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{shard_done} = [ $self, $repo_ctx, $index_done ];
	for my $n (0..$#shard_in) {
		$shard_in[$n]->flush or die "flush shard[$n]: $!";
		-s $shard_in[$n] or next;
		last if $DO_QUIT;
		$IDX_SHARDS[$n]->wq_io_do('shard_index',
					[ $shard_in[$n], $p->{op_p} ],
					$git, \@roots);
		$repo_ctx->{active}->{$n} = undef;
	}
	# shard_done fires when shard_index is done
}

sub get_roots ($$) {
	my ($self, $git) = @_;
	return if $DO_QUIT;
	my $refs = $git->{-repo}->{refs} // die 'BUG: no {-repo}->{refs}';
	sysseek($refs, 0, SEEK_SET);
	open my $roots_fh, '+>', undef;
	$git->{-repo}->{roots_fh} = $roots_fh;
	run_git([ qw(rev-list --stdin --max-parents=0) ],
		{ 0 => $refs, 1 => $roots_fh }, \&index_repo, $self, $git)
}

# for PublicInbox::SearchIdx::patch_id and with_umask
sub git { $_[0]->{git} }

sub load_existing ($) { # for -u/--update
	my ($self) = @_;
	my $dirs = $self->{git_dirs} //= [];
	if ($self->{-opt}->{update} || $self->{-opt}->{prune}) {
		local $self->{xdb};
		$self->xdb or
			die "E: $self->{cidx_dir} non-existent for --update\n";
		my @cur = grep {
			if (-e $_) {
				1;
			} else {
				push @GIT_DIR_GONE, $_;
				undef;
			}
		} $self->all_terms('P');
		if (@GIT_DIR_GONE && !$self->{-opt}->{prune}) {
			warn "W: the following repos no longer exist:\n",
				(map { "W:\t$_\n" } @GIT_DIR_GONE),
				"W: use --prune to remove them from ",
				$self->{cidx_dir}, "\n";
		}
		push @$dirs, @cur;
	}
	@$dirs = uniqstr @$dirs;
}

# SIG handlers:
sub shard_quit { $DO_QUIT = POSIX->can("SIG$_[0]")->() }
sub shard_usr1 { $TXN_BYTES = -1 }

sub cidx_init ($) {
	my ($self) = @_;
	my $dir = $self->{cidx_dir};
	unless (-d $dir) {
		warn "# creating $dir\n" if !$self->{-opt}->{quiet};
		File::Path::mkpath($dir);
	}
	$self->lock_acquire;
	my @shards;
	my $l = $self->{indexlevel} //= $self->{-opt}->{indexlevel};

	for my $n (0..($self->{nshard} - 1)) {
		my $shard = bless { %$self, shard => $n }, ref($self);
		delete @$shard{qw(lockfh lock_path)};
		my $xdb = $shard->idx_acquire;
		if (!$n) {
			if (($l // '') eq 'medium') {
				$xdb->set_metadata('indexlevel', $l);
			} elsif (($l // '') eq 'full') {
				$xdb->set_metadata('indexlevel', ''); # unset
			}
			$l ||= $xdb->get_metadata('indexlevel') || 'full';
		}
		$shard->{indexlevel} = $l;
		$shard->idx_release;
		$shard->wq_workers_start("cidx shard[$n]", 1, $SIGSET, {
			siblings => \@shards, # for ipc_atfork_child
		}, \&shard_done_wait, $self);
		push @shards, $shard;
	}
	$self->{indexlevel} //= $l;
	# this warning needs to happen after idx_acquire
	state $once;
	warn <<EOM if $PublicInbox::Search::X{CLOEXEC_UNSET} && !$once++;
W: Xapian v1.2.21..v1.2.24 were missing close-on-exec on OFD locks,
W: memory usage may be high for large indexing runs
EOM
	@shards;
}

sub gits_fini {
	undef $GITS_NR;
	PublicInbox::DS::enqueue_reap(); # kick @post_loop_do
}

sub scan_git_dirs ($) {
	my ($self) = @_;
	@$GIT_TODO = map { PublicInbox::Git->new($_) } @{$self->{git_dirs}};
	$GITS_NR = @$GIT_TODO;
	my $gits_fini = PublicInbox::OnDestroy->new($$, \&gits_fini);
	$_->{-cidx_gits_fini} = $gits_fini for @$GIT_TODO;
	progress($self, "scanning $GITS_NR code repositories...");
}

sub prune_init { # via wq_io_do in IDX_SHARDS
	my ($self) = @_;
	$self->{nr_prune} = 0;
	$TXN_BYTES = $BATCH_BYTES;
	$self->begin_txn_lazy;
}

sub prune_one { # via wq_io_do in IDX_SHARDS
	my ($self, $term) = @_;
	my @docids = docids_by_postlist($self, $term);
	for (@docids) {
		$TXN_BYTES -= $self->{xdb}->get_doclength($_) * 42;
		$self->{xdb}->delete_document($_);
	}
	++$self->{nr_prune};
	$TXN_BYTES < 0 and
		cidx_ckpoint($self, "prune [$self->{shard}] $self->{nr_prune}");
}

sub prune_commit { # via wq_io_do in IDX_SHARDS
	my ($self) = @_;
	my $prune_op_p = delete $self->{0} // die 'BUG: no {0} op_p';
	my $nr = delete $self->{nr_prune} // die 'BUG: nr_prune undef';
	cidx_ckpoint($self, "prune [$self->{shard}] $nr done") if $nr;
	send($prune_op_p, "prune_done $self->{shard}", 0);
}

sub shards_active { # post_loop_do
	return if $DO_QUIT;
	return if grep(defined, $PRUNE_DONE, $GIT_TODO, $IDX_TODO) != 3;
	return 1 if grep(defined, @$PRUNE_DONE) != @IDX_SHARDS;
	return 1 if $GITS_NR || scalar(@$IDX_TODO) || $REPO_CTX;
	return 1 if @IBXQ || keys(%TODO);
	for my $s (grep { $_->{-wq_s1} } @IDX_SHARDS) {
		$s->{-cidx_quit} = 1 if defined($s->{-wq_s1});
		$s->wq_close; # may recurse via awaitpid outside of event_loop
	}
	scalar(grep { $_->{-cidx_quit} } @IDX_SHARDS);
}

# signal handlers
sub kill_shards { $_->wq_kill(@_) for (@IDX_SHARDS) }

sub parent_quit {
	$DO_QUIT = POSIX->can("SIG$_[0]")->();
	$XHC = undef;
	kill_shards(@_);
	warn "# SIG$_[0] received, quitting...\n";
}

sub prep_umask ($) {
	my ($self) = @_;
	if ($self->{-cidx_internal}) { # respect core.sharedRepository
		@{$self->{git_dirs}} == 1 or die 'BUG: only for GIT_DIR';
		local $self->{git} =
			PublicInbox::Git->new($self->{git_dirs}->[0]);
		$self->with_umask;
	} elsif (-d $self->{cidx_dir}) { # respect existing perms
		my @st = stat(_);
		my $um = (~$st[2] & 0777);
		$self->{umask} = $um; # for SearchIdx->with_umask
		umask == $um or progress($self, 'using umask from ',
						$self->{cidx_dir}, ': ',
						sprintf('0%03o', $um));
		PublicInbox::OnDestroy->new($$, \&CORE::umask, umask($um));
	} else {
		$self->{umask} = umask; # for SearchIdx->with_umask
		undef;
	}
}

sub prep_alternate_end { # run_await cb for config extensions.objectFormat
	my ($pid, $cmd, undef, $opt, $objdir, $run_prune) = @_;
	my ($status, $sig) = ($? >> 8, $? & 127);
	my $next_dir = shift(@PRUNE_QUEUE);
	prep_alternate_start($next_dir, $run_prune) if defined($next_dir);
	my $fmt;
	if (!$sig && $status == 1) { # unset, default is '' (SHA-1)
		$fmt = 'sha1';
	} elsif (!$sig && $status == 0) {
		chomp($fmt = ${$opt->{1}} || 'sha1');
	}
	$fmt // return warn("git config \$?=$? for objdir=$objdir");
	my $hexlen = $OFMT2HEXLEN{$fmt} // return warn <<EOM;
E: ignoring objdir=$objdir, unknown extensions.objectFormat=$fmt
EOM
	unless ($ALT_FH{$hexlen}) {
		require PublicInbox::Import;
		my $git_dir = "$TMPDIR/hexlen$hexlen.git";
		PublicInbox::Import::init_bare($git_dir, 'cidx-all', $fmt);
		open $ALT_FH{$hexlen}, '>', "$git_dir/objects/info/alternates";
	}
	say { $ALT_FH{$hexlen} } $objdir;
}

sub prep_alternate_start {
	my ($git_dir, $run_prune) = @_;
	my $o = $git_dir.'/objects';
	while (!-d $o) {
		$git_dir = shift(@PRUNE_QUEUE) // return
		$o = $git_dir.'/objects';
	}
	my $cmd = [ 'git', "--git-dir=$git_dir",
			qw(config extensions.objectFormat) ];
	my $opt = { quiet => 1 };
	run_await($cmd, undef, $opt, \&prep_alternate_end, $o, $run_prune);
}

sub cmd_done { # run_await cb for sort, xapian-delve, sed failures
	my ($pid, $cmd, undef, undef, $run_on_destroy) = @_;
	$? and die "fatal: @$cmd (\$?=$?)\n";
	# $run_on_destroy calls associate() or run_prune()
}

# runs once all inboxes and shards are dumped via OnDestroy
sub associate {
	my ($self) = @_;
	return if $DO_QUIT;
	@IDX_SHARDS or return warn("# aborting on no shards\n");
	unlink("$TMPDIR/root2id");
	my @pending = keys %{$self->{PENDING}};
	die "E: pending=@pending jobs not done\n" if @pending;
	progress($self, 'associating...');
	my @join = ('time', @JOIN, 'to_ibx_id', 'to_root_id');
	my $rd = popen_rd(\@join, $CMD_ENV, { -C => "$TMPDIR" });
	my %score;
	while (<$rd>) { # PFX ibx_ids root_id
		my (undef, $ibx_ids, @root_ids) = split(/ /, $_);
		for my $ibx_id (split(/,/, $ibx_ids)) {
			++$score{"$ibx_id $_"} for @root_ids;
		}
	}
	$rd->close or die "fatal: @join failed: \$?=$?";
	my $min = $self->{-opt}->{'assoc-min'} // 10;
	progress($self, scalar(keys %score).' potential pairings...');
	for my $k (keys %score) {
		my $nr = $score{$k};
		my ($ibx_id, $root) = split(/ /, $k);
		my $ekey = $IBX[$ibx_id]->eidx_key;
		$root = $ID2ROOT[$root];
		progress($self, "$ekey => $root has $nr matches");
	}
	delete $TODO{associating}; # break out of shards_active()
	# TODO
	warn "# Waiting for $TMPDIR/cont @JOIN";
	system "ls -Rl $TMPDIR >&2";
	system "wc -l $TMPDIR/to_*_id >&2";
	#sleep(1) until -f "$TMPDIR/cont";
	# warn "# Waiting for $TMPDIR/cont";
	# sleep(1) until -f "$TMPDIR/cont";
}

sub require_progs {
	my $op = shift;
	while (my ($x, $argv) = splice(@_, 0, 2)) {
		my $e = $x;
		$e =~ tr/a-z-/A-Z_/;
		my $c = $ENV{$e} // $x;
		$argv->[0] //= which($c) // die "E: `$x' required for --$op\n";
	}
}

sub init_associate_postfork ($) {
	my ($self) = @_;
	return unless $self->{-opt}->{associate};
	require_progs('associate', join => \@JOIN);
	$QRY_STR = $self->{-opt}->{'associate-date-range'} // '1.year.ago..';
	substr($QRY_STR, 0, 0) = 'dt:';
	@{$self->{git_dirs} // []} or die "E: no coderepos to associate\n";
	@IBX or die "E: no inboxes to associate\n";
	my $approx_git = PublicInbox::Git->new($self->{git_dirs}->[0]); # ugh
	$self->query_approxidate($approx_git, $QRY_STR); # in-place
	$TODO{associate} = PublicInbox::OnDestroy->new($$, \&associate, $self);
	$TODO{dump_ibx_start} = PublicInbox::OnDestroy->new($$,
				\&dump_ibx_start, $self, $TODO{associate});
	$TODO{dump_roots_start} = PublicInbox::OnDestroy->new($$,
				\&dump_roots_start, $self, $TODO{associate});
	my $id = -1;
	@IBXQ = map { ++$id } @IBX;
}

sub init_prune ($) {
	my ($self) = @_;
	return (@$PRUNE_DONE = map { 1 } @IDX_SHARDS) if !$self->{-opt}->{prune};

	# Dealing with millions of commits here at once, so use faster tools.
	# xapian-delve is nearly an order-of-magnitude faster than Xapian Perl
	# bindings.  sed/awk are faster than Perl for simple stream ops, and
	# sort+comm are more memory-efficient with gigantic lists.
	# pipeline: delve | sed | sort >indexed_commits
	my @delve = (undef, qw(-A Q -1));
	my @sed = (undef, '-ne', 's/^Q//p');
	@COMM = (undef, qw(-2 -3 indexed_commits -));
	@AWK = (undef, '$2 == "commit" { print $1 }'); # --batch-check output
	require_progs('prune', 'xapian-delve' => \@delve, sed => \@sed,
			comm => \@COMM, awk => \@AWK);
	for (0..$#IDX_SHARDS) { push @delve, "$self->{xpfx}/$_" }
	my $run_prune = PublicInbox::OnDestroy->new($$, \&run_prune, $self);
	my ($sort_opt, $sed_opt, $delve_opt);
	pipe(local $sed_opt->{0}, local $delve_opt->{1});
	pipe(local $sort_opt->{0}, local $sed_opt->{1});
	open($sort_opt->{1}, '+>', "$TMPDIR/indexed_commits");
	run_await([@SORT, '-u'], $CMD_ENV, $sort_opt, \&cmd_done, $run_prune);
	run_await(\@sed, $CMD_ENV, $sed_opt, \&cmd_done, $run_prune);
	run_await(\@delve, undef, $delve_opt, \&cmd_done, $run_prune);
	@PRUNE_QUEUE = @{$self->{git_dirs}};
	for (1..$LIVE_JOBS) {
		prep_alternate_start(shift(@PRUNE_QUEUE) // last, $run_prune);
	}
}

sub dump_git_commits { # run_await cb
	my ($pid, undef, undef, $batch_opt) = @_;
	(defined($pid) && $?) and die "E: @PRUNE_BATCH: \$?=$?";
	return if $DO_QUIT;
	my ($hexlen) = keys(%ALT_FH) or return; # done
	close(delete $ALT_FH{$hexlen}); # flushes `say' buffer

	$PRUNE_BATCH[1] = "--git-dir=$TMPDIR/hexlen$hexlen.git";
	run_await(\@PRUNE_BATCH, undef, $batch_opt, \&dump_git_commits);
}

sub run_prune { # OnDestroy when `git config extensions.objectFormat' are done
	my ($self) = @_;
	return if $DO_QUIT;
	# setup the following pipeline: (
	#	git --git-dir=hexlen40.git cat-file \
	#		--batch-all-objects --batch-check &&
	#	git --git-dir=hexlen64.git cat-file \
	#		--batch-all-objects --batch-check
	# ) | awk | sort | comm | cidx_read_comm()
	my ($awk_opt, $sort_opt, $batch_opt);
	my $comm_opt = { -C => "$TMPDIR" };
	pipe(local $awk_opt->{0}, local $batch_opt->{1});
	pipe(local $sort_opt->{0}, local $awk_opt->{1});
	pipe(local $comm_opt->{0}, local $sort_opt->{1});
	run_await(\@AWK, $CMD_ENV, $awk_opt, \&cmd_done);
	run_await([@SORT, '-u'], $CMD_ENV, $sort_opt, \&cmd_done);
	my $comm_rd = popen_rd(\@COMM, $CMD_ENV, $comm_opt, \&cmd_done, \@COMM);
	PublicInbox::CidxComm->new($comm_rd, $self); # calls cidx_read_comm
	my $git_ver = PublicInbox::Git::git_version();
	push @PRUNE_BATCH, '--buffer' if $git_ver ge v2.6;

	# Yes, we pipe --unordered git output to sort(1) because sorting
	# inside git leads to orders-of-magnitude slowdowns on rotational
	# storage.  GNU sort(1) also works well on larger-than-memory
	# datasets, and it's not worth eliding sort(1) for old git.
	push @PRUNE_BATCH, '--unordered' if $git_ver ge v2.19;
	warn(sprintf(<<EOM, $git_ver)) if $git_ver lt v2.19;
W: git v2.19+ recommended for high-latency storage (have git v%vd)
EOM
	dump_git_commits(undef, undef, undef, $batch_opt);
}

sub cidx_read_comm { # via PublicInbox::CidxComm::event_step
	my ($self, $comm_rd) = @_;
	return if $DO_QUIT;
	$_->wq_do('prune_init') for @IDX_SHARDS;
	while (defined(my $cmt = <$comm_rd>)) {
		chop($cmt) eq "\n" or die "BUG: no LF in comm output ($cmt)";
		my $n = hex(substr($cmt, 0, 8)) % scalar(@IDX_SHARDS);
		$IDX_SHARDS[$n]->wq_do('prune_one', 'Q'.$cmt);
		last if $DO_QUIT;
	}
	for my $git_dir (@GIT_DIR_GONE) {
		my $n = git_dir_hash($git_dir) % scalar(@IDX_SHARDS);
		$IDX_SHARDS[$n]->wq_do('prune_one', 'P'.$git_dir);
	}
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{prune_done} = [ $self ];
	$_->wq_io_do('prune_commit', [ $p->{op_p} ]) for @IDX_SHARDS;
}

sub init_associate_prefork ($) {
	my ($self) = @_;
	return unless $self->{-opt}->{associate};
	require PublicInbox::CidxXapHelperAux;
	require PublicInbox::XapClient;
	$self->{-pi_cfg} = PublicInbox::Config->new;
	my @unknown;
	my @pfx = @{$self->{-opt}->{'associate-prefixes'} // [ 'patchid' ]};
	@pfx = map { split(/\s*,\s*/) } @pfx;
	for (@pfx) {
		my $v = $PublicInbox::Search::PATCH_BOOL_COMMON{$_} //
			push(@unknown, $_);
		push(@ASSOC_PFX, split(/ /, $v));
	}
	die <<EOM if @unknown;
--associate-prefixes contains unsupported prefixes: @unknown
EOM
	@ASSOC_PFX = uniqstr @ASSOC_PFX;
	my %incl = map {
		rel2abs_collapsed($_) => undef;
	} (@{$self->{-opt}->{include} // []});
	my $all = $self->{-opt}->{all};
	if (my $only = $self->{-opt}->{only}) {
		die <<'' if $all;
E: --all is incompatible with --only

		$incl{rel2abs_collapsed($_)} = undef for @$only;
	}
	unless (keys(%incl)) {
		$all = 1;
		warn <<EOM unless $self->{opt}->{quiet};
# --all implied since no inboxes were specified with --only or --include
EOM
	}
	$self->{-pi_cfg}->each_inbox(\&_prep_ibx, $self, \%incl, $all);
	my $nr = scalar(@IBX) or die "E: no inboxes to associate\n";
	progress($self, "will associate $nr inboxes in ",
			$self->{-pi_cfg}->{-f}, " using: @pfx");
}

sub _prep_ibx { # each_inbox callback
	my ($ibx, $self, $incl, $all) = @_;
	($all || exists($incl->{$ibx->{inboxdir}})) and push @IBX, $ibx;
}

sub show_roots { # for diagnostics
	my ($self) = @_;
	local $self->{xdb};
	my $cur = $self->xdb->allterms_begin('G');
	my $end = $self->{xdb}->allterms_end('G');
	my $qrepo = $PublicInbox::Search::X{Query}->new('T'.'r');
	my $enq = $PublicInbox::Search::X{Enquire}->new($self->{xdb});
	$enq->set_weighting_scheme($PublicInbox::Search::X{BoolWeight}->new);
	$enq->set_docid_order($PublicInbox::Search::ENQ_ASCENDING);
	for (; $cur != $end; $cur++) {
		my $G_oidhex = $cur->get_termname;
		my $qry = $PublicInbox::Search::X{Query}->new(
				PublicInbox::Search::OP_FILTER(),
				$qrepo, $G_oidhex);
		$enq->set_query($qry);
		my ($off, $lim) = (0, 10000);
		say 'commit ',substr($G_oidhex, 1), ' appears in:';
		while (1) {
			my $mset = $enq->get_mset($off, $lim);
			my $size = $mset->size or last;
			for my $x ($mset->items) {
				my $doc = $x->get_document;
				for (xap_terms('P', $x->get_document)) {
					say '- /', substr($_, 1);
				}
			}
			$off += $size;
		}
	}
}

sub do_inits { # called via PublicInbox::DS::add_timer
	my ($self) = @_;
	init_prune($self);
	init_associate_postfork($self);
	scan_git_dirs($self) if $self->{-opt}->{scan} // 1;
	my $max = $TODO{associate} ? max($LIVE_JOBS, $NPROC) : $LIVE_JOBS;
	index_next($self) for (1..$max);
}

sub cidx_run { # main entry point
	my ($self) = @_;
	my $restore_umask = prep_umask($self);
	local $SIGSET = PublicInbox::DS::block_signals(
					POSIX::SIGTSTP, POSIX::SIGCONT);
	my $restore = PublicInbox::OnDestroy->new($$,
		\&PublicInbox::DS::sig_setmask, $SIGSET);
	local $PRUNE_DONE = [];
	local $IDX_TODO = [];
	local $GIT_TODO = [];
	local ($DO_QUIT, $REINDEX, $TXN_BYTES, @GIT_DIR_GONE, @PRUNE_QUEUE,
		$REPO_CTX, %ALT_FH, $TMPDIR, @AWK, @COMM, $CMD_ENV,
		%TODO, @IBXQ, @IBX, @JOIN, @ASSOC_PFX, $DUMP_IBX_WPIPE,
		@ID2ROOT, $XHC, @SORT, $GITS_NR);
	local $BATCH_BYTES = $self->{-opt}->{batch_size} //
				$PublicInbox::SearchIdx::BATCH_BYTES;
	local $self->{ASSOC_PFX} = \@ASSOC_PFX;
	local $self->{PENDING} = {};
	local $self->{-pi_cfg};
	if (grep { $_ } @{$self->{-opt}}{qw(prune associate)}) {
		require File::Temp;
		$TMPDIR = File::Temp->newdir('cidx-all-git-XXXX', TMPDIR => 1);
		$CMD_ENV = { TMPDIR => "$TMPDIR", LC_ALL => 'C', LANG => 'C' };
		require_progs('(prune|associate)', sort => \@SORT);
		for (qw(parallel compress-program buffer-size)) { # GNU sort
			my $v = $self->{-opt}->{"sort-$_"};
			push @SORT, "--$_=$v" if defined $v;
		}
		init_associate_prefork($self)
	}
	local @IDX_SHARDS = cidx_init($self); # forks workers
	local $self->{current_info} = '';
	local $MY_SIG = {
		CHLD => \&PublicInbox::DS::enqueue_reap,
		USR1 => \&kill_shards,
	};
	local @PRUNE_BATCH = @PRUNE_BATCH;
	$MY_SIG->{$_} = \&parent_quit for qw(TERM QUIT INT);
	my $cb = $SIG{__WARN__} || \&CORE::warn;
	local $SIG{__WARN__} = sub {
		my $m = shift @_;
		$self->{current_info} eq '' or
			$m =~ s/\A(#?\s*)/$1$self->{current_info}: /;
		$cb->($m, @_);
	};
	load_existing($self) unless $self->{-cidx_internal};
	if ($self->{-opt}->{reindex}) {
		require PublicInbox::SharedKV;
		$REINDEX = PublicInbox::SharedKV->new;
		delete $REINDEX->{lock_path};
		$REINDEX->dbh;
	}
	my @nc = grep { File::Spec->canonpath($_) ne $_ } @{$self->{git_dirs}};
	if (@nc) {
		warn "E: BUG? paths in $self->{cidx_dir} not canonicalized:\n";
		for my $d (@{$self->{git_dirs}}) {
			my $c = File::Spec->canonpath($_);
			warn "E: $d => $c\n";
			$d = $c;
		}
		warn "E: canonicalized and attempting to continue\n";
	}
	if (defined(my $excl = $self->{-opt}->{exclude})) {
		my $re = '(?:'.join('\\z|', map {
				glob2re($_) // qr/\A\Q$_\E/
			} @$excl).'\\z)';
		my @excl;
		@{$self->{git_dirs}} = grep {
			$_ =~ /$re/ ? (push(@excl, $_), 0) : 1;
		} @{$self->{git_dirs}};
		warn("# excluding $_\n") for @excl;
		@GIT_DIR_GONE = uniqstr @GIT_DIR_GONE, @excl;
	}
	local $NCHANGE = 0;
	local $NPROC = PublicInbox::IPC::detect_nproc();
	local $LIVE_JOBS = $self->{-opt}->{jobs} || $NPROC || 2;
	local @RDONLY_XDB = $self->xdb_shards_flat;
	PublicInbox::DS::add_timer(0, \&do_inits, $self);

	# FreeBSD ignores/discards SIGCHLD while signals are blocked and
	# EVFILT_SIGNAL is inactive, so we pretend we have a SIGCHLD pending
	PublicInbox::DS::enqueue_reap();

	local @PublicInbox::DS::post_loop_do = (\&shards_active);
	PublicInbox::DS::event_loop($MY_SIG, $SIGSET);
	PublicInbox::DS->Reset;
	$self->lock_release(!!$NCHANGE);
	show_roots($self) if $self->{-opt}->{'show-roots'} # for diagnostics
}

sub ipc_atfork_child { # @IDX_SHARDS
	my ($self) = @_;
	$self->SUPER::ipc_atfork_child;
	$SIG{USR1} = \&shard_usr1;
	$SIG{$_} = \&shard_quit for qw(INT TERM QUIT);
	my $x = delete $self->{siblings} // die 'BUG: no {siblings}';
	$_->wq_close for @$x;
	undef;
}

sub shard_done_wait { # awaitpid cb via ipc_worker_reap
	my ($pid, $shard, $self) = @_;
	my $quit_req = delete($shard->{-cidx_quit});
	return if $DO_QUIT;
	if ($? == 0) { # success
		$quit_req // warn 'BUG: {-cidx_quit} unset';
	} else {
		warn "PID:$pid $shard->{shard} exited with \$?=$?\n";
		++$self->{shard_err} if defined($self->{shard_err});
	}
}

1;
