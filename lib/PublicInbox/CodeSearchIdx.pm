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
use PublicInbox::SHA qw(sha256_hex);
use PublicInbox::Search qw(xap_terms);
use PublicInbox::SearchIdx qw(add_val);
use PublicInbox::Config qw(glob2re);
use PublicInbox::Spawn qw(which spawn popen_rd);
use PublicInbox::OnDestroy;
use PublicInbox::CidxLogP;
use PublicInbox::CidxComm;
use PublicInbox::Git qw(%OFMT2HEXLEN);
use Socket qw(MSG_EOR);
use Carp ();
our (
	$LIVE, # pid => cmd
	$LIVE_JOBS, # integer
	$MY_SIG, # like %SIG
	$SIGSET,
	$TXN_BYTES, # number of bytes in current shard transaction
	$BATCH_BYTES,
	$DO_QUIT, # signal number
	@RDONLY_XDB, # Xapian::Database
	@IDX_SHARDS, # clones of self
	$MAX_SIZE,
	$REINDEX, # PublicInbox::SharedKV
	@GIT_DIR_GONE, # [ git_dir1, git_dir2 ]
	$PRUNE_DONE, # marks off prune completions
	$NCHANGE, # current number of changes
	$REPO_CTX, # current repo being indexed in shards
	$IDX_TODO, # [ $git0, $root0, $git1, $root1, ...]
	$GIT_TODO, # [ GIT_DIR0, GIT_DIR1, ...]
	%ALT_FH, # hexlen => tmp IO for TMPDIR git alternates
	$TMPDIR, # File::Temp->newdir object for prune
	@PRUNE_QUEUE, # GIT_DIRs to prepare for pruning
	$PRUNE_ENV, # env for awk(1), comm(1), sort(1) commands during prune
	@AWK, @COMM, @SORT, # awk(1), comm(1), sort(1) commands
);

# stop walking history if we see >$SEEN_MAX existing commits, this assumes
# branches don't diverge by more than this number of commits...
# git walks commits quickly if it doesn't have to read trees
our $SEEN_MAX = 100000;

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

sub store_repo { # wq_do - returns docid
	my ($self, $repo) = @_;
	$self->begin_txn_lazy;
	$self->{xdb}->delete_document($_) for @{$repo->{to_delete}};
	my $doc = $PublicInbox::Search::X{Document}->new;
	add_val($doc, PublicInbox::CodeSearch::CT, $repo->{ct});
	$doc->add_boolean_term("P$repo->{git_dir}");
	$doc->add_boolean_term('T'.'r');
	$doc->add_boolean_term('G'.$_) for @{$repo->{roots}};
	$doc->set_data($repo->{fp}); # \n delimited
	if ($repo->{docid}) {
		$self->{xdb}->replace_document($repo->{docid}, $doc);
		$repo->{docid};
	} else {
		$self->{xdb}->add_document($doc);
	}
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
	my ($pid, $self, $op_p) = @_;
	if (!$? || ($DO_QUIT && (($? & 127) == $DO_QUIT ||
				($? & 127) == POSIX::SIGPIPE))) {
		send($op_p, "shard_done $self->{shard}", MSG_EOR);
	} else {
		warn "E: git @LOG_STDIN: \$?=$?\n";
		$self->{xdb}->cancel_transaction;
	}
}

sub shard_index { # via wq_io_do in IDX_SHARDS
	my ($self, $git, $roots) = @_;

	my $in = delete($self->{0}) // die 'BUG: no {0} input';
	my $op_p = delete($self->{1}) // die 'BUG: no {1} op_p';
	sysseek($in, 0, SEEK_SET) or die "seek: $!";
	my ($rd, $pid) = $git->popen(@LOG_STDIN, undef, { 0 => $in });
	close $in or die "close: $!";
	awaitpid($pid, \&cidx_reap_log, $self, $op_p);
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

sub cidx_await_cb { # awaitpid cb
	my ($pid, $cb, $self, $git, @args) = @_;
	return if !$LIVE || $DO_QUIT;
	my $cmd = delete $LIVE->{$pid} // die 'BUG: no $cmd';
	PublicInbox::DS::enqueue_reap() if !keys(%$LIVE); # once more for PLC
	if ($?) {
		$git->{-cidx_err} = 1;
		return warn("@$cmd error: \$?=$?\n");
	}
	$cb->($self, $git, @args);
}

sub cidx_await ($$$$$@) {
	my ($pid, $cmd, $cb, $self, $git, @args) = @_;
	$LIVE->{$pid} = $cmd;
	awaitpid($pid, \&cidx_await_cb, $cb, $self, $git, @args);
}

# this is different from the grokmirror-compatible fingerprint since we
# only care about --heads (branches) and --tags, and not even their names
sub fp_start ($$$) {
	my ($self, $git, $prep_repo) = @_;
	return if !$LIVE || $DO_QUIT;
	open my $refs, '+>', undef or die "open: $!";
	my $cmd = ['git', "--git-dir=$git->{git_dir}",
		qw(show-ref --heads --tags --hash)];
	my $pid = spawn($cmd, undef, { 1 => $refs });
	$git->{-repo}->{refs} = $refs;
	cidx_await($pid, $cmd, \&fp_fini, $self, $git, $prep_repo);
}

sub fp_fini { # cidx_await cb
	my ($self, $git, $prep_repo) = @_;
	my $refs = $git->{-repo}->{refs} // die 'BUG: no {-repo}->{refs}';
	seek($refs, 0, SEEK_SET) or die "seek: $!";
	my $buf;
	my $dig = PublicInbox::SHA->new(256);
	while (read($refs, $buf, 65536)) { $dig->add($buf) }
	$git->{-repo}->{fp} = $dig->hexdigest;
}

sub ct_start ($$$) {
	my ($self, $git, $prep_repo) = @_;
	return if !$LIVE || $DO_QUIT;
	my $cmd = [ 'git', "--git-dir=$git->{git_dir}",
		qw[for-each-ref --sort=-committerdate
		--format=%(committerdate:raw) --count=1
		refs/heads/ refs/tags/] ];
	my ($rd, $pid) = popen_rd($cmd);
	cidx_await($pid, $cmd, \&ct_fini, $self, $git, $rd, $prep_repo);
}

sub ct_fini { # cidx_await cb
	my ($self, $git, $rd, $prep_repo) = @_;
	defined(my $ct = <$rd>) or return;
	$ct =~ s/\s+.*\z//s; # drop TZ + LF
	$git->{-repo}->{ct} = $ct + 0;
}

# TODO: also index gitweb.owner and the full fingerprint for grokmirror?
sub prep_repo ($$) {
	my ($self, $git) = @_;
	return if !$LIVE || $DO_QUIT;
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
	sysseek($refs, 0, SEEK_SET) or die "seek: $!"; # for rev-list --stdin
	my $rfh = $git->popen(qw(rev-list --stdin), undef, { 0 => $refs });
	close $refs or die "close: $!";
	my $seen = 0;
	my @shard_in = map {
		$_->reopen;
		open my $fh, '+>', undef or die "open: $!";
		$fh;
	} @RDONLY_XDB;

	my $n0 = $NCHANGE;
	while (defined(my $cmt = <$rfh>)) {
		chomp $cmt;
		my $n = hex(substr($cmt, 0, 8)) % scalar(@RDONLY_XDB);
		if ($REINDEX && $REINDEX->set_maybe(pack('H*', $cmt), '')) {
			say { $shard_in[$n] } $cmt or die "say: $!";
			++$NCHANGE;
		} elsif (seen($RDONLY_XDB[$n], 'Q'.$cmt)) {
			last if ++$seen > $SEEN_MAX;
		} else {
			say { $shard_in[$n] } $cmt or die "say: $!";
			++$NCHANGE;
			$seen = 0;
		}
		if ($DO_QUIT) {
			close($rfh);
			return ();
		}
	}
	close($rfh);
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
	send($op_p, "shard_done $self->{shard}", MSG_EOR);
}

sub index_next ($) {
	my ($self) = @_;
	return if $DO_QUIT;
	if ($IDX_TODO && @$IDX_TODO) {
		index_repo($self, shift @$IDX_TODO);
	} elsif ($GIT_TODO && @$GIT_TODO) {
		my $git = PublicInbox::Git->new(shift @$GIT_TODO);
		my $prep_repo = PublicInbox::OnDestroy->new($$, \&prep_repo,
							$self, $git);
		fp_start($self, $git, $prep_repo);
		ct_start($self, $git, $prep_repo);
	}
	# else: wait for shards_active (post_loop_do) callback
}

sub next_repos { # OnDestroy cb
	my ($repo_ctx) = @_;
	progress($repo_ctx->{self}, "$repo_ctx->{repo}->{git_dir}: done");
	return if $DO_QUIT;
	if ($REPO_CTX) {
		$REPO_CTX == $repo_ctx or die "BUG: $REPO_CTX != $repo_ctx";
		$REPO_CTX = undef;
		index_next($repo_ctx->{self});
	}
}

sub commit_shard { # OnDestroy cb
	my ($repo_ctx) = @_;
	my ($self, $repo, $active) = @$repo_ctx{qw(self repo active)};

	my $n = grep { ! $repo_ctx->{shard_ok}->{$_} } keys %$active;
	die "E: $repo->{git_dir} $n shards failed" if $n && !$DO_QUIT;

	$repo_ctx->{shard_ok} = {};
	if (!$DO_QUIT) {
		my $id = $IDX_SHARDS[$repo->{shard_n}]->wq_do('store_repo',
								$repo);
		(!defined($id) || $id <= 0) and
			die "E: store_repo $repo->{git_dir}: id=$id";
		$active->{$repo->{shard_n}} = undef;
	}
	my $next = PublicInbox::OnDestroy->new($$, \&next_repos, $repo_ctx);
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{shard_done} = [ $repo_ctx->{self}, $repo_ctx, $next ];
	for my $n (keys %$active) {
		$IDX_SHARDS[$n]->wq_io_do('shard_commit', [ $p->{op_p} ]);
	}
	undef $p; # shard_done fires when all shards are committed
}

sub index_repo { # cidx_await cb
	my ($self, $git) = @_;
	return if $DO_QUIT;
	return index_next($self) if $git->{-cidx_err};
	return push(@$IDX_TODO, $git) if $REPO_CTX; # busy
	my $repo = delete $git->{-repo} or return index_next($self);
	my $roots_fh = delete $repo->{roots_fh} // die 'BUG: no {roots_fh}';
	seek($roots_fh, 0, SEEK_SET) or die "seek: $!";
	chomp(my @roots = <$roots_fh>);
	close($roots_fh) or die "close: $!";
	if (!@roots) {
		warn("E: $git->{git_dir} has no root commits\n");
		return index_next($self);
	}
	$repo->{roots} = \@roots;
	local $self->{current_info} = $git->{git_dir};
	my @shard_in = partition_refs($self, $git, delete($repo->{refs}));
	$repo->{git_dir} = $git->{git_dir};
	my $repo_ctx = $REPO_CTX = { self => $self, repo => $repo };
	my $commit_shard = PublicInbox::OnDestroy->new($$, \&commit_shard,
							$repo_ctx);
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{shard_done} = [ $self, $repo_ctx, $commit_shard ];
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
	return if !$LIVE || $DO_QUIT;
	my $refs = $git->{-repo}->{refs} // die 'BUG: no {-repo}->{refs}';
	sysseek($refs, 0, SEEK_SET) or die "seek: $!";
	open my $roots_fh, '+>', undef or die "open: $!";
	my $cmd = [ 'git', "--git-dir=$git->{git_dir}",
			qw(rev-list --stdin --max-parents=0) ];
	my $pid = spawn($cmd, undef, { 0 => $refs, 1 => $roots_fh });
	$git->{-repo}->{roots_fh} = $roots_fh;
	cidx_await($pid, $cmd, \&index_repo, $self, $git);
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
	my %uniq; # List::Util::uniq requires Perl 5.26+
	@$dirs = grep { !$uniq{$_}++ } @$dirs;
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

sub scan_git_dirs ($) {
	my ($self) = @_;
	@$GIT_TODO = @{$self->{git_dirs}};
	index_next($self) for (1..$LIVE_JOBS);
}

sub prune_do { # via wq_io_do in IDX_SHARDS
	my ($self) = @_;
	my $gone = delete $self->{0} // die 'BUG: no {0} gone input';
	my $prune_op_p = delete $self->{1} // die 'BUG: no {1} op_p';
	$TXN_BYTES = $BATCH_BYTES;
	$self->begin_txn_lazy;
	my $xdb = $self->{xdb};
	my $nr = 0;
	local $/ = "\0";
	while (my $p = <$gone>) { # Q$cmt or P$git_dir
		chomp $p;
		my @docids = docids_by_postlist($self, $p);
		for (@docids) {
			$TXN_BYTES -= $xdb->get_doclength($_) * 42;
			$xdb->delete_document($_);
		}
		++$nr;
		$TXN_BYTES < 0 and
			cidx_ckpoint($self, "prune [$self->{shard}] $nr");
	}
	send($prune_op_p, "prune_done $self->{shard}", MSG_EOR);
	cidx_ckpoint($self, "prune [$self->{shard}] $nr done") if $nr;
}

sub shards_active { # post_loop_do
	return if $DO_QUIT;
	return if grep(defined, $PRUNE_DONE, $GIT_TODO, $IDX_TODO, $LIVE) != 4;
	return 1 if grep(defined, @$PRUNE_DONE) != @IDX_SHARDS;
	return 1 if scalar(@$GIT_TODO) || scalar(@$IDX_TODO) || $REPO_CTX;
	return 1 if keys(%$LIVE);
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

sub prep_alternate_end { # awaitpid callback for config extensions.objectFormat
	my ($pid, $objdir, $out, $run_prune) = @_;
	my $status = $? >> 8;
	my $next_dir = shift(@PRUNE_QUEUE);
	prep_alternate_start($next_dir, $run_prune) if defined($next_dir);
	my $fmt;
	if ($status == 1) { # unset, default is '' (SHA-1)
		$fmt = 'sha1';
	} elsif ($status == 0) {
		seek($out, 0, SEEK_SET) or die "seek: $!";
		chomp($fmt = <$out> // 'sha1');
	} else {
		return warn("git config \$?=$? for objdir=$objdir");
	}
	my $hexlen = $OFMT2HEXLEN{$fmt} // return warn <<EOM;
E: ignoring objdir=$objdir, unknown extensions.objectFormat=$fmt
EOM
	unless ($ALT_FH{$hexlen}) {
		my $git_dir = "$TMPDIR/hexlen$hexlen.git";
		PublicInbox::Import::init_bare($git_dir, 'cidx-all', $fmt);
		my $f = "$git_dir/objects/info/alternates";
		open $ALT_FH{$hexlen}, '>', $f or die "open($f): $!";
	}
	say { $ALT_FH{$hexlen} } $objdir or die "say: $!";
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
	open my $out, '+>', undef or die "open(tmp): $!";
	my $pid = spawn($cmd, undef, { 1 => $out });
	awaitpid($pid, \&prep_alternate_end, $o, $out, $run_prune);
}

sub prune_cmd_done { # awaitpid cb for sort, xapian-delve, sed failures
	my ($pid, $cmd, $run_prune) = @_;
	$? and die "@$cmd failed: \$?=$?";
}

sub init_prune ($) {
	my ($self) = @_;
	return (@$PRUNE_DONE = map { 1 } @IDX_SHARDS) if !$self->{-opt}->{prune};

	require File::Temp;
	require PublicInbox::Import;
	$TMPDIR = File::Temp->newdir('cidx-all-git-XXXX', TMPDIR => 1);

	# Dealing with millions of commits here at once, so use faster tools.
	# xapian-delve is nearly an order-of-magnitude faster than Xapian Perl
	# bindings.  sed/awk are faster than Perl for simple stream ops, and
	# sort+comm are more memory-efficient with gigantic lists
	my @delve = (undef, qw(-A Q -1));
	my @sed = (undef, '-ne', 's/^Q//p');
	@SORT = (undef, '-u');
	@COMM = (undef, qw(-2 -3 indexed_commits -));
	@AWK = (undef, '$2 == "commit" { print $1 }'); # --batch-check output
	my @x = ('xapian-delve' => \@delve, sed => \@sed,
		sort => \@SORT, comm => \@COMM, awk => \@AWK);
	while (my ($x, $argv) = splice(@x, 0, 2)) {
		my $e = $x;
		$e =~ tr/a-z-/A-Z_/;
		my $c = $ENV{$e} // $x;
		$argv->[0] = which($c) // die "E: `$x' required for --prune\n";
	}
	for (0..$#IDX_SHARDS) { push @delve, "$self->{xpfx}/$_" }
	for (qw(parallel compress-program buffer-size)) { # GNU sort options
		my $v = $self->{-opt}->{"sort-$_"};
		push @SORT, "--$_=$v" if defined $v;
	}
	my $run_prune = PublicInbox::OnDestroy->new($$, \&run_prune, $self);
	pipe(my ($sed_in, $delve_out)) or die "pipe: $!";
	pipe(my ($sort_in, $sed_out)) or die "pipe: $!";
	open(my $sort_out, '+>', "$TMPDIR/indexed_commits") or die "open: $!";
	$PRUNE_ENV = { TMPDIR => "$TMPDIR", LC_ALL => 'C', LANG => 'C' };
	my $pid = spawn(\@SORT, $PRUNE_ENV, { 0 => $sort_in, 1 => $sort_out });
	awaitpid($pid, \&prune_cmd_done, \@SORT, $run_prune);
	$pid = spawn(\@sed, $PRUNE_ENV, { 0 => $sed_in, 1 => $sed_out });
	awaitpid($pid, \&prune_cmd_done, \@sed, $run_prune);
	$pid = spawn(\@delve, undef, { 1 => $delve_out });
	awaitpid($pid, \&prune_cmd_done, \@delve, $run_prune);
	@PRUNE_QUEUE = @{$self->{git_dirs}};
	for (1..$LIVE_JOBS) {
		prep_alternate_start(shift(@PRUNE_QUEUE) // last, $run_prune);
	}
}

sub dump_git_commits { # awaitpid cb
	my ($pid, $batch_out) = @_;
	(defined($pid) && $?) and die "E: @PRUNE_BATCH: \$?=$?";
	return if $DO_QUIT;
	my ($hexlen) = keys(%ALT_FH) or return; # done
	close(delete $ALT_FH{$hexlen}) or die "close: $!";

	$PRUNE_BATCH[1] = "--git-dir=$TMPDIR/hexlen$hexlen.git";
	$pid = spawn(\@PRUNE_BATCH, undef, { 1 => $batch_out });
	awaitpid($pid, \&dump_git_commits, $batch_out);
}

sub run_prune { # OnDestroy when `git config extensions.objectFormat' are done
	my ($self) = @_;
	return if $DO_QUIT;
	pipe(my ($awk_in, $batch_out)) or die "pipe: $!";
	pipe(my ($sort_in, $awk_out)) or die "pipe: $!";
	pipe(my ($comm_in, $sort_out)) or die "pipe: $!";
	my $awk_pid = spawn(\@AWK, $PRUNE_ENV, { 0 => $awk_in, 1 => $awk_out });
	my $sort_pid = spawn(\@SORT, $PRUNE_ENV,
				{ 0 => $sort_in, 1 => $sort_out });
	my ($comm_rd, $comm_pid) = popen_rd(\@COMM, $PRUNE_ENV,
				{ 0 => $comm_in, -C => "$TMPDIR" });
	awaitpid($awk_pid, \&prune_cmd_done, \@AWK);
	awaitpid($sort_pid, \&prune_cmd_done, \@SORT);
	awaitpid($comm_pid, \&prune_cmd_done, \@COMM);
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
	dump_git_commits(undef, $batch_out);
}

sub cidx_read_comm { # via PublicInbox::CidxComm::event_step
	my ($self, $comm_rd) = @_;
	return if $DO_QUIT;
	my ($c, $p) = PublicInbox::PktOp->pair;
	$c->{ops}->{prune_done} = [ $self ];
	my @gone;
	for my $n (0..$#IDX_SHARDS) {
		pipe(my ($r, $w)) or die "pipe: $!";
		push @gone, $w;
		$IDX_SHARDS[$n]->wq_io_do('prune_do', [$r, $p->{op_p}]);
	}
	while (defined(my $cmt = <$comm_rd>)) {
		chomp $cmt;
		my $n = hex(substr($cmt, 0, 8)) % scalar(@gone);
		print { $gone[$n] } 'Q', $cmt, "\0" or die "print: $!";
		last if $DO_QUIT;
	}
	for my $git_dir (@GIT_DIR_GONE) {
		my $n = git_dir_hash($git_dir) % scalar(@gone);
		print { $gone[$n] } 'P', $git_dir, "\0" or die "print: $!";
	}
	for (@gone) { close $_ or die "close: $!" };
}

sub cidx_run { # main entry point
	my ($self) = @_;
	my $restore_umask = prep_umask($self);
	local $SIGSET = PublicInbox::DS::block_signals(
					POSIX::SIGTSTP, POSIX::SIGCONT);
	my $restore = PublicInbox::OnDestroy->new($$,
		\&PublicInbox::DS::sig_setmask, $SIGSET);
	local $LIVE = {};
	local $PRUNE_DONE = [];
	local $IDX_TODO = [];
	local $GIT_TODO = [];
	local ($DO_QUIT, $REINDEX, $TXN_BYTES, @GIT_DIR_GONE, @PRUNE_QUEUE,
		$REPO_CTX, %ALT_FH, $TMPDIR, @AWK, @COMM, @SORT, $PRUNE_ENV);
	local $BATCH_BYTES = $self->{-opt}->{batch_size} //
				$PublicInbox::SearchIdx::BATCH_BYTES;
	local @IDX_SHARDS = cidx_init($self);
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
		my %uniq; # List::Util::uniq requires Perl 5.26+
		@GIT_DIR_GONE = grep { !$uniq{$_}++ } (@GIT_DIR_GONE, @excl);
	}
	local $NCHANGE = 0;
	local $LIVE_JOBS = $self->{-opt}->{jobs} ||
			PublicInbox::IPC::detect_nproc() || 2;
	local @RDONLY_XDB = $self->xdb_shards_flat;
	init_prune($self);
	scan_git_dirs($self) if $self->{-opt}->{scan} // 1;

	# FreeBSD ignores/discards SIGCHLD while signals are blocked and
	# EVFILT_SIGNAL is inactive, so we pretend we have a SIGCHLD pending
	PublicInbox::DS::enqueue_reap();

	local @PublicInbox::DS::post_loop_do = (\&shards_active);
	PublicInbox::DS::event_loop($MY_SIG, $SIGSET) if shards_active();
	PublicInbox::DS->Reset;
	$self->lock_release(!!$NCHANGE);
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
	return if $DO_QUIT || !$LIVE;
	if ($? == 0) { # success
		$quit_req // warn 'BUG: {-cidx_quit} unset';
	} else {
		warn "PID:$pid $shard->{shard} exited with \$?=$?\n";
		++$self->{shard_err} if defined($self->{shard_err});
	}
	PublicInbox::DS::enqueue_reap() if !shards_active(); # once more for PLC
}

1;
