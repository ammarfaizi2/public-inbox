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
use PublicInbox::Eml;
use PublicInbox::DS qw(awaitpid);
use PublicInbox::PktOp;
use PublicInbox::IPC qw(nproc_shards);
use PublicInbox::Admin;
use POSIX qw(WNOHANG SEEK_SET);
use File::Path ();
use File::Spec ();
use PublicInbox::SHA qw(sha256_hex);
use PublicInbox::Search qw(xap_terms);
use PublicInbox::SearchIdx qw(add_val);
use PublicInbox::Config qw(glob2re);
use PublicInbox::Spawn qw(spawn popen_rd);
use PublicInbox::OnDestroy;
use Socket qw(MSG_EOR);
use Carp ();
our (
	$LIVE, # pid => cmd
	$DEFER, # [ [ cb, @args ], ... ]
	$LIVE_JOBS, # integer
	$MY_SIG, # like %SIG
	$SIGSET,
	$TXN_BYTES, # number of bytes in current shard transaction
	$DO_QUIT, # signal number
	@RDONLY_SHARDS, # Xapian::Database
	@IDX_SHARDS, # clones of self
	$MAX_SIZE,
	$TMP_GIT, # PublicInbox::Git object for --prune
	$REINDEX, # PublicInbox::SharedKV
);

# stop walking history if we see >$SEEN_MAX existing commits, this assumes
# branches don't diverge by more than this number of commits...
# git walks commits quickly if it doesn't have to read trees
our $SEEN_MAX = 100000;

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
		indexlevel => $l,
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
	my $xdb = $self->{xdb};
	for (@{$repo->{to_delete}}) { $xdb->delete_document($_) } # XXX needed?
	if (defined $repo->{docid}) {
		my $doc = $xdb->get_document($repo->{docid}) //
			die "$repo->{git_dir} doc #$repo->{docid} gone";
		add_val($doc, PublicInbox::CodeSearch::CT, $repo->{ct});
		my %new = map { $_ => undef } @{$repo->{roots}};
		my $old = xap_terms('G', $doc);
		delete @new{keys %$old};
		$doc->add_boolean_term('G'.$_) for keys %new;
		delete @$old{@{$repo->{roots}}};
		$doc->remove_term('G'.$_) for keys %$old;
		$doc->set_data($repo->{fp});
		$xdb->replace_document($repo->{docid}, $doc);
		$repo->{docid}
	} else {
		my $new = $PublicInbox::Search::X{Document}->new;
		add_val($new, PublicInbox::CodeSearch::CT, $repo->{ct});
		$new->add_boolean_term("P$repo->{git_dir}");
		$new->add_boolean_term('T'.'r');
		$new->add_boolean_term('G'.$_) for @{$repo->{roots}};
		$new->set_data($repo->{fp}); # \n delimited
		$xdb->add_document($new);
	}
}

sub cidx_ckpoint ($$) {
	my ($self, $msg) = @_;
	progress($self, $msg);
	return if $PublicInbox::Search::X{CLOEXEC_UNSET};
	$self->{xdb}->commit_transaction;
	$self->{xdb}->begin_transaction;
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

# sharded reader for `git log --pretty=format: --stdin'
sub shard_index { # via wq_io_do
	my ($self, $git, $n, $roots) = @_;
	local $self->{current_info} = "$git->{git_dir} [$n]";
	local $self->{roots} = $roots;
	my $in = delete($self->{0}) // die 'BUG: no {0} input';
	my $op_p = delete($self->{1}) // die 'BUG: no {1} op_p';
	my $batch_bytes = $self->{-opt}->{batch_size} //
				$PublicInbox::SearchIdx::BATCH_BYTES;
	local $MAX_SIZE = $self->{-opt}->{max_size};
	# local-ized in parent before fork
	$TXN_BYTES = $batch_bytes;
	local $self->{git} = $git; # for patchid
	return if $DO_QUIT;
	my $rd = $git->popen(@LOG_STDIN, undef, { 0 => $in });
	close $in or die "close: $!";
	my $nr = 0;

	# a patch may have \0, see c4201214cbf10636e2c1ab9131573f735b42c8d4
	# in linux.git, so we use $/ = "\n\0" to check end-of-patch
	my $FS = "\n\0";
	my $len;
	my $cmt = {};
	local $/ = $FS;
	my $buf = <$rd> // return close($rd); # leading $FS
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
		$TXN_BYTES -= $len;
		if ($TXN_BYTES <= 0) {
			cidx_ckpoint($self, "[$n] $nr");
			$TXN_BYTES = $batch_bytes - $len;
		}
		update_commit($self, $cmt);
		++$nr;
		if ($TXN_BYTES <= 0) {
			cidx_ckpoint($self, "[$n] $nr");
			$TXN_BYTES = $batch_bytes;
		}
		$/ = $FS;
	}
	close($rd);
	if (!$? || ($DO_QUIT && (($? & 127) == $DO_QUIT ||
				($? & 127) == POSIX::SIGPIPE))) {
		send($op_p, "shard_done $n", MSG_EOR);
	} else {
		warn "E: git @LOG_STDIN: \$?=$?\n";
		$self->{xdb}->cancel_transaction;
	}
}

sub shard_done { # called via PktOp on shard_index completion
	my ($self, $n) = @_;
	$self->{-shard_ok}->{$n} = 1 if defined($self->{-shard_ok});
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

sub run_todo ($) {
	my ($self) = @_;
	my $n;
	while (defined(my $x = shift(@{$self->{todo} // []}))) {
		my $cb = shift @$x;
		$cb->(@$x);
		++$n;
	}
	$n;
}

sub need_reap { # post_loop_do
	my (undef, $jobs) = @_;
	return if !$LIVE || $DO_QUIT;
	scalar(keys(%$LIVE)) > $jobs;
}

sub cidx_reap ($$) {
	my ($self, $jobs) = @_;
	while (run_todo($self)) {}
	local @PublicInbox::DS::post_loop_do = (\&need_reap, $jobs);
	while (need_reap(undef, $jobs)) {
		PublicInbox::DS::event_loop($MY_SIG, $SIGSET);
	}
	while (!$jobs && run_todo($self)) {}
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
	push(@$DEFER, [ $cb, $self, $git, @args ]) if $DEFER;
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
	cidx_reap($self, $LIVE_JOBS);
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
	cidx_reap($self, $LIVE_JOBS);
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
	return if !$LIVE || $DO_QUIT || $git->{-cidx_err};
	my $repo = $git->{-repo} // die 'BUG: no {-repo}';
	if (!defined($repo->{ct})) {
		warn "W: $git->{git_dir} has no commits, skipping\n";
		delete $git->{-repo};
		return;
	}
	my $n = git_dir_hash($git->{git_dir}) % $self->{nshard};
	my $shard = bless { %$self, shard => $n }, ref($self);
	$repo->{shard_n} = $n;
	delete @$shard{qw(lockfh lock_path)};
	local $shard->{xdb} = $RDONLY_SHARDS[$n] // die "BUG: shard[$n] undef";
	$shard->retry_reopen(\&check_existing, $self, $git);
}

sub check_existing { # retry_reopen callback
	my ($shard, $self, $git) = @_;
	my @docids = docids_by_postlist($shard, 'P'.$git->{git_dir});
	my $docid = shift(@docids) // return get_roots($self, $git);
	my $doc = $shard->{xdb}->get_document($docid) //
			die "BUG: no #$docid ($git->{git_dir})";
	my $old_fp = $REINDEX ? "\0invalid" : $doc->get_data;
	if ($old_fp eq $git->{-repo}->{fp}) { # no change
		delete $git->{-repo};
		return;
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
	my ($seen, $nchange) = (0, 0);
	my @shard_in = map {
		$_->reopen;
		open my $fh, '+>', undef or die "open: $!";
		$fh;
	} @RDONLY_SHARDS;

	while (defined(my $cmt = <$rfh>)) {
		chomp $cmt;
		my $n = hex(substr($cmt, 0, 8)) % scalar(@RDONLY_SHARDS);
		if ($REINDEX && $REINDEX->set_maybe(pack('H*', $cmt), '')) {
			say { $shard_in[$n] } $cmt or die "say: $!";
			++$nchange;
		} elsif (seen($RDONLY_SHARDS[$n], 'Q'.$cmt)) {
			last if ++$seen > $SEEN_MAX;
		} else {
			say { $shard_in[$n] } $cmt or die "say: $!";
			++$nchange;
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
		$self->{nchange} += $nchange;
		progress($self, "$git->{git_dir}: $nchange commits");
		for my $fh (@shard_in) {
			$fh->flush or die "flush: $!";
			sysseek($fh, 0, SEEK_SET) or die "seek: $!";
		}
		return @shard_in;
	}
	die "git --git-dir=$git->{git_dir} rev-list: \$?=$?\n";
}

sub shard_commit { # via wq_io_do
	my ($self, $n) = @_;
	my $op_p = delete($self->{0}) // die 'BUG: no {0} op_p';
	$self->commit_txn_lazy;
	send($op_p, "shard_done $n", MSG_EOR);
}

sub consumers_open { # post_loop_do
	my (undef, $consumers) = @_;
	return if $DO_QUIT;
	scalar(grep { $_->{sock} } values %$consumers);
}

sub wait_consumers ($$$) {
	my ($self, $git, $consumers) = @_;
	local @PublicInbox::DS::post_loop_do = (\&consumers_open, $consumers);
	PublicInbox::DS::event_loop($MY_SIG, $SIGSET);
	my $n = grep { ! $self->{-shard_ok}->{$_} } keys %$consumers;
	die "E: $git->{git_dir} $n shards failed" if $n && !$DO_QUIT;
}

sub commit_used_shards ($$$) {
	my ($self, $git, $consumers) = @_;
	local $self->{-shard_ok} = {};
	for my $n (keys %$consumers) {
		my ($c, $p) = PublicInbox::PktOp->pair;
		$c->{ops}->{shard_done} = [ $self ];
		$IDX_SHARDS[$n]->wq_io_do('shard_commit', [ $p->{op_p} ], $n);
		$consumers->{$n} = $c;
	}
	wait_consumers($self, $git, $consumers);
}

sub index_repo { # cidx_await cb
	my ($self, $git, $roots) = @_;
	return if $git->{-cidx_err} || $DO_QUIT;
	my $repo = delete $git->{-repo} or return;
	seek($roots, 0, SEEK_SET) or die "seek: $!";
	chomp(my @roots = <$roots>);
	close($roots) or die "close: $!";
	@roots or return warn("E: $git->{git_dir} has no root commits\n");
	$repo->{roots} = \@roots;
	local $self->{current_info} = $git->{git_dir};
	my @shard_in = partition_refs($self, $git, delete($repo->{refs}));
	local $self->{-shard_ok} = {}; # [0..$#shard_in] => 1
	my $consumers = {};
	for my $n (0..$#shard_in) {
		-s $shard_in[$n] or next;
		last if $DO_QUIT;
		my ($c, $p) = PublicInbox::PktOp->pair;
		$c->{ops}->{shard_done} = [ $self ];
		$IDX_SHARDS[$n]->wq_io_do('shard_index',
					[ $shard_in[$n], $p->{op_p} ],
					$git, $n, \@roots);
		$consumers->{$n} = $c;
	}
	@shard_in = ();
	wait_consumers($self, $git, $consumers);
	if ($DO_QUIT) {
		commit_used_shards($self, $git, $consumers);
		progress($self, "$git->{git_dir}: done");
		return;
	}
	$repo->{git_dir} = $git->{git_dir};
	my $id = $IDX_SHARDS[$repo->{shard_n}]->wq_do('store_repo', $repo);
	if ($id > 0) {
		$consumers->{$repo->{shard_n}} = undef;
		commit_used_shards($self, $git, $consumers);
		progress($self, "$git->{git_dir}: done");
		return run_todo($self);
	}
	die "E: store_repo $git->{git_dir}: id=$id";
}

sub get_roots ($$) {
	my ($self, $git) = @_;
	return if !$LIVE || $DO_QUIT;
	my $refs = $git->{-repo}->{refs} // die 'BUG: no {-repo}->{refs}';
	sysseek($refs, 0, SEEK_SET) or die "seek: $!";
	open my $roots, '+>', undef or die "open: $!";
	my $cmd = [ 'git', "--git-dir=$git->{git_dir}",
			qw(rev-list --stdin --max-parents=0) ];
	my $pid = spawn($cmd, undef, { 0 => $refs, 1 => $roots });
	cidx_await($pid, $cmd, \&index_repo, $self, $git, $roots);
}

# for PublicInbox::SearchIdx::patch_id and with_umask
sub git { $_[0]->{git} }

sub load_existing ($) { # for -u/--update
	my ($self) = @_;
	my $dirs = $self->{git_dirs} // [];
	if ($self->{-opt}->{update} || $self->{-opt}->{prune}) {
		local $self->{xdb};
		$self->xdb or
			die "E: $self->{cidx_dir} non-existent for --update\n";
		my @missing;
		my @cur = grep {
			if (-e $_) {
				1;
			} else {
				push @missing, $_;
				undef;
			}
		} $self->all_terms('P');
		@missing = () if $self->{-opt}->{prune};
		@missing and warn "W: the following repos no longer exist:\n",
				(map { "W:\t$_\n" } @missing),
				"W: use --prune to remove them from ",
				$self->{cidx_dir}, "\n";
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
	local $TXN_BYTES;
	for my $n (0..($self->{nshard} - 1)) {
		my $shard = bless { %$self, shard => $n }, ref($self);
		delete @$shard{qw(lockfh lock_path)};
		$shard->idx_acquire;
		$shard->idx_release;
		$shard->wq_workers_start("shard[$n]", 1, $SIGSET, {
			siblings => \@shards, # for ipc_atfork_child
		}, \&shard_done_wait, $self);
		push @shards, $shard;
	}
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
	for (@{$self->{git_dirs}}) {
		my $git = PublicInbox::Git->new($_);
		my $prep_repo = PublicInbox::OnDestroy->new($$, \&prep_repo,
							$self, $git);
		fp_start($self, $git, $prep_repo);
		ct_start($self, $git, $prep_repo);
		last if $DO_QUIT;
	}
	cidx_reap($self, 0);
}

sub prune_cb { # git->check_async callback
	my ($hex, $type, undef, $self_id) = @_;
	if ($type ne 'commit') {
		my ($self, $id) = @$self_id;
		progress($self, "$hex $type");
		++$self->{pruned};
		$self->{xdb}->delete_document($id);
	}
}

sub shard_prune { # via wq_io_do
	my ($self, $n, $git_dir) = @_;
	my $op_p = delete($self->{0}) // die 'BUG: no {0} op_p';
	my $git = PublicInbox::Git->new($git_dir); # TMP_GIT copy
	$self->begin_txn_lazy;
	my $xdb = $self->{xdb};
	my $cur = $xdb->postlist_begin('Tc');
	my $end = $xdb->postlist_end('Tc');
	my ($id, @cmt, $oid);
	local $self->{pruned} = 0;
	for (; $cur != $end && !$DO_QUIT; $cur++) {
		@cmt = xap_terms('Q', $xdb, $id = $cur->get_docid);
		scalar(@cmt) == 1 or
			warn "BUG? shard[$n] #$id has multiple commits: @cmt";
		for $oid (@cmt) {
			$git->check_async($oid, \&prune_cb, [ $self, $id ]);
		}
	}
	$git->async_wait_all;
	for my $d ($self->all_terms('P')) { # GIT_DIR paths
		last if $DO_QUIT;
		next if -d $d;
		for $id (docids_by_postlist($self, 'P'.$d)) {
			progress($self, "$d gone #$id");
			$xdb->delete_document($id);
		}
	}
	$self->commit_txn_lazy;
	$self->{pruned} and
		progress($self, "[$n] pruned $self->{pruned} commits");
	send($op_p, "shard_done $n", MSG_EOR);
}

sub do_prune ($) {
	my ($self) = @_;
	my $consumers = {};
	my $git_dir = $TMP_GIT->{git_dir};
	my $n = 0;
	local $self->{-shard_ok} = {};
	for my $s (@IDX_SHARDS) {
		my ($c, $p) = PublicInbox::PktOp->pair;
		$c->{ops}->{shard_done} = [ $self ];
		$s->wq_io_do('shard_prune', [ $p->{op_p} ], $n, $git_dir);
		$consumers->{$n++} = $c;
	}
	wait_consumers($self, $TMP_GIT, $consumers);
}

sub shards_active { # post_loop_do
	scalar(grep { $_->{-cidx_quit} } @IDX_SHARDS);
}

# signal handlers
sub kill_shards { $_->wq_kill(@_) for @IDX_SHARDS }

sub parent_quit {
	$DO_QUIT = POSIX->can("SIG$_[0]")->();
	kill_shards(@_);
	warn "# SIG$_[0] received, quitting...\n";
}

sub init_tmp_git_dir ($) {
	my ($self) = @_;
	return unless $self->{-opt}->{prune};
	require File::Temp;
	require PublicInbox::Import;
	my $tmp = File::Temp->newdir('cidx-all-git-XXXX', TMPDIR => 1);
	PublicInbox::Import::init_bare("$tmp", 'cidx-all');
	my $f = "$tmp/objects/info/alternates";
	open my $fh, '>', $f or die "open($f): $!";
	my $o;
	for (@{$self->{git_dirs}}) { # TODO: sha256 check?
		$o = $_.'/objects';
		say $fh $o if -d $o;
	}
	close $fh or die "close($f): $!";
	$TMP_GIT = PublicInbox::Git->new("$tmp");
	$TMP_GIT->{-tmp} = $tmp;
}

sub cidx_run { # main entry point
	my ($self) = @_;
	local $self->{todo} = [];
	local $DEFER = $self->{todo};
	local $SIGSET = PublicInbox::DS::block_signals();
	my $restore = PublicInbox::OnDestroy->new($$,
		\&PublicInbox::DS::sig_setmask, $SIGSET);
	local $LIVE = {};
	local $DO_QUIT;
	local $TMP_GIT;
	local @IDX_SHARDS = cidx_init($self);
	local $self->{current_info} = '';
	local $MY_SIG = {
		CHLD => \&PublicInbox::DS::enqueue_reap,
		USR1 => \&kill_shards,
	};
	$MY_SIG->{$_} = \&parent_quit for qw(TERM QUIT INT);
	my $cb = $SIG{__WARN__} || \&CORE::warn;
	local $SIG{__WARN__} = sub {
		my $m = shift @_;
		$self->{current_info} eq '' or
			$m =~ s/\A(#?\s*)/$1$self->{current_info}: /;
		$cb->($m, @_);
	};
	load_existing($self) unless $self->{-internal};
	local $REINDEX;
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
		@{$self->{git_dirs}} = grep {
			$_ =~ /$re/ ? (warn("# excluding $_\n"), 0) : 1;
		} @{$self->{git_dirs}};
	}
	local $self->{nchange} = 0;
	local $LIVE_JOBS = $self->{-opt}->{jobs} ||
			PublicInbox::IPC::detect_nproc() || 2;
	local @RDONLY_SHARDS = $self->xdb_shards_flat;
	init_tmp_git_dir($self);
	do_prune($self) if $self->{-opt}->{prune};
	scan_git_dirs($self) if $self->{-opt}->{scan} // 1;

	for my $s (@IDX_SHARDS) {
		$s->{-cidx_quit} = 1;
		$s->wq_close;
	}

	local @PublicInbox::DS::post_loop_do = (\&shards_active);
	PublicInbox::DS::event_loop($MY_SIG, $SIGSET) if shards_active();
	$self->lock_release(!!$self->{nchange});
}

sub ipc_atfork_child {
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
		return;
	}
	warn "PID:$pid $shard->{shard} exited with \$?=$?\n";
	++$self->{shard_err} if defined($self->{shard_err});
}

sub with_umask { # TODO
	my ($self, $cb, @arg) = @_;
	$cb->(@arg);
}

1;
