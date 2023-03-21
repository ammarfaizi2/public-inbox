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
use parent qw(PublicInbox::Lock PublicInbox::CodeSearch PublicInbox::SearchIdx);
use PublicInbox::Eml;
use PublicInbox::DS ();
use PublicInbox::IPC qw(nproc_shards);
use PublicInbox::Admin;
use POSIX qw(WNOHANG SEEK_SET);
use File::Path ();
use File::Spec ();
use PublicInbox::SHA qw(sha256_hex);
use PublicInbox::Search qw(xap_terms);
use PublicInbox::SearchIdx qw(add_val);
use PublicInbox::Config;
use PublicInbox::Spawn qw(spawn);
use PublicInbox::OnDestroy;
our $LIVE; # pid => callback
our $LIVE_JOBS;
our @XDB_SHARDS_FLAT;

# stop walking history if we see >$SEEN_MAX existing commits, this assumes
# branches don't diverge by more than this number of commits...
# git walks commits quickly if it doesn't have to read trees
our $SEEN_MAX = 100000;

# TODO: do we care about committer name + email? or tree OID?
my @FMT = qw(H P ct an ae at s b); # (b)ody must be last
my @LOG_STDIN = (qw(log --no-decorate --no-color --no-notes -p --stat -M
	--stdin --no-walk=unsorted), '--pretty=format:%n%x00'.
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

sub add_commit ($$) {
	my ($self, $cmt) = @_; # fields from @FMT
	my $x = 'Q'.$cmt->{H};
	for (docids_by_postlist($self, $x)) {
		$self->{xdb}->delete_document($_)
	}
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
	$self->{xdb}->add_document($doc);
}

sub progress {
	my ($self, @msg) = @_;
	my $pr = $self->{-opt}->{-progress} or return;
	$pr->($self->{git} ? ("$self->{git}->{git_dir}: ") : (), @msg, "\n");
}

sub store_repo ($$$) {
	my ($self, $git, $repo) = @_;
	my $xdb = delete($repo->{shard})->idx_acquire;
	$xdb->begin_transaction;
	for (@{$repo->{to_delete}}) { $xdb->delete_document($_) } # XXX needed?
	if (defined $repo->{id}) {
		my $doc = $xdb->get_document($repo->{id}) //
			die "$git->{git_dir} doc #$repo->{id} gone";
		add_val($doc, PublicInbox::CodeSearch::CT, $repo->{ct});
		my %new = map { $_ => undef } @{$repo->{roots}};
		my $old = xap_terms('G', $doc);
		delete @new{keys %$old};
		$doc->add_boolean_term('G'.$_) for keys %new;
		delete @$old{@{$repo->{roots}}};
		$doc->remove_term('G'.$_) for keys %$old;
		$doc->set_data($repo->{fp});
		$xdb->replace_document($repo->{id}, $doc);
	} else {
		my $new = $PublicInbox::Search::X{Document}->new;
		add_val($new, PublicInbox::CodeSearch::CT, $repo->{ct});
		$new->add_boolean_term("P$git->{git_dir}");
		$new->add_boolean_term('T'.'r');
		$new->add_boolean_term('G'.$_) for @{$repo->{roots}};
		$new->set_data($repo->{fp}); # \n delimited
		$xdb->add_document($new);
	}
	$xdb->commit_transaction;
}

# sharded reader for `git log --pretty=format: --stdin'
sub shard_worker ($$$) {
	my ($self, $r, $sigset) = @_;
	my ($quit, $cmt);
	my $batch_bytes = $self->{-opt}->{batch_size} //
				$PublicInbox::SearchIdx::BATCH_BYTES;
	my $max = $batch_bytes;
	$SIG{USR1} = sub { $max = -1 }; # similar to `git fast-import'
	$SIG{QUIT} = $SIG{TERM} = $SIG{INT} = sub { $quit = shift };
	PublicInbox::DS::sig_setmask($sigset);

	# the parent process of this shard process writes directly to
	# the stdin of `git log', we consume git log's stdout:
	my $rd = $self->{git}->popen(@LOG_STDIN, undef, { 0 => $r });
	close $r or die "close: $!";
	my $nr = 0;

	# a patch may have \0, see c4201214cbf10636e2c1ab9131573f735b42c8d4
	# in linux.git, so we use $/ = "\n\0" to check end-of-patch
	my $FS = "\n\0";
	local $/ = $FS;
	my $buf = <$rd> // return; # leading $FS
	$buf eq $FS or die "BUG: not LF-NUL: $buf\n";
	my $xdb = $self->idx_acquire;
	$xdb->begin_transaction;
	while (defined($buf = <$rd>)) {
		chomp($buf);
		$max -= length($buf);
		@$cmt{@FMT} = split(/\n/, $buf, scalar(@FMT));
		$/ = "\n";
		add_commit($self, $cmt);
		last if $quit; # likely SIGPIPE
		++$nr;
		if ($max <= 0 && !$PublicInbox::Search::X{CLOEXEC_UNSET}) {
			progress($self, $nr);
			$xdb->commit_transaction;
			$max = $batch_bytes;
			$xdb->begin_transaction;
		}
		$/ = $FS;
	}
	close($rd);
	if (!$? || ($quit && ($? & 127) == POSIX::SIGPIPE)) {
		$xdb->commit_transaction;
	} else {
		warn "E: git @LOG_STDIN: \$?=$?\n";
		$xdb->cancel_transaction;
	}
}

sub seen ($$) {
	my ($xdb, $q) = @_; # $q = "Q$COMMIT_HASH"
	$xdb->postlist_begin($q) != $xdb->postlist_end($q)
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

sub cidx_reap ($$) {
	my ($self, $jobs) = @_;
	while (keys(%$LIVE) >= $jobs) {
		my $pid = waitpid(-1, 0) // die "waitpid(-1): $!";
		last if $pid < 0;
		if (my $x = delete $LIVE->{$pid}) {
			my $cb = shift @$x;
			$cb->(@$x) if $cb;
		} else {
			warn "reaped unknown PID=$pid ($?)\n";
		}
	}
}

# this is different from the grokmirror-compatible fingerprint since we
# only care about --heads (branches) and --tags, and not even their names
sub fp_start ($$$) {
	my ($self, $git, $prep_repo) = @_;
	return if !$LIVE; # premature exit
	cidx_reap($self, $LIVE_JOBS);
	open my $refs, '+>', undef or die "open: $!";
	my $pid = spawn(['git', "--git-dir=$git->{git_dir}",
		qw(show-ref --heads --tags --hash)], undef, { 1 => $refs });
	$git->{-repo}->{refs} = $refs;
	$LIVE->{$pid} = [ \&fp_fini, $self, $git, $prep_repo ];
}

sub fp_fini {
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
	return if !$LIVE; # premature exit
	cidx_reap($self, $LIVE_JOBS);
	my ($rd, $pid) = $git->popen([qw[for-each-ref --sort=-committerdate
		--format=%(committerdate:raw) --count=1
		refs/heads/ refs/tags/]]);
	$LIVE->{$pid} = [ \&ct_fini, $self, $git, $rd, $prep_repo ];
}

sub ct_fini {
	my ($self, $git, $rd, $prep_repo) = @_;
	defined(my $ct = <$rd>) or return;
	$ct =~ s/\s+.*\z//s; # drop TZ + LF
	$git->{-repo}->{ct} = $ct + 0;
}

# TODO: also index gitweb.owner and the full fingerprint for grokmirror?
sub prep_repo ($$) {
	my ($self, $git) = @_;
	return if !$LIVE; # premature exit
	my $repo = $git->{-repo} // die 'BUG: no {-repo}';
	my $git_dir = $git->{git_dir};
	if (!defined($repo->{ct})) {
		warn "W: $git_dir has no commits, skipping\n";
		delete $git->{-repo};
		return;
	}
	my $n = git_dir_hash($git_dir) % $self->{nshard};
	my $shard = $repo->{shard} = bless { %$self, shard => $n }, ref($self);
	delete @$shard{qw(lockfh lock_path)};
	my $xdb = $XDB_SHARDS_FLAT[$n] // die "BUG: shard[$n] undef";
	$xdb->reopen;
	my @docids = docids_by_postlist({ xdb => $xdb }, 'P'.$git_dir);
	my $docid = shift(@docids) // return get_roots($self, $git);
	if (@docids) {
		warn "BUG: $git_dir indexed multiple times, culling\n";
		$repo->{to_delete} = \@docids; # XXX needed?
	}
	my $doc = $xdb->get_document($docid) //
		die "BUG: no #$docid ($git_dir)";
	my $old_fp = $doc->get_data;
	if ($old_fp eq $repo->{fp}) { # no change
		progress($self, "$git_dir unchanged");
		delete $git->{-repo};
		return;
	}
	$repo->{id} = $docid;
	get_roots($self, $git);
}

sub partition_refs ($$$) {
	my ($self, $git, $refs) = @_; # show-ref --heads --tags --hash output
	sysseek($refs, 0, SEEK_SET) or die "seek: $!"; # for rev-list --stdin
	my $fh = $git->popen(qw(rev-list --stdin), undef, { 0 => $refs });
	close $refs or die "close: $!";
	my ($seen, $nchange) = (0, 0);
	my @shard_in = map {
		$_->reopen;
		open my $fh, '+>', undef or die "open: $!";
		$fh;
	} @XDB_SHARDS_FLAT;

	while (defined(my $cmt = <$fh>)) {
		chomp $cmt;
		my $n = hex(substr($cmt, 0, 8)) % scalar(@XDB_SHARDS_FLAT);
		if (seen($XDB_SHARDS_FLAT[$n], 'Q'.$cmt)) {
			last if ++$seen > $SEEN_MAX;
		} else {
			say { $shard_in[$n] } $cmt or die "say: $!";
			++$nchange;
			$seen = 0;
		}
	}
	close($fh);
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

sub index_repo {
	my ($self, $git, $roots) = @_;
	return if !$LIVE; # premature exit
	my $repo = delete $git->{-repo} or return;
	seek($roots, 0, SEEK_SET) or die "seek: $!";
	chomp(my @roots = <$roots>);
	close($roots) or die "close: $!";
	@roots or return warn("E: $git->{git_dir} has no root commits\n");
	$repo->{roots} = \@roots;
	local $self->{current_info} = $git->{git_dir};
	my @shard_in = partition_refs($self, $git, delete($repo->{refs}));
	my %pids;
	my $fwd_kill = sub {
		my ($sig) = @_;
		kill($sig, $_) for keys %pids;
	};
	local $SIG{USR1} = $fwd_kill;
	local $SIG{QUIT} = $fwd_kill;
	local $SIG{INT} = $fwd_kill;
	local $SIG{TERM} = $fwd_kill;
	my $sigset = PublicInbox::DS::block_signals();
	for (my $n = 0; $n <= $#shard_in; $n++) {
		-s $shard_in[$n] or next;
		my $pid = fork // die "fork: $!";
		if ($pid == 0) { # no RNG use, here
			$0 = "code index [$n]";
			$self->{git} = $git;
			$self->{shard} = $n;
			$self->{current_info} = "$self->{current_info} [$n]";
			delete @$self{qw(lockfh lock_path)};
			my $in = $shard_in[$n];
			@shard_in = ();
			$self->{roots} = \@roots;
			undef $repo;
			eval { shard_worker($self, $in, $sigset) };
			warn "E: $@" if $@;
			POSIX::_exit($@ ? 1 : 0);
		} else {
			$pids{$pid} = "code index [$n]";
		}
	}
	PublicInbox::DS::sig_setmask($sigset);
	@shard_in = ();
	my ($err, @todo);
	while (keys %pids) {
		my $pid = waitpid(-1, 0) // die "waitpid: $!";
		if (my $j = delete $pids{$pid}) {
			next if $? == 0;
			warn "PID:$pid $j exited with \$?=$?\n";
			$err = 1;
		} elsif (my $todo = delete $LIVE->{$pid}) {
			warn "PID:$pid exited with \$?=$?\n" if $?;
			push @todo, $todo;
		} else {
			warn "reaped unknown PID=$pid ($?)\n";
		}
	}
	die "subprocess(es) failed\n" if $err;
	store_repo($self, $git, $repo);
	progress($self, "$git->{git_dir}: done");
	# TODO: check fp afterwards?
	while (my $x = shift @todo) {
		my $cb = shift @$x;
		$cb->(@$x) if $cb;
	}
}

sub get_roots ($$) {
	my ($self, $git) = @_;
	return if !$LIVE; # premature exit
	cidx_reap($self, $LIVE_JOBS);
	my $refs = $git->{-repo}->{refs} // die 'BUG: no {-repo}->{refs}';
	sysseek($refs, 0, SEEK_SET) or die "seek: $!";
	open my $roots, '+>', undef or die "open: $!";
	my $pid = spawn(['git', "--git-dir=$git->{git_dir}",
			qw(rev-list --stdin --max-parents=0)],
			undef, { 0 => $refs, 1 => $roots });
	$LIVE->{$pid} = [ \&index_repo, $self, $git, $roots ];
}

# for PublicInbox::SearchIdx::patch_id and with_umask
sub git { $_[0]->{git} }

sub load_existing ($) { # for -u/--update
	my ($self) = @_;
	my $dirs = $self->{git_dirs} // [];
	if ($self->{-opt}->{update}) {
		local $self->{xdb};
		$self->xdb or
			die "E: $self->{cidx_dir} non-existent for --update\n";
		my @cur = $self->all_terms('P');
		push @$dirs, @cur;
	}
	my %uniq; # List::Util::uniq requires Perl 5.26+
	@$dirs = grep { !$uniq{$_}++ } @$dirs;
}

sub cidx_init ($) {
	my ($self) = @_;
	my $dir = $self->{cidx_dir};
	unless (-d $dir) {
		warn "# creating $dir\n" if !$self->{-opt}->{quiet};
		File::Path::mkpath($dir);
	}
	for my $n (0..($self->{nshard} - 1)) {
		my $shard = bless { %$self, shard => $n }, ref($self);
		$shard->idx_acquire;
	}
	# this warning needs to happen after idx_acquire
	state $once;
	warn <<EOM if $PublicInbox::Search::X{CLOEXEC_UNSET} && !$once++;
W: Xapian v1.2.21..v1.2.24 were missing close-on-exec on OFD locks,
W: memory usage may be high for large indexing runs
EOM
}

sub scan_git_dirs ($) {
	my ($self) = @_;
	local $LIVE_JOBS = $self->{-opt}->{jobs} //
			PublicInbox::IPC::detect_nproc() // 2;
	local $LIVE = {};
	local @XDB_SHARDS_FLAT = $self->xdb_shards_flat;
	for (@{$self->{git_dirs}}) {
		my $git = PublicInbox::Git->new($_);
		my $prep_repo = PublicInbox::OnDestroy->new($$, \&prep_repo,
							$self, $git);
		fp_start($self, $git, $prep_repo);
		ct_start($self, $git, $prep_repo);
	}
	cidx_reap($self, 0);
}

sub cidx_run {
	my ($self) = @_;
	cidx_init($self);
	local $self->{current_info} = '';
	my $cb = $SIG{__WARN__} || \&CORE::warn;
	local $SIG{__WARN__} = sub {
		my $m = shift @_;
		$self->{current_info} eq '' or
			$m =~ s/\A(#?\s*)/$1$self->{current_info}: /;
		$cb->($m, @_);
	};
	$self->lock_acquire;
	load_existing($self);
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
	local $self->{nchange} = 0;
	# do_prune($self) if $self->{-opt}->{prune}; TODO
	scan_git_dirs($self) if $self->{-opt}->{scan} // 1;
	$self->lock_release(!!$self->{nchange});
}

1;
