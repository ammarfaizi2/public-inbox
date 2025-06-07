# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# "Solve" blobs which don't exist in git code repositories by
# searching inboxes for post-image blobs.

# this emits a lot of debugging/tracing information which may be
# publicly viewed over HTTP(S).  Be careful not to expose
# local filesystem layouts in the process.
package PublicInbox::SolverGit;
use strict;
use v5.10.1;
use File::Temp 0.19 (); # 0.19 for ->newdir
use autodie qw(mkdir sysseek);
use Fcntl qw(SEEK_SET);
use PublicInbox::Git qw(git_unquote git_quote git_exe);
use PublicInbox::IO qw(write_file);
use PublicInbox::MsgIter qw(msg_part_text);
use PublicInbox::Qspawn;
use PublicInbox::Tmpfile;
use PublicInbox::GitAsyncCat;
use PublicInbox::Eml;
use PublicInbox::Compat qw(uniqstr);
use URI::Escape qw(uri_escape_utf8);
use B qw(cstring); # unconditional quoting, unlike git_quote

# POSIX requires _POSIX_ARG_MAX >= 4096, and xargs is required to
# subtract 2048 bytes.  We also don't factor in environment variable
# headroom into this.
use POSIX qw(sysconf _SC_ARG_MAX);
my $ARG_SIZE_MAX = (sysconf(_SC_ARG_MAX) || 4096) - 2048;
my $OID_MIN = 7;

# By default, "git format-patch" generates filenames with a four-digit
# prefix, so that means 9999 patch series are OK, right? :>
# Maybe we can make this configurable, main concern is disk space overhead
# for uncompressed patch fragments.  Aside from space, public-inbox-httpd
# is otherwise unaffected by having many patches, here, as it can share
# work fairly.  Other PSGI servers may have trouble, though.
my $MAX_PATCH = 9999;

my $LF = qr!\r?\n!;
my $ANY = qr![^\r\n]+!;
my $MODE = '100644|120000|100755';
my $FN = qr!(?:("?[^/\n]+/[^\r\n]+)|/dev/null)!;
my %BAD_COMPONENT = ('' => 1, '.' => 1, '..' => 1);

# di = diff info / a hashref with information about a diff ($di):
# {
#	oid_a => abbreviated pre-image oid,
#	oid_b => abbreviated post-image oid,
#	tmp => anonymous file handle with the diff,
#	hdr_lines => string of various header lines for mode information
#	mode_a => original mode of oid_a (string, not integer),
#	ibx => PublicInbox::Inbox object containing the diff
#	smsg => PublicInbox::Smsg object containing diff
#	path_a => pre-image path
#	path_b => post-image path
#	n => numeric path of the patch (relative to worktree)
# }

sub dbg ($$) {
	print { $_[0]->{out} } $_[1], "\n" or ERR($_[0], "print(dbg): $!");
}

sub done ($$) {
	my ($self, $res) = @_;
	my $ucb = delete($self->{user_cb}) or return;
	$ucb->($res, $self->{uarg});
}

sub ERR ($$) {
	my ($self, $err) = @_;
	print { $self->{out} } $err, "\n";
	eval { done($self, $err) };
	die $err;
}

sub ck_existing_cb { # async_check cb
	my (undef, $oid_full, $type, $size, $arg) = @_;
	my ($self, $want) = @$arg;

	# git <2.21 would show `dangling' (2.21+ shows `ambiguous')
	# https://public-inbox.org/git/20190118033845.s2vlrb3wd3m2jfzu@dcvr/T/
	if ($type =~ /\A(?:missing|ambiguous)\z/ ||
				($oid_full//'') eq 'dangling') {
		undef $oid_full;
		undef $type;
	}
	my $try = $want->{try_gits};
	my $git = shift @$try // die 'BUG: no {try_gits}';

	if ($want->{oid_b} eq ($oid_full // '') || (defined($type) &&
				(!$self->{have_hints} || $type eq 'blob'))) {
		delete $want->{try_gits};
		dbg($self, "found $want->{oid_b} in " .
			join(" ||\n\t", $git->pub_urls($self->{psgi_env})));

		my $existing = [ $git, $oid_full, $type, $size + 0 ];
		if ($want->{oid_b} eq $self->{oid_want} || $type ne 'blob') {
			eval { done($self, $existing) };
			die "E: $@" if $@;
			return;
		}
		mark_found($self, $want->{oid_b}, $existing);
		return next_step($self); # onto patch application
	}

	# parse stderr of "git cat-file --batch-check", async means
	# we may end up slurping output from other requests:
	my @err = grep /\s+(?:[a-f0-9]{7,})\s+(?:blob|commit|tree|tag)\b/g,
			split /^/ms, $git->last_check_err;
	my $prev = $git->{-prev} //= [];
	push @$prev, @err;
	my $oid_b_re = qr/\s+\Q$want->{oid_b}\E[a-f0-9]*\s+/;
	my $ambig = join '', grep /$oid_b_re/, @$prev;
	@$prev = grep !/$oid_b_re/, @$prev;
	delete $git->{-prev} unless @$prev;
	if ($ambig ne '') {
		my @urls = $git->pub_urls($self->{psgi_env});
		rindex($urls[0], '/') >= 0 and
			$ambig =~ s!\b([a-f0-9]{7,})\b!$urls[0]$1/s/!g;
		dbg($self, "`$want->{oid_b}' ambiguous in " .
			join("\n\t", @urls) . "\n" .  $ambig);
	}

	return retry_current($self, $want) if @$try;

	# we may retry if inbox scan (below) fails
	delete $want->{try_gits};

	# scan through inboxes to look for emails which results in
	# the oid we want:
	my $ibx = shift(@{$want->{try_ibxs}}) or return done($self, undef);

	# maybe another inbox has it
	my $srch = $ibx->isrch or return try_harder($self, $want);

	my $post = $want->{oid_b} or die 'BUG: no {oid_b}';
	$post =~ /\A[a-f0-9]+\z/ or die "BUG: oid_b not hex: $post";
	my ($q, $pre)= ("dfpost:$post", $want->{oid_a});
	$q .= " dfpre:$pre" if defined $pre && $pre =~ /\A[a-f0-9]+\z/;
	my $path_b = $want->{path_b};
	if (path_searchable($path_b)) {
		$q .= filename_query($path_b);

		my $path_a = $want->{path_a};
		(path_searchable($path_a) && $path_a ne $path_b) and
			$q .= filename_query($path_a);
	}
	$srch->async_mset($q, { relevance => 1 },
			\&find_results, $self, $want, $ibx);
}

sub find_results { # async_mset cb
	my ($self, $want, $ibx, $mset, $exc) = @_;
	if ($mset && $mset->size) {
		if (my $srch = $ibx->isrch) {
			my $msgs = $srch->mset_to_smsg($ibx, $mset);
			$want->{try_smsgs} = $msgs;
			$want->{cur_ibx} = $ibx;
			$self->{tmp_diffs} = [];
			return retry_current($self, $want);
		}
		my $dir = $ibx->{inboxdir} // $ibx->{topdir};
		warn 'W: ', $dir, " search disappeared, skipping\n";
	}
	try_harder($self, $want);
}

# look for existing objects already in git repos, returns arrayref
# if found, number of remaining git coderepos to try if not.
sub solve_existing ($$) {
	my ($self, $want) = @_;
	my $try = $want->{try_gits} //= [ @{$self->{gits}} ]; # array copy
	my $git = $try->[0] // die 'BUG {try_gits} empty';

	if ($self->{psgi_env}->{'pi-httpd.app'}) {
		async_check({ git => $git }, $want->{oid_b},
				\&ck_existing_cb, [ $self, $want ]);
		$git->schedule_cleanup;
	} else {
		my ($oid_full, $type, $size) = $git->check($want->{oid_b});
		$type //= 'missing';
		ck_existing_cb(undef, $oid_full, $type, $size, [ $self, $want ])
	}
}

sub _tmp {
	$_[0]->{tmp} //=
		File::Temp->newdir("solver.$_[0]->{oid_want}-XXXX", TMPDIR => 1);
}

sub extract_diff ($$) {
	my ($p, $arg) = @_;
	my ($self, $want, $smsg) = @$arg;
	my ($part) = @$p; # ignore $depth and @idx;
	my $ct = $part->content_type || 'text/plain';
	my $post = $want->{oid_b};
	my $pre = $want->{oid_a};
	if (!defined($pre) || $pre !~ /\A[a-f0-9]+\z/) {
		$pre = '[a-f0-9]{7}'; # for RE below
	}

	# Email::MIME::Encodings forces QP to be CRLF upon decoding,
	# change it back to LF:
	my $cte = $part->header('Content-Transfer-Encoding') || '';
	my ($s, undef) = msg_part_text($part, $ct);
	defined $s or return;
	delete $part->{bdy};
	if ($cte =~ /\bquoted-printable\b/i && $part->crlf eq "\n") {
		$s =~ s/\r\n/\n/sg;
	}

	# Quiet "Complex regular subexpression recursion limit" warning.
	# Not much we can do about it, but it's no longer relevant to
	# Perl 5.3x (the warning was removed in 5.37.1, and actual
	# recursino sometime before then).
	no warnings 'regexp';
	$s =~ m!( # $1 start header lines we save for debugging:

		# everything before ^index is optional, but we don't
		# want to match ^(old|copy|rename|deleted|...) unless
		# we match /^diff --git/ first:
		(?: # begin optional stuff:

		# try to get the pre-and-post filenames as $2 and $3
		(?:^diff\x20--git\x20$FN\x20$FN$LF)

		(?:^(?: # pass all this to git-apply:
			# old mode $4
			(?:old\x20mode\x20($MODE))
			|
			# new mode (possibly new file) ($5)
			(?:new\x20(?:file\x20)?mode\x20($MODE))
			|
			(?:(?:copy|rename|deleted|
				dissimilarity|similarity)$ANY)
		)$LF)*

		)? # end of optional stuff, everything below is required

		# match the pre and post-image OIDs as $6 $7
		^index\x20(${pre}[a-f0-9]*)\.\.(${post}[a-f0-9]*)
			# mode if unchanged $8
			(?:\x20(100644|120000|100755))?$LF
	) # end of header lines ($1)
	( # $9 is the patch body
		# "--- a/foo.c" sets pre-filename ($10) in case
		# $2 is missing
		(?:^---\x20$FN$LF)

		# "+++ b/foo.c" sets post-filename ($11) in case
		# $3 is missing or truncated
		(?:^\+{3}\x20$FN$LF)

		# the meat of the diff, including "^\\No newline ..."
		# We also allow for totally blank lines w/o leading spaces,
		# because git-apply(1) handles that case, too
		(?:^(?:[\@\+\x20\-\\][^\n]*|)$LF)+
	)!smx or return;
	undef $s; # free memory

	my $di = {
		hdr_lines => $1,
		oid_a => $6,
		oid_b => $7,
		mode_a => $5 // $8 // $4, # new (file) // unchanged // old
	};
	my $path_a = $2 // $10;
	my $path_b = defined $11 && defined $3 && length $11 > length $3 ?
			$11 // $3 : $3 // $11;
	my $patch = $9;

	# don't care for leading 'a/' and 'b/'
	my (undef, @a) = split(m{/}, git_unquote($path_a)) if defined($path_a);
	my (undef, @b) = split(m{/}, git_unquote($path_b));

	# get rid of path-traversal attempts and junk patches:
	# it's junk at best, an attack attempt at worse:
	foreach (@a, @b) { return if $BAD_COMPONENT{$_} }

	$di->{path_a} = join('/', @a) if @a;
	$di->{path_b} = join('/', @b);

	my $path = ++$self->{tot};
	$di->{n} = $path;
	my $f = _tmp($self)->dirname."/$path";
	write_file '>:utf8', $f, $di->{hdr_lines}, $patch;

	# for debugging/diagnostics:
	$di->{ibx} = $want->{cur_ibx};
	$di->{smsg} = $smsg;

	push @{$self->{tmp_diffs}}, $di;
}

sub path_searchable ($) { defined($_[0]) && $_[0] =~ m!\A[\w/\. \-]+\z! }

# ".." appears in path names, which confuses Xapian into treating
# it as a range query.  So we split on ".." since Xapian breaks
# on punctuation anyways:
sub filename_query ($) {
	join('', map { qq( dfn:"$_") } split(/\.\./, $_[0]));
}

sub find_smsgs ($$$) {
	my ($self, $ibx, $want) = @_;
}

sub update_index_result ($$) {
	my ($bref, $self) = @_;
	my ($qsp_err, $msg) = delete @$self{qw(-qsp_err -msg)};
	ERR($self, "git update-index error:$qsp_err") if $qsp_err;
	dbg($self, $msg);
	next_step($self); # onto do_git_apply
}

sub qsp_qx ($$$) {
	my ($self, $qsp, $cb) = @_;
	$qsp->{qsp_err} = \($self->{-qsp_err} = '');
	$qsp->psgi_qx($self->{psgi_env}, $self->{limiter}, $cb, $self);
}

sub prepare_index ($) {
	my ($self) = @_;
	my $patches = $self->{patches};
	$self->{nr} = 0;

	my $di = $patches->[0] or die 'no patches';
	my $oid_a = $di->{oid_a} or die '{oid_a} unset';
	my $existing = $self->{found}->{$oid_a};

	# no index creation for added files
	$oid_a =~ /\A0+\z/ and return next_step($self);

	die "BUG: $oid_a not found" unless $existing;

	my $oid_full = $existing->[1];
	my $path_a = $di->{path_a} or die "BUG: path_a missing for $oid_full";
	my $mode_a = $di->{mode_a} // '100644';

	my $in = tmpfile("update-index.$oid_full") or die "tmpfile: $!";
	print $in "$mode_a $oid_full\t$path_a\0";
	$in->flush or die "$in->flush: $!";
	sysseek $in, 0, SEEK_SET;

	dbg($self, 'preparing index');
	my $rdr = { 0 => $in };
	my $cmd = [ git_exe, qw(update-index -z --index-info) ];
	my $qsp = PublicInbox::Qspawn->new($cmd, $self->{git_env}, $rdr);
	$path_a = git_quote($path_a);
	$self->{-msg} = "index prepared:\n$mode_a $oid_full\t$path_a";
	qsp_qx $self, $qsp, \&update_index_result;
}

# pure Perl "git init"
sub do_git_init ($) {
	my ($self) = @_;
	my $git_dir = _tmp($self)->dirname.'/git';

	mkdir("$git_dir/$_") for ('', qw(objects refs objects/info refs/heads));
	my $first = $self->{gits}->[0];
	my $fmt = $first->object_format;
	my ($v, @ext) = defined($$fmt) ? (1, <<EOM) : (0);
[extensions]
	objectformat = $$fmt
EOM
	write_file '>', "$git_dir/config", <<EOF, @ext;
[core]
	repositoryFormatVersion = $v
	filemode = true
	bare = false
	logAllRefUpdates = false
EOF
	write_file '>', "$git_dir/HEAD", "ref: refs/heads/master\n";
	write_file '>', "$git_dir/objects/info/alternates", map {
			$_->git_path('objects')."\n"
		} @{$self->{gits}};
	my $tmp_git = $self->{tmp_git} = PublicInbox::Git->new($git_dir);
	$tmp_git->{-tmp} = $self->{tmp};
	$self->{git_env} = {
		GIT_DIR => $git_dir,
		GIT_INDEX_FILE => "$git_dir/index",
		GIT_TEST_FSYNC => 0, # undocumented git env
	};
	prepare_index($self);
}

sub do_finish ($) {
	my ($self) = @_;
	my $oid_want = $self->{oid_want};
	if (my $exists = $self->{found}->{$oid_want}) {
		return done($self, $exists);
	}

	# let git disambiguate if oid_want was too short,
	# but long enough to be unambiguous:
	if ($self->{psgi_env}->{'pi-httpd.app'}) {
		async_check { git => $self->{tmp_git} }, $oid_want,
				\&finish_cb, $self
	} else {
		my ($hex, $type) = $self->{tmp_git}->check($oid_want);
		finish_cb(undef, $hex, $type // 'missing', undef, $self)
	}
}

sub finish_cb { # async_check cb
	my (undef, $oid_full, $type, undef, $self) = @_;
	return done $self, $self->{found}->{$oid_full} if $type eq 'blob';
	my $err = $self->{tmp_git}->last_check_err;
	dbg $self, $err if $err;
	done $self, undef;
}

sub event_step ($) {
	my ($self) = @_;
	eval {
		# step 1: resolve blobs to patches in the todo queue
		if (my $want = pop @{$self->{todo}}) {
			# this populates {patches} and {todo}
			resolve_patch($self, $want);

		# step 2: then we instantiate a working tree once
		# the todo queue is finally empty:
		} elsif (!defined($self->{tmp_git})) {
			do_git_init($self);

		# step 3: apply each patch in the stack
		} elsif (scalar @{$self->{patches}}) {
			do_git_apply($self);

		# step 4: execute the user-supplied callback with
		# our result: (which may be undef)
		# Other steps may call user_cb to terminate prematurely
		# on error
		} elsif (exists $self->{user_cb}) {
			do_finish($self);
		} else {
			die 'about to call user_cb twice'; # Oops :x
		}
	}; # eval
	my $err = $@;
	if ($err) {
		$err =~ s/^\s*Exception:\s*//; # bad word to show users :P
		dbg($self, "E: $err");
		eval { done($self, $err) };
	}
}

sub next_step ($) {
	# if outside of public-inbox-httpd, caller is expected to be
	# looping event_step, anyways
	PublicInbox::DS::requeue($_[0]) if $_[0]->{psgi_env}->{'pi-httpd.app'}
}

sub mark_found ($$$) {
	my ($self, $oid, $found_info) = @_;
	my $found = $self->{found};
	$found->{$oid} = $found_info;
	my $oid_cur = $found_info->[1];
	while ($oid_cur ne $oid && length($oid_cur) > $OID_MIN) {
		$found->{$oid_cur} = $found_info;
		chop($oid_cur);
	}
}

sub parse_ls_files ($$) {
	my ($self, $bref) = @_;
	my $qsp_err = delete $self->{-qsp_err};
	die "git ls-files -s -z error:$qsp_err" if $qsp_err;

	my $di = $self->{-cur_di};
	my @ls = split(/\0/, $$bref);
	my ($line, @extra) = grep(/\t\Q$di->{path_b}\E\z/, @ls);
	scalar(@extra) and die 'BUG? extra files in index:',
				(map { "\n\t".cstring($_) } ($line, @extra));

	# some MUAs add trailing whitespace:
	if (!defined $line && (my $no_tws = $di->{path_b}) =~ s/(\s+)\z//s) {
		my $rm_sp = $1;
		($line, @extra) = grep /\t\Q$no_tws\E\z/, @ls;
		if (defined $line) {
			dbg($self, 'removed trailing space(es) from '.
				cstring($di->{path_b}).' => '.cstring($no_tws));
			$di->{path_b} = $no_tws;
		}
	}
	$line // die 'BUG? no ', cstring($di->{path_b}), ' in',
				(map { "\n\t".cstring($_) } @ls);
	my ($info, $file) = split(/\t/, $line, 2);
	my ($mode_b, $oid_b_full, $stage) = split(/ /, $info);
	$file eq $di->{path_b} or die "BUG? index mismatch: file=",
		cstring($file), ' != path_b=', cstring($di->{path_b});

	dbg($self, "index at:\n$mode_b $oid_b_full\t".git_quote($file));
	my $tmp_git = $self->{tmp_git} or die 'no git working tree';
	if ($self->{psgi_env}->{'pi-httpd.app'}) {
		async_check { git => $tmp_git }, $oid_b_full,
			\&ck_size_cb, $self
	} else {
		ck_size_cb(undef, $tmp_git->check($oid_b_full), $self);
	}
}

sub ck_size_cb { # async_check cb
	my (undef, $oid_b_full, undef, $size, $self) = @_;
	$size // die "check $oid_b_full failed";
	my $di = delete $self->{-cur_di} // die 'BUG: no -cur_di';
	my $created = [ $self->{tmp_git}, $oid_b_full, 'blob', $size, $di ];
	mark_found $self, $di->{oid_b}, $created;
	next_step $self; # onto the next patch
}

sub ls_files_result {
	my ($bref, $self) = @_;
	eval { parse_ls_files($self, $bref) };
	ERR($self, $@) if $@;
}

sub oids_same_ish ($$) {
	(index($_[0], $_[1]) == 0) || (index($_[1], $_[0]) == 0);
}

sub skip_identical ($$$) {
	my ($self, $patches, $cur_oid_b) = @_;
	while (my $nxt = $patches->[0]) {
		if (oids_same_ish($cur_oid_b, $nxt->{oid_b})) {
			dbg($self, 'skipping '.di_url($self, $nxt).
				" for $cur_oid_b");
			shift @$patches;
		} else {
			return;
		}
	}
}

sub apply_result ($$) { # qx_cb
	my ($bref, $self) = @_;
	my ($qsp_err, $di) = delete @$self{qw(-qsp_err -cur_di)};
	dbg($self, $$bref);
	my $patches = $self->{patches};
	if ($qsp_err) {
		my $msg = "git apply error:$qsp_err";
		my $nxt = $patches->[0];
		if ($nxt && oids_same_ish($nxt->{oid_b}, $di->{oid_b})) {
			dbg($self, $msg);
			dbg($self, 'trying '.di_url($self, $nxt));
			return do_git_apply($self);
		} else {
			$msg .= " (no patches left to try for $di->{oid_b})\n";
			dbg($self, $msg);
			return done($self, undef);
		}
	} else {
		skip_identical($self, $patches, $di->{oid_b});
	}

	my @cmd = (git_exe, qw(ls-files -s -z));
	my $qsp = PublicInbox::Qspawn->new(\@cmd, $self->{git_env});
	$self->{-cur_di} = $di;
	qsp_qx $self, $qsp, \&ls_files_result;
}

sub do_git_apply ($) {
	my ($self) = @_;
	my $patches = $self->{patches};

	# we need --ignore-whitespace because some patches are CRLF
	my @cmd = (git_exe, qw(apply --cached --ignore-whitespace
			--unidiff-zero --whitespace=warn --verbose));
	my $len = length(join(' ', @cmd));
	my $di; # keep track of the last one for "git ls-files"
	my $prv_oid_b;

	do {
		my $i = ++$self->{nr};
		$di = shift @$patches;
		dbg($self, "\napplying [$i/$self->{nr_p}] " .
			di_url($self, $di) . "\n" . $di->{hdr_lines});
		my $path = $di->{n};
		$len += length($path) + 1;
		push @cmd, $path;
		$prv_oid_b = $di->{oid_b};
	} while (@$patches && $len < $ARG_SIZE_MAX &&
		 !oids_same_ish($patches->[0]->{oid_b}, $prv_oid_b));

	my $opt = { 2 => 1, -C => _tmp($self)->dirname, quiet => 1 };
	my $qsp = PublicInbox::Qspawn->new(\@cmd, $self->{git_env}, $opt);
	$self->{-cur_di} = $di;
	qsp_qx $self, $qsp, \&apply_result;
}

sub di_url ($$) {
	my ($self, $di) = @_;
	# note: we don't pass the PSGI env unconditionally, here,
	# different inboxes can have different HTTP_HOST on the same instance.
	my $ibx = $di->{ibx};
	my $env = $self->{psgi_env} if $ibx eq $self->{inboxes}->[0];
	my $url = $ibx->base_url($env);
	my $mid = $di->{smsg}->{mid};
	defined($url) ? "$url$mid/" : "<$mid>";
}

sub retry_current {
	my ($self, $want) = @_;
	push @{$self->{todo}}, $want;
	next_step($self); # retry solve_existing
}

sub try_harder ($$) {
	my ($self, $want) = @_;

	# do we have more inboxes to try?
	return retry_current($self, $want) if scalar @{$want->{try_ibxs}};

	my $cur_want = $want->{oid_b};
	if (length($cur_want) > $OID_MIN) { # maybe a shorter OID will work
		delete $want->{try_ibxs}; # drop empty arrayref
		chop($cur_want);
		dbg($self, "retrying $want->{oid_b} as $cur_want");
		$want->{oid_b} = $cur_want;
		return retry_current($self, $want); # retry with shorter abbrev
	}

	dbg($self, "could not find $cur_want");
	eval { done($self, undef) };
	die "E: $@" if $@;
}

sub extract_diffs_done {
	my ($self, $want) = @_;

	delete $want->{try_smsgs};
	delete $want->{cur_ibx};

	my $diffs = delete $self->{tmp_diffs};
	if (scalar @$diffs) {
		unshift @{$self->{patches}}, @$diffs;
		my @u = uniqstr(map { di_url($self, $_) } @$diffs);
		dbg($self, "found $want->{oid_b} in " .  join(" ||\n\t", @u));
		++$self->{nr_p};

		# good, we can find a path to the oid we $want, now
		# lets see if we need to apply more patches:
		my $di = $diffs->[0];
		my $src = $di->{oid_a};

		unless ($src =~ /\A0+\z/) {
			# we have to solve it using another oid, fine:
			my $job = { oid_b => $src, path_b => $di->{path_a} };
			push @{$self->{todo}}, $job;
		}
		return next_step($self); # onto the next todo item
	}
	try_harder($self, $want);
}

sub extract_diff_async { # cat_async cb
	my ($bref, $oid, $type, $size, $x) = @_;
	my ($self, $want, $smsg) = @$x;
	if (defined($oid)) {
		$smsg->{blob} eq $oid or
				ERR($self, "BUG: $smsg->{blob} != $oid");
		PublicInbox::Eml->new($bref)->each_part(\&extract_diff, $x, 1);
	}

	scalar(@{$want->{try_smsgs}}) ? retry_current($self, $want)
					: extract_diffs_done($self, $want);
}

sub resolve_patch ($$) {
	my ($self, $want) = @_;

	if (scalar(@{$self->{patches}}) > $MAX_PATCH) {
		die "Aborting, too many steps to $self->{oid_want}";
	}

	if (my $msgs = $want->{try_smsgs}) {
		my $smsg = shift @$msgs;
		if ($self->{psgi_env}->{'pi-httpd.app'}) {
			return ibx_async_cat($want->{cur_ibx}, $smsg->{blob},
						\&extract_diff_async,
						[$self, $want, $smsg]);
		} else {
			if (my $eml = $want->{cur_ibx}->smsg_eml($smsg)) {
				$eml->each_part(\&extract_diff,
						[ $self, $want, $smsg ], 1);
			}
		}

		return scalar(@$msgs) ? retry_current($self, $want)
					: extract_diffs_done($self, $want);
	}

	# see if we can find the blob in an existing git repo:
	if (!$want->{try_ibxs} && $self->{seen_oid}->{$want->{oid_b}}++) {
		die "Loop detected solving $want->{oid_b}\n";
	}
	$want->{try_ibxs} //= [ @{$self->{inboxes}} ]; # array copy
	solve_existing($self, $want);
}

# this API is designed to avoid creating self-referential structures;
# so user_cb never references the SolverGit object
sub new {
	my ($class, $ibx, $user_cb, $uarg) = @_;
	my $gits = $ibx ? $ibx->{-repo_objs} : undef;

	# FIXME: cindex --join= is super-aggressive and may hit too many
	$gits = [ @$gits[0..2] ] if $gits && @$gits > 3;

	bless { # $ibx is undef if coderepo only (see WwwCoderepo)
		gits => $gits,
		user_cb => $user_cb,
		uarg => $uarg,
		# -cur_di, -qsp_err, -msg => temp fields for Qspawn callbacks

		# TODO: config option for searching related inboxes
		inboxes => $ibx ? [ $ibx ] : [],
	}, $class;
}

# recreate $oid_want using $hints
# hints keys: path_a, path_b, oid_a (note: `oid_b' is NOT a hint)
# Calls {user_cb} with: [ ::Git object, oid_full, type, size, di (diff_info) ]
# with found object, or undef if nothing was found
# Calls {user_cb} with a string error on fatal errors
sub solve ($$$$$) {
	my ($self, $env, $out, $oid_want, $hints) = @_;

	# should we even get here? Probably not, but somebody
	# could be manually typing URLs:
	return done($self, undef) if $oid_want =~ /\A0+\z/;

	$self->{oid_want} = $oid_want;
	$self->{out} = $out;
	$self->{seen_oid} = {};
	$self->{tot} = $self->{nr_p} = 0;
	$self->{psgi_env} = $env;
	$self->{have_hints} = 1 if scalar keys %$hints;
	$self->{todo} = [ { %$hints, oid_b => $oid_want } ];
	$self->{patches} = []; # [ $di, $di, ... ]
	$self->{found} = {}; # { abbr => [ ::Git, oid, type, size, $di ] }

	dbg($self, "solving $oid_want ...");
	if ($env->{'pi-httpd.app'}) {
		PublicInbox::DS::requeue($self);
	} else {
		event_step($self) while $self->{user_cb};
	}
}

1;
