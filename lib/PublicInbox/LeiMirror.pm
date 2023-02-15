# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# "lei add-external --mirror" support (also "public-inbox-clone");
package PublicInbox::LeiMirror;
use v5.12;
use parent qw(PublicInbox::IPC);
use IO::Uncompress::Gunzip qw(gunzip $GunzipError);
use IO::Compress::Gzip qw(gzip $GzipError);
use PublicInbox::Spawn qw(popen_rd spawn run_die);
use File::Path ();
use File::Temp ();
use File::Spec ();
use Fcntl qw(SEEK_SET O_CREAT O_EXCL O_WRONLY);
use Carp qw(croak);
use URI;
use PublicInbox::Config;
use PublicInbox::Inbox;
use PublicInbox::Git;
use PublicInbox::LeiCurl;
use PublicInbox::OnDestroy;
use PublicInbox::SHA qw(sha256_hex sha1_hex);
use POSIX qw(strftime);

our $LIVE; # pid => callback
our $FGRP_TODO; # objstore -> [[ to resume ], [ to clone ]]
our $TODO; # reference => [ non-fgrp mirror objects ]
our @PUH; # post-update hooks

sub keep_going ($) {
	$LIVE && (!$_[0]->{lei}->{child_error} ||
		$_[0]->{lei}->{opt}->{'keep-going'});
}

sub _wq_done_wait { # awaitpid cb (via wq_eof)
	my ($pid, $mrr, $lei) = @_;
	if ($?) {
		$lei->child_error($?);
	} elsif (!$lei->{child_error}) {
		if (!$mrr->{dry_run} && $lei->{cmd} ne 'public-inbox-clone') {
			require PublicInbox::LeiAddExternal;
			PublicInbox::LeiAddExternal::_finish_add_external(
							$lei, $mrr->{dst});
		}
		$lei->qerr("# mirrored $mrr->{src} => $mrr->{dst}");
	}
	$lei->dclose;
}

# for old installations without manifest.js.gz
sub try_scrape {
	my ($self, $fallback_manifest) = @_;
	my $uri = URI->new($self->{src});
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $cmd = $curl->for_uri($lei, $uri, '--compressed');
	my $opt = { 0 => $lei->{0}, 2 => $lei->{2} };
	my $fh = popen_rd($cmd, undef, $opt);
	my $html = do { local $/; <$fh> } // die "read(curl $uri): $!";
	close($fh) or return $lei->child_error($?, "@$cmd failed");

	# we grep with URL below, we don't want Subject/From headers
	# making us clone random URLs.  This assumes remote instances
	# prior to public-inbox 1.7.0
	# 5b96edcb1e0d8252 (www: move mirror instructions to /text/, 2021-08-28)
	my @html = split(/<hr>/, $html);
	my @urls = ($html[-1] =~ m!\bgit clone --mirror ([a-z\+]+://\S+)!g);
	if (!@urls && $fallback_manifest) {
		warn <<EOM;
W: failed to extract URLs from $uri, trying manifest.js.gz...
EOM
		return start_clone_url($self);
	}
	my $url = $uri->as_string;
	chop($url) eq '/' or die "BUG: $uri not canonicalized";

	# since this is for old instances w/o manifest.js.gz, try v1 first
	return clone_v1($self) if grep(m!\A\Q$url\E/*\z!, @urls);
	if (my @v2_urls = grep(m!\A\Q$url\E/[0-9]+\z!, @urls)) {
		my %v2_epochs = map {
			my ($n) = (m!/([0-9]+)\z!);
			$n => [ URI->new($_), '' ]
		} @v2_urls; # uniq
		clone_v2_prep($self, \%v2_epochs);
		delete local $lei->{opt}->{epoch};
		clone_all($self);
		return;
	}

	# filter out common URLs served by WWW (e.g /$MSGID/T/)
	if (@urls && $url =~ s!/+[^/]+\@[^/]+/.*\z!! &&
			grep(m!\A\Q$url\E/*\z!, @urls)) {
		die <<"";
E: confused by scraping <$uri>, did you mean <$url>?

	}
	@urls and die <<"";
E: confused by scraping <$uri>, got ambiguous results:
@urls

	die "E: scraping <$uri> revealed nothing\n";
}

sub clone_cmd {
	my ($lei, $opt) = @_;
	my @cmd = qw(git);
	$opt->{$_} = $lei->{$_} for (0..2);
	# we support "-c $key=$val" for arbitrary git config options
	# e.g.: git -c http.proxy=socks5h://127.0.0.1:9050
	push(@cmd, '-c', $_) for @{$lei->{opt}->{c} // []};
	push @cmd, qw(clone --mirror);
	push @cmd, '-q' if $lei->{opt}->{quiet} ||
			($lei->{opt}->{jobs} // 1) > 1;
	push @cmd, '-v' if $lei->{opt}->{verbose};
	# XXX any other options to support?
	# --reference is tricky with multiple epochs, but handled
	# automatically if using manifest.js.gz
	@cmd;
}

sub ft_rename ($$$;$) {
	my ($ft, $dst, $open_mode, $fh) = @_;
	my @st = stat($fh // $dst);
	my $mode = @st ? ($st[2] & 07777) : ($open_mode & ~umask);
	chmod($mode, $ft) or croak "E: chmod($ft): $!";
	require File::Copy;
	File::Copy::mv($ft->filename, $dst) or croak "E: mv($ft => $dst): $!";
	$ft->unlink_on_destroy(0);
}

sub do_reap ($;$) {
	my ($self, $jobs) = @_;
	$jobs //= $self->{-jobs} //= $self->{lei}->{opt}->{jobs} // 1;
	$jobs = 1 if $jobs < 1;
	while (keys(%$LIVE) >= $jobs) {
		my $pid = waitpid(-1, 0) // die "waitpid(-1): $!";
		if (my $x = delete $LIVE->{$pid}) {
			my $cb = shift @$x;
			$cb->(@$x) if $cb;
		} else {
			warn "reaped unknown PID=$pid ($?)\n";
		}
	}
}

sub _get_txt_start { # non-fatal
	my ($self, $endpoint, $fini) = @_;
	my $uri = URI->new($self->{cur_src} // $self->{src});
	my $lei = $self->{lei};
	my $path = $uri->path;
	chop($path) eq '/' or die "BUG: $uri not canonicalized";
	$uri->path("$path/$endpoint");
	my $f = (split(m!/!, $endpoint))[-1];
	my $ft = File::Temp->new(TEMPLATE => "$f-XXXX", TMPDIR => 1);
	my $opt = { 0 => $lei->{0}, 1 => $lei->{1}, 2 => $lei->{2} };
	my $cmd = $self->{curl}->for_uri($lei, $uri, qw(--compressed -R -o),
					$ft->filename);
	do_reap($self);
	$lei->qerr("# @$cmd");
	return if $self->{dry_run};
	$self->{"-get_txt.$endpoint"} = [ $ft, $cmd, $uri ];
	$LIVE->{spawn($cmd, undef, $opt)} =
			[ \&_get_txt_done, $self, $endpoint, $fini ];
}

sub _get_txt_done { # returns true on error (non-fatal), undef on success
	my ($self, $endpoint) = @_;
	my ($fh, $cmd, $uri) = @{delete $self->{"-get_txt.$endpoint"}};
	my $cerr = $?;
	$? = 0; # don't influence normal lei exit
	return warn("$uri missing\n") if ($cerr >> 8) == 22;
	return warn("# @$cmd failed (non-fatal)\n") if $cerr;
	seek($fh, SEEK_SET, 0) or die "seek: $!";
	$self->{"mtime.$endpoint"} = (stat($fh))[9];
	local $/;
	$self->{"txt.$endpoint"} = <$fh>;
	undef; # success
}

sub _write_inbox_config {
	my ($self) = @_;
	my $buf = delete($self->{'txt._/text/config/raw'}) // return;
	my $dst = $self->{cur_dst} // $self->{dst};
	my $f = "$dst/inbox.config.example";
	my $mtime = delete $self->{'mtime._/text/config/raw'};
	if (sysopen(my $fh, $f, O_CREAT|O_EXCL|O_WRONLY)) {
		print $fh $buf or die "print: $!";
		chmod(0444 & ~umask, $fh) or die "chmod($f): $!";
		$fh->flush or die "flush($f): $!";
		if (defined $mtime) {
			utime($mtime, $mtime, $fh) or die "utime($f): $!";
		}
	} elsif (!$!{EEXIST}) {
		die "open($f): $!";
	}
	my $cfg = PublicInbox::Config->git_config_dump($f, $self->{lei}->{2});
	my $ibx = $self->{ibx} = {};
	for my $sec (grep(/\Apublicinbox\./, @{$cfg->{-section_order}})) {
		for (qw(address newsgroup nntpmirror)) {
			$ibx->{$_} = $cfg->{"$sec.$_"};
		}
	}
}

sub set_description ($) {
	my ($self) = @_;
	my $dst = $self->{cur_dst} // $self->{dst};
	chomp(my $orig = PublicInbox::Git::try_cat("$dst/description"));
	my $d = $orig;
	while (defined($d) && ($d =~ m!^\(\$INBOX_DIR/description missing\)! ||
			$d =~ /^Unnamed repository/ || $d !~ /\S/)) {
		$d = delete($self->{'txt.description'});
	}
	$d //= 'mirror of '.($self->{cur_src} // $self->{src});
	atomic_write($dst, 'description', $d."\n") if $d ne $orig;
}

sub index_cloned_inbox {
	my ($self, $iv) = @_;
	my $lei = $self->{lei};

	# n.b. public-inbox-clone works w/o (SQLite || Xapian)
	# lei is useless without Xapian + SQLite
	if ($lei->{cmd} ne 'public-inbox-clone') {
		require PublicInbox::InboxWritable;
		require PublicInbox::Admin;
		my $ibx = delete($self->{ibx}) // {
			address => [ 'lei@example.com' ],
			version => $iv,
		};
		$ibx->{inboxdir} = $self->{cur_dst} // $self->{dst};
		PublicInbox::Inbox->new($ibx);
		PublicInbox::InboxWritable->new($ibx);
		my $opt = {};
		for my $sw ($lei->index_opt) {
			my ($k) = ($sw =~ /\A([\w-]+)/);
			$opt->{$k} = $lei->{opt}->{$k};
		}
		# force synchronous awaitpid for v2:
		local $PublicInbox::DS::in_loop = 0;
		my $cfg = PublicInbox::Config->new(undef, $lei->{2});
		my $env = PublicInbox::Admin::index_prepare($opt, $cfg);
		local %ENV = (%ENV, %$env) if $env;
		PublicInbox::Admin::progress_prepare($opt, $lei->{2});
		PublicInbox::Admin::index_inbox($ibx, undef, $opt);
	}
	return if defined $self->{cur_dst}; # one of many repos to clone
}

sub run_reap {
	my ($lei, $cmd, $opt) = @_;
	$lei->qerr("# @$cmd");
	waitpid(spawn($cmd, undef, $opt), 0) // die "waitpid: $!";
	my $ret = $?;
	$? = 0; # don't let it influence normal exit
	$ret;
}

sub start_cmd {
	my ($self, $cmd, $opt, $fini) = @_;
	do_reap($self);
	utf8::decode(my $msg = "# @$cmd");
	$self->{lei}->qerr($msg);
	return if $self->{dry_run};
	$LIVE->{spawn($cmd, undef, $opt)} = [ \&reap_cmd, $self, $cmd, $fini ]
}

sub fetch_args ($$) {
	my ($lei, $opt) = @_;
	my @cmd; # (git --git-dir=...) to be added by caller
	$opt->{$_} = $lei->{$_} for (0..2);
	# we support "-c $key=$val" for arbitrary git config options
	# e.g.: git -c http.proxy=socks5h://127.0.0.1:9050
	push(@cmd, '-c', $_) for @{$lei->{opt}->{c} // []};
	push @cmd, 'fetch';
	push @cmd, '-q' if $lei->{opt}->{quiet} ||
			($lei->{opt}->{jobs} // 1) > 1;
	push @cmd, '-v' if $lei->{opt}->{verbose};
	push(@cmd, '-p') if $lei->{opt}->{prune};
	PublicInbox::Git::version() >= ((2 << 24) | (29 << 16)) and
		push(@cmd, '--no-write-fetch-head');
	@cmd;
}

sub upr { # feed `git update-ref --stdin -z' verbosely
	my ($lei, $w, $op, @rest) = @_; # ($ref, $oid) = @rest
	$lei->qerr("# $op @rest") if $lei->{opt}->{verbose};
	print $w "$op ", join("\0", @rest, '') or die "print(w): $!";
}

sub start_update_ref {
	my ($fgrp) = @_;
	pipe(my ($r, $w)) or die "pipe: $!";
	my $cmd = [ 'git', "--git-dir=$fgrp->{cur_dst}",
		qw(update-ref --stdin -z) ];
	my $pack = PublicInbox::OnDestroy->new($$, \&satellite_done, $fgrp);
	start_cmd($fgrp, $cmd, { 0 => $r, 2 => $fgrp->{lei}->{2} }, $pack);
	close $r or die "close(r): $!";
	$fgrp->{dry_run} ? undef : $w;
}

sub upref_warn { warn "E: close(update-ref --stdin): $! (need git 1.8.5+)\n" }

sub fgrp_update {
	my ($fgrp) = @_;
	return if !keep_going($fgrp);
	my $srcfh = delete $fgrp->{srcfh} or return;
	my $dstfh = delete $fgrp->{dstfh} or return;
	seek($srcfh, SEEK_SET, 0) or die "seek(src): $!";
	seek($dstfh, SEEK_SET, 0) or die "seek(dst): $!";
	my %src = map { chomp; split(/\0/) } (<$srcfh>);
	close $srcfh;
	my %dst = map { chomp; split(/\0/) } (<$dstfh>);
	close $dstfh;
	my $w = start_update_ref($fgrp) or return;
	my $lei = $fgrp->{lei};
	my $ndel;
	for my $ref (keys %dst) {
		my $new = delete $src{$ref};
		my $old = $dst{$ref};
		if (defined $new) {
			$new eq $old or
				upr($lei, $w, 'update', $ref, $new, $old);
		} else {
			upr($lei, $w, 'delete', $ref, $old);
			++$ndel;
		}
	}
	# git's ref files backend doesn't allow directory/file conflicts
	# between `delete' and `create' ops:
	if ($ndel && scalar(keys %src)) {
		$fgrp->{-create_refs} = \%src;
	} else {
		while (my ($ref, $oid) = each %src) {
			upr($lei, $w, 'create', $ref, $oid);
		}
	}
	close($w) or upref_warn();
}

sub satellite_done {
	my ($fgrp) = @_;
	if (my $create = delete $fgrp->{-create_refs}) {
		my $w = start_update_ref($fgrp) or return;
		while (my ($ref, $oid) = each %$create) {
			upr($fgrp->{lei}, $w, 'create', $ref, $oid);
		}
		close($w) or upref_warn();
	} else {
		pack_refs($fgrp, $fgrp->{cur_dst});
		run_puh($fgrp);
	}
}

sub pack_refs {
	my ($self, $git_dir) = @_;
	my $cmd = [ 'git', "--git-dir=$git_dir", qw(pack-refs --all --prune) ];
	start_cmd($self, $cmd, { 2 => $self->{lei}->{2} });
}

sub fgrpv_done {
	my ($fgrpv) = @_;
	return if !$LIVE;
	my $first = $fgrpv->[0] // die 'BUG: no fgrpv->[0]';
	return if !keep_going($first);
	pack_refs($first, $first->{-osdir}); # objstore refs always packed
	for my $fgrp (@$fgrpv) {
		my $rn = $fgrp->{-remote};
		my %opt = ( 2 => $fgrp->{lei}->{2} );

		my $update_ref = PublicInbox::OnDestroy->new($$,
							\&fgrp_update, $fgrp);

		my $src = [ 'git', "--git-dir=$fgrp->{-osdir}", 'for-each-ref',
			"--format=refs/%(refname:lstrip=3)%00%(objectname)",
			"refs/remotes/$rn/" ];
		open(my $sfh, '+>', undef) or die "open(src): $!";
		$fgrp->{srcfh} = $sfh;
		start_cmd($fgrp, $src, { %opt, 1 => $sfh }, $update_ref);
		my $dst = [ 'git', "--git-dir=$fgrp->{cur_dst}", 'for-each-ref',
			'--format=%(refname)%00%(objectname)' ];
		open(my $dfh, '+>', undef) or die "open(dst): $!";
		$fgrp->{dstfh} = $dfh;
		start_cmd($fgrp, $dst, { %opt, 1 => $dfh }, $update_ref);
	}
}

sub fgrp_fetch_all {
	my ($self) = @_;
	my $todo = $FGRP_TODO;
	$FGRP_TODO = \'BUG on further use';
	keys(%$todo) or return;

	# Rely on the fgrptmp remote groups in the config file rather
	# than listing all remotes since the remote name list may exceed
	# system argv limits:
	my $grp = 'fgrptmp';

	my @git = (@{$self->{-torsocks}}, 'git');
	my $j = $self->{lei}->{opt}->{jobs};
	my $opt = {};
	my @fetch = do {
		local $self->{lei}->{opt}->{jobs} = 1;
		(fetch_args($self->{lei}, $opt), qw(--no-tags --multiple));
	};
	push(@fetch, "-j$j") if $j;
	while (my ($osdir, $fgrp_old_new) = each %$todo) {
		my $f = "$osdir/config";
		return if !keep_going($self);
		my ($old, $new) = @$fgrp_old_new;
		@$old = sort { $b->{-sort} <=> $a->{-sort} } @$old;
		# $new is ordered by {references}
		my $cmd = ['git', "--git-dir=$osdir", qw(config -f), $f ];

		# clobber settings from previous run atomically
		for ("remotes.$grp", 'fetch.hideRefs') {
			my $c = [ @$cmd, '--unset-all', $_ ];
			$self->{lei}->qerr("# @$c");
			next if $self->{dry_run};
			my $pid = spawn($c, undef, $opt);
			waitpid($pid, 0) // die "waitpid: $!";
			die "E: @$c \$?=$?" if ($? && ($? >> 8) != 5);
		}

		# permanent configs:
		my $cfg = PublicInbox::Config->git_config_dump($f);
		for my $fgrp (@$old, @$new) {
			my $u = $fgrp->{-uri} // die 'BUG: no {-uri}';
			my $rn = $fgrp->{-remote} // die 'BUG: no {-remote}';
			for ("url=$u", "fetch=+refs/*:refs/remotes/$rn/*",
					'tagopt=--no-tags') {
				my ($k, $v) = split(/=/, $_, 2);
				$k = "remote.$rn.$k";
				next if ($cfg->{$k} // '') eq $v;
				my $c = [@$cmd, $k, $v];
				$fgrp->{lei}->qerr("# @$c");
				next if $fgrp->{dry_run};
				run_die($c, undef, $opt);
			}
		}

		if (!$self->{dry_run}) {
			# update the config atomically via O_APPEND while
			# respecting git-config locking
			sysopen(my $lk, "$f.lock", O_CREAT|O_EXCL|O_WRONLY)
				or die "open($f.lock): $!";
			open my $fh, '>>', $f or die "open(>>$f): $!";
			$fh->autoflush(1);
			my $buf = '';
			if (@$old) {
				$buf = "[fetch]\n\thideRefs = refs\n";
				$buf .= join('', map {
					"\thideRefs = !refs/remotes/" .
						"$_->{-remote}/\n";
				} @$old);
			}
			$buf .= join('', "[remotes]\n",
				(map { "\t$grp = $_->{-remote}\n" } @$old),
				(map { "\t$grp = $_->{-remote}\n" } @$new));
			print $fh $buf or die "print($f): $!";
			close $fh or die "close($f): $!";
			unlink("$f.lock") or die "unlink($f.lock): $!";
		}
		$cmd  = [ @git, "--git-dir=$osdir", @fetch, $grp ];
		push @$old, @$new;
		my $end = PublicInbox::OnDestroy->new($$, \&fgrpv_done, $old);
		start_cmd($self, $cmd, $opt, $end);
	}
}

# keep this idempotent for future use by public-inbox-fetch
sub forkgroup_prep {
	my ($self, $uri) = @_;
	$self->{-ent} // return;
	my $os = $self->{-objstore} // return;
	my $fg = $self->{-ent}->{forkgroup} // return;
	my $dir = "$os/$fg.git";
	if (!-d $dir && !$self->{dry_run}) {
		PublicInbox::Import::init_bare($dir);
		my $f = "$dir/config";
		open my $fh, '+>>', $f or die "open:($f): $!";
		print $fh <<EOM or die "print($f): $!";
[repack]
	useDeltaIslands = true
[pack]
	island = refs/remotes/([^/]+)/
EOM
		close $fh or die "close($f): $!";
	}
	my $key = $self->{-key} // die 'BUG: no -key';
	my $rn = substr(sha256_hex($key), 0, 16);
	if (!-d $self->{cur_dst} && !$self->{dry_run}) {
		PublicInbox::Import::init_bare($self->{cur_dst});
		my $f = "$self->{cur_dst}/config";
		open my $fh, '+>>', $f or die "open:($f): $!";
		print $fh <<EOM or die "print($f): $!";
; rely on the "$rn" remote in the
; $fg fork group for fetches
; only uncomment the following iff you detach from fork groups
; [remote "origin"]
;	url = $uri
;	fetch = +refs/*:refs/*
;	mirror = true
EOM
		close $fh or die "close($f): $!";
	}
	if (!$self->{dry_run}) {
		my $alt = File::Spec->rel2abs("$dir/objects");
		my $o = "$self->{cur_dst}/objects";
		my $f = "$o/info/alternates";
		my $l = File::Spec->abs2rel($alt, File::Spec->rel2abs($o));
		open my $fh, '+>>', $f or die "open($f): $!";
		seek($fh, SEEK_SET, 0) or die "seek($f): $!";
		chomp(my @cur = <$fh>);
		if (!grep(/\A\Q$l\E\z/, @cur)) {
			say $fh $l or die "say($f): $!";
		}
		close $fh or die "close($f): $!";
	}
	bless {
		%$self, -osdir => $dir, -remote => $rn, -uri => $uri
	}, __PACKAGE__;
}

sub fp_done {
	my ($self, $cmd, $cb, @arg) = @_;
	if ($?) {
		$self->{lei}->err("@$cmd failed (\$?=$?) (non-fatal)");
		$? = 0; # don't let it influence normal exit
	}
	return if !keep_going($self);
	my $fh = delete $self->{-show_ref} // die 'BUG: no show-ref output';
	seek($fh, SEEK_SET, 0) or die "seek(show_ref): $!";
	$self->{-ent} // die 'BUG: no -ent';
	my $A = $self->{-ent}->{fingerprint} // die 'BUG: no fingerprint';
	my $B = sha1_hex(do { local $/; <$fh> } // die("read(show_ref): $!"));
	return $cb->($self, @arg) if $A ne $B;
	$self->{lei}->qerr("# $self->{-key} up-to-date");
}

sub cmp_fp_do {
	my ($self, $cb, @arg) = @_;
	# $cb is either resume_fetch or fgrp_enqueue
	$self->{-ent} // return $cb->($self, @arg);
	my $new = $self->{-ent}->{fingerprint} // return $cb->($self, @arg);
	my $key = $self->{-key} // die 'BUG: no -key';
	if (my $cur_ent = $self->{-local_manifest}->{$key}) {
		# runs go_fetch->DESTROY run if eq
		return if $cur_ent->{fingerprint} eq $new;
	}
	my $dst = $self->{cur_dst} // $self->{dst};
	my $cmd = ['git', "--git-dir=$dst", 'show-ref'];
	my $opt = { 2 => $self->{lei}->{2} };
	open($opt->{1}, '+>', undef) or die "open(tmp): $!";
	$self->{-show_ref} = $opt->{1};
	do_reap($self);
	$self->{lei}->qerr("# @$cmd");
	$LIVE->{spawn($cmd, undef, $opt)} = [ \&fp_done, $self, $cmd,
						$cb, @arg ];
}

sub resume_fetch {
	my ($self, $uri, $fini) = @_;
	return if !keep_going($self);
	my $dst = $self->{cur_dst} // $self->{dst};
	my @git = ('git', "--git-dir=$dst");
	my $opt = { 2 => $self->{lei}->{2} };
	my $rn = 'random'.int(rand(1 << 30));
	for ("url=$uri", "fetch=+refs/*:refs/*", 'mirror=true') {
		push @git, '-c', "remote.$rn.$_";
	}
	my $cmd = [ @{$self->{-torsocks}}, @git,
			fetch_args($self->{lei}, $opt), $rn ];
	push @$cmd, '-P' if $self->{lei}->{prune}; # --prune-tags implied
	my $run_puh = PublicInbox::OnDestroy->new($$, \&run_puh, $self, $fini);
	++$self->{chg}->{nr_chg};
	start_cmd($self, $cmd, $opt, $run_puh);
}

sub fgrp_enqueue {
	my ($fgrp, $end) = @_; # $end calls fgrp_fetch_all
	return if !keep_going($fgrp);
	++$fgrp->{chg}->{nr_chg};
	my $dst = $FGRP_TODO->{$fgrp->{-osdir}} //= [ [], [] ]; # [ old, new ]
	push @{$dst->[defined($fgrp->{-sort} ? 0 : 1)]}, $fgrp;
}

sub clone_v1 {
	my ($self, $end) = @_;
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $uri = URI->new($self->{cur_src} // $self->{src});
	my $path = $uri->path;
	$path =~ s!/*\z!! and $uri->path($path);
	defined($lei->{opt}->{epoch}) and
		die "$uri is a v1 inbox, --epoch is not supported\n";
	$self->{-torsocks} //= $curl->torsocks($lei, $uri) or return;
	my $dst = $self->{cur_dst} // $self->{dst};
	my $fini = PublicInbox::OnDestroy->new($$, \&v1_done, $self);
	my $resume = -d $dst;
	if (my $fgrp = forkgroup_prep($self, $uri)) {
		$fgrp->{-fini} = $fini;
		if ($resume) {
			$fgrp->{-sort} = $fgrp->{-ent}->{modified};
			cmp_fp_do($fgrp, \&fgrp_enqueue, $end);
		} else { # new repo, save for last
			fgrp_enqueue($fgrp, $end);
		}
	} elsif ($resume) {
		cmp_fp_do($self, \&resume_fetch, $uri, $fini);
	} else { # normal clone
		my $cmd = [ @{$self->{-torsocks}},
				clone_cmd($lei, my $opt = {}), "$uri", $dst ];
		if (defined($self->{-ent})) {
			if (defined(my $ref = $self->{-ent}->{reference})) {
				-e "$self->{dst}$ref" and
					push @$cmd, '--reference',
						"$self->{dst}$ref";
			}
		}
		++$self->{chg}->{nr_chg};
		start_cmd($self, $cmd, $opt, PublicInbox::OnDestroy->new($$,
						\&run_puh, $self, $fini));
	}
	if (!$self->{-is_epoch} && $lei->{opt}->{'inbox-config'} =~
				/\A(?:always|v1)\z/s) {
		_get_txt_start($self, '_/text/config/raw', $fini);
	}

	my $d = $self->{-ent} ? $self->{-ent}->{description} : undef;
	$self->{'txt.description'} = $d if defined $d;
	(!defined($d) && !$end) and
		_get_txt_start($self, 'description', $fini);

	$end or do_reap($self, 1); # for non-manifest clone
}

sub parse_epochs ($$) {
	my ($opt_epochs, $v2_epochs) = @_; # $epochs "LOW..HIGH"
	$opt_epochs // return; # undef => all epochs
	my ($lo, $dotdot, $hi, @extra) = split(/(\.\.)/, $opt_epochs);
	undef($lo) if ($lo // '') eq '';
	my $re = qr/\A~?[0-9]+\z/;
	if (@extra || (($lo // '0') !~ $re) ||
			(($hi // '0') !~ $re) ||
			!(grep(defined, $lo, $hi))) {
		die <<EOM;
--epoch=$opt_epochs not in the form of `LOW..HIGH', `LOW..', nor `..HIGH'
EOM
	}
	my @n = sort { $a <=> $b } keys %$v2_epochs;
	for (grep(defined, $lo, $hi)) {
		if (/\A[0-9]+\z/) {
			$_ > $n[-1] and die
"`$_' exceeds maximum available epoch ($n[-1])\n";
			$_ < $n[0] and die
"`$_' is lower than minimum available epoch ($n[0])\n";
		} elsif (/\A~([0-9]+)/) {
			my $off = -$1 - 1;
			$n[$off] // die "`$_' is out of range\n";
			$_ = $n[$off];
		} else { die "`$_' not understood\n" }
	}
	defined($lo) && defined($hi) && $lo > $hi and die
"low value (`$lo') exceeds high (`$hi')\n";
	$lo //= $n[0] if $dotdot;
	$hi //= $n[-1] if $dotdot;
	$hi //= $lo;
	my $want = {};
	for ($lo..$hi) {
		if (defined $v2_epochs->{$_}) {
			$want->{$_} = 1;
		} else {
			warn
"# epoch $_ is not available (non-fatal, $lo..$hi)\n";
		}
	}
	$want
}

sub init_placeholder ($$$) {
	my ($src, $edst, $ent) = @_;
	PublicInbox::Import::init_bare($edst);
	my $f = "$edst/config";
	open my $fh, '>>', $f or die "open($f): $!";
	print $fh <<EOM or die "print($f): $!";
[remote "origin"]
	url = $src
	fetch = +refs/*:refs/*
	mirror = true

; This git epoch was created read-only and "public-inbox-fetch"
; will not fetch updates for it unless write permission is added.
; Hint: chmod +w $edst
EOM
	if (defined($ent->{owner})) {
		print $fh <<EOM or die "print($f): $!";
[gitweb]
	owner = $ent->{owner}
EOM
	}
	close $fh or die "close($f): $!";
	my %map = (head => 'HEAD', description => undef);
	while (my ($key, $fn) = each %map) {
		my $val = $ent->{$key} // next;
		$fn //= $key;
		$fn = "$edst/$fn";
		open $fh, '>', $fn or die "open($fn): $!";
		print $fh $val, "\n" or die "print($fn): $!";
		close $fh or die "close($fn): $!";
	}
}

sub reap_cmd { # async, called via SIGCHLD
	my ($self, $cmd) = @_;
	my $cerr = $?;
	$? = 0; # don't let it influence normal exit
	$self->{lei}->child_error($cerr, "@$cmd failed (\$?=$cerr)") if $cerr;
}

sub up_fp_done {
	my ($self) = @_;
	return if !keep_going($self);
	my $fh = delete $self->{-show_ref_up} // die 'BUG: no show-ref output';
	seek($fh, SEEK_SET, 0) or die "seek(show_ref): $!";
	$self->{-ent} // die 'BUG: no -ent';
	my $A = $self->{-ent}->{fingerprint} // die 'BUG: no fingerprint';
	my $B = sha1_hex(do { local $/; <$fh> } // die("read(show_ref): $!"));
	return if $A eq $B;
	$self->{-ent}->{fingerprint} = $B;
	push @{$self->{chg}->{fp_mismatch}}, $self->{-key};
}

sub atomic_write ($$$) {
	my ($dn, $bn, $raw) = @_;
	my $ft = File::Temp->new(DIR => $dn, TEMPLATE => "$bn-XXXX");
	print $ft $raw or die "print($ft): $!";
	$ft->flush or die "flush($ft): $!";
	ft_rename($ft, "$dn/$bn", 0666);
}

sub run_next_puh {
	my ($self) = @_;
	my $puh = shift @{$self->{-puh_todo}} // return delete($self->{-fini});
	my $fini = PublicInbox::OnDestroy->new($$, \&run_next_puh, $self);
	my $cmd = [ @$puh, ($self->{cur_dst} // $self->{dst}) ];
	my $opt = +{ map { $_ => $self->{lei}->{$_} } (0..2) };
	start_cmd($self, $cmd, undef, $opt, $fini);
}

sub run_puh {
	my ($self, $fini) = @_;
	$self->{-fini} = $fini;
	@{$self->{-puh_todo}} = @PUH;
	run_next_puh($self);
}

# modifies the to-be-written manifest entry, and sets values from it, too
sub update_ent {
	my ($self) = @_;
	my $key = $self->{-key} // die 'BUG: no -key';
	my $new = $self->{-ent}->{fingerprint};
	my $cur = $self->{-local_manifest}->{$key}->{fingerprint} // "\0";
	my $dst = $self->{cur_dst} // $self->{dst};
	if (defined($new) && $new ne $cur) {
		my $cmd = ['git', "--git-dir=$dst", 'show-ref'];
		my $opt = { 2 => $self->{lei}->{2} };
		open($opt->{1}, '+>', undef) or die "open(tmp): $!";
		$self->{-show_ref_up} = $opt->{1};
		my $done = PublicInbox::OnDestroy->new($$, \&up_fp_done, $self);
		start_cmd($self, $cmd, $opt, $done);
	}
	$new = $self->{-ent}->{head};
	$cur = $self->{-local_manifest}->{$key}->{head} // "\0";
	if (defined($new) && $new ne $cur) {
		# n.b. grokmirror writes raw contents to $dst/HEAD w/o locking
		my $cmd = [ 'git', "--git-dir=$dst" ];
		if ($new =~ s/\Aref: //) {
			push @$cmd, qw(symbolic-ref HEAD), $new;
		} elsif ($new =~ /\A[a-f0-9]{40,}\z/) {
			push @$cmd, qw(update-ref --no-deref HEAD), $new;
		} else {
			undef $cmd;
			warn "W: $key: {head} => `$new' not understood\n";
		}
		start_cmd($self, $cmd, { 2 => $self->{lei}->{2} }) if $cmd;
	}
	if (my $symlinks = $self->{-ent}->{symlinks}) {
		my $top = File::Spec->rel2abs($self->{dst});
		push @{$self->{-new_symlinks}}, @$symlinks;
		for my $p (@$symlinks) {
			my $ln = "$top/$p";
			$ln =~ tr!/!/!s;
			my (undef, $dn, $bn) = File::Spec->splitpath($ln);
			File::Path::mkpath($dn);
			my $tgt = "$top/$key";
			$tgt = File::Spec->abs2rel($tgt, $dn);
			if (lstat($ln)) {
				if (-l _) {
					next if readlink($ln) eq $tgt;
					unlink($ln) or die "unlink($ln): $!";
				} else {
					push @{$self->{chg}->{badlink}}, $p;
				}
			}
			symlink($tgt, $ln) or die "symlink($tgt, $ln): $!";
			++$self->{chg}->{nr_chg};
		}
	}
	if (defined(my $t = $self->{-ent}->{modified})) {
		my ($dn, $bn) = ("$dst/info/web", 'last-modified');
		my $orig = PublicInbox::Git::try_cat("$dn/$bn");
		$t = strftime('%F %T', gmtime($t))." +0000\n";
		File::Path::mkpath($dn);
		atomic_write($dn, $bn, $t) if $orig ne $t;
	}

	$new = $self->{-ent}->{owner} // return;
	$cur = $self->{-local_manifest}->{$key}->{owner} // "\0";
	return if $cur eq $new;
	my $cmd = [ qw(git config -f), "$dst/config", 'gitweb.owner', $new ];
	start_cmd($self, $cmd, { 2 => $self->{lei}->{2} });
}

sub v1_done { # called via OnDestroy
	my ($self) = @_;
	return if $self->{dry_run} || !keep_going($self);
	_write_inbox_config($self);
	my $dst = $self->{cur_dst} // $self->{dst};
	update_ent($self) if $self->{-ent};
	my $o = "$dst/objects";
	if (open(my $fh, '<', my $fn = "$o/info/alternates")) {;
		my $base = File::Spec->rel2abs($o);
		my @l = <$fh>;
		my $ft;
		for (@l) {
			next unless m!\A/!;
			$_ = File::Spec->abs2rel($_, $base);
			$ft //= File::Temp->new(TEMPLATE => '.XXXX',
						DIR => "$o/info");
		}
		if ($ft) {
			print $ft @l or die "print($ft): $!";
			$ft->flush or die "flush($ft): $!";
			ft_rename($ft, $fn, 0666, $fh);
		}
	}
	eval { set_description($self) };
	warn $@ if $@;
	return if ($self->{-is_epoch} ||
		$self->{lei}->{opt}->{'inbox-config'} ne 'always');
	write_makefile($dst, 1);
	index_cloned_inbox($self, 1);
}

sub v2_done { # called via OnDestroy
	my ($self) = @_;
	return if $self->{dry_run} || !keep_going($self);
	my $dst = $self->{cur_dst} // $self->{dst};
	require PublicInbox::Lock;
	my $lk = bless { lock_path => "$dst/inbox.lock" }, 'PublicInbox::Lock';
	my $lck = $lk->lock_for_scope($$);
	_write_inbox_config($self);
	require PublicInbox::MultiGit;
	my $mg = PublicInbox::MultiGit->new($dst, 'all.git', 'git');
	$mg->fill_alternates;
	for my $i ($mg->git_epochs) { $mg->epoch_cfg_set($i) }
	for my $edst (@{delete($self->{-read_only}) // []}) {
		my @st = stat($edst) or die "stat($edst): $!";
		chmod($st[2] & 0555, $edst) or die "chmod(a-w, $edst): $!";
	}
	write_makefile($dst, 2);
	undef $lck; # unlock
	eval { set_description($self) };
	warn $@ if $@;
	index_cloned_inbox($self, 2);
}

sub clone_v2_prep ($$;$) {
	my ($self, $v2_epochs, $m) = @_; # $m => manifest.js.gz hashref
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $first_uri = (map { $_->[0] } values %$v2_epochs)[0];
	$self->{-torsocks} //= $curl->torsocks($lei, $first_uri) or return;
	my $dst = $self->{cur_dst} // $self->{dst};
	my $want = parse_epochs($lei->{opt}->{epoch}, $v2_epochs);
	my $task = $m ? bless { %$self }, __PACKAGE__ : $self;
	my (@skip, $desc);
	my $fini = PublicInbox::OnDestroy->new($$, \&v2_done, $task);
	for my $nr (sort { $a <=> $b } keys %$v2_epochs) {
		my ($uri, $key) = @{$v2_epochs->{$nr}};
		my $src = $uri->as_string;
		my $edst = $dst;
		$src =~ m!/([0-9]+)(?:\.git)?\z! or die <<"";
failed to extract epoch number from $src

		$1 + 0 == $nr or die "BUG: <$uri> miskeyed $1 != $nr";
		$edst .= "/git/$nr.git";
		my $ent;
		if ($m) {
			$ent = $m->{$key} //
				die("BUG: `$key' not in manifest.js.gz");
			if (defined(my $d = $ent->{description})) {
				$d =~ s/ \[epoch [0-9]+\]\z//s;
				$desc = $d;
			}
		}
		if (!$want || $want->{$nr}) {
			my $etask = bless { %$task, -key => $key }, __PACKAGE__;
			$etask->{-ent} = $ent; # may have {reference}
			$etask->{cur_src} = $src;
			$etask->{cur_dst} = $edst;
			$etask->{-is_epoch} = $fini;
			my $ref = $ent->{reference} // '';
			push @{$TODO->{$ref}}, $etask;
			$self->{any_want}->{$key} = 1;
		} else { # create a placeholder so users only need to chmod +w
			init_placeholder($src, $edst, $ent);
			push @{$task->{-read_only}}, $edst;
			push @skip, $key;
		}
	}
	# filter out the epochs we skipped
	$self->{chg}->{manifest} = 1 if $m && delete(@$m{@skip});

	(!$self->{dry_run} && !-d $dst) and File::Path::mkpath($dst);

	$lei->{opt}->{'inbox-config'} =~ /\A(?:always|v2)\z/s and
		_get_txt_start($task, '_/text/config/raw', $fini);

	defined($desc) ? ($task->{'txt.description'} = $desc) :
		_get_txt_start($task, 'description', $fini);
}

sub decode_manifest ($$$) {
	my ($fh, $fn, $uri) = @_;
	my $js;
	my $gz = do { local $/; <$fh> } // die "slurp($fn): $!";
	gunzip(\$gz => \$js, MultiStream => 1) or
		die "gunzip($uri): $GunzipError\n";
	my $m = eval { PublicInbox::Config->json->decode($js) };
	die "$uri: error decoding `$js': $@\n" if $@;
	ref($m) eq 'HASH' or die "$uri unknown type: ".ref($m);
	$m;
}

sub load_current_manifest ($) {
	my ($self) = @_;
	my $fn = $self->{-manifest} // return;
	if (open(my $fh, '<', $fn)) {
		decode_manifest($fh, $fn, $fn);
	} elsif ($!{ENOENT}) { # non-fatal, we can just do it slowly
		warn "open($fn): $!\n" if !$self->{-initial_clone};
		undef;
	} else {
		die "open($fn): $!\n";
	}
}

sub multi_inbox ($$$) {
	my ($self, $path, $m) = @_;
	my $incl = $self->{lei}->{opt}->{include};
	my $excl = $self->{lei}->{opt}->{exclude};

	# assuming everything not v2 is v1, for now
	my @v1 = sort grep(!m!.+/git/[0-9]+\.git\z!, keys %$m);
	my @v2_epochs = sort grep(m!.+/git/[0-9]+\.git\z!, keys %$m);
	my $v2 = {};

	for (@v2_epochs) {
		m!\A(/.+)/git/[0-9]+\.git\z! or die "BUG: $_";
		push @{$v2->{$1}}, $_;
	}
	my $n = scalar(keys %$v2) + scalar(@v1);
	my @orig = defined($incl // $excl) ? (keys %$v2, @v1) : ();
	if (defined $incl) {
		my $re = '(?:'.join('\\z|', map {
				$self->{lei}->glob2re($_) // qr/\A\Q$_\E/
			} @$incl).'\\z)';
		my @gone = delete @$v2{grep(!/$re/, keys %$v2)};
		delete @$m{map { @$_ } @gone} and $self->{chg}->{manifest} = 1;
		delete @$m{grep(!/$re/, @v1)} and $self->{chg}->{manifest} = 1;
		@v1 = grep(/$re/, @v1);
	}
	if (defined $excl) {
		my $re = '(?:'.join('\\z|', map {
				$self->{lei}->glob2re($_) // qr/\A\Q$_\E/
			} @$excl).'\\z)';
		my @gone = delete @$v2{grep(/$re/, keys %$v2)};
		delete @$m{map { @$_ } @gone} and $self->{chg}->{manifest} = 1;
		delete @$m{grep(/$re/, @v1)} and $self->{chg}->{manifest} = 1;
		@v1 = grep(!/$re/, @v1);
	}
	my $ret; # { v1 => [ ... ], v2 => { "/$inbox_name" => [ epochs ] }}
	$ret->{v1} = \@v1 if @v1;
	$ret->{v2} = $v2 if keys %$v2;
	$ret //= @orig ? "Nothing to clone, available repositories:\n\t".
				join("\n\t", sort @orig)
			: "Nothing available to clone\n";
	my $path_pfx = '';

	# PSGI mount prefixes and manifest.js.gz prefixes don't always align...
	if (@v2_epochs) {
		until (grep(m!\A\Q$$path\E/git/[0-9]+\.git\z!,
				@v2_epochs) == @v2_epochs) {
			$$path =~ s!\A(/[^/]+)/!/! or last;
			$path_pfx .= $1;
		}
	} elsif (@v1) {
		while (!defined($m->{$$path}) && $$path =~ s!\A(/[^/]+)/!/!) {
			$path_pfx .= $1;
		}
	}
	($path_pfx, $n, $ret);
}

sub clone_all {
	my ($self, $m) = @_;
	my $todo = $TODO;
	$TODO = \'BUG on further use';
	my $end = PublicInbox::OnDestroy->new($$, \&fgrp_fetch_all, $self);
	{
		my $nodep = delete $todo->{''};

		# do not download unwanted deps
		my $any_want = delete $self->{any_want};
		my @unwanted = grep { !$any_want->{$_} } keys %$todo;
		my @nodep = delete(@$todo{@unwanted});
		push(@$nodep, @$_) for @nodep;

		# handle no-dependency repos, first
		for (@$nodep) {
			clone_v1($_, $end);
			return if !keep_going($self);
		}
	}
	# resolve references, deepest, first:
	while (scalar keys %$todo) {
		for my $x (keys %$todo) {
			my ($nr, $nxt);
			# resolve multi-level references
			while ($m && defined($nxt = $m->{$x}->{reference})) {
				exists($todo->{$nxt}) or last;
				if (++$nr > 1000) {
					$m->{$x}->{reference} = undef;
					$m->{$nxt}->{reference} = undef;
					warn <<EOM
E: dependency loop detected (`$x' => `$nxt'), breaking
EOM
				}
				$x = $nxt;
			}
			my $y = delete $todo->{$x} // next; # already done
			for (@$y) {
				clone_v1($_, $end);
				return if !keep_going($self);
			}
			last; # restart %$todo iteration
		}
	}

	# $end->DESTROY will call fgrp_fetch_all once all references
	# in $LIVE are gone, and do_reap will eventually drain $LIVE
	$end = undef;
	do_reap($self, 1);
}

sub dump_manifest ($$) {
	my ($m, $ft) = @_;
	# write the smaller manifest if epochs were skipped so
	# users won't have to delete manifest if they +w an
	# epoch they no longer want to skip
	my $json = PublicInbox::Config->json->encode($m);
	my $mtime = (stat($ft))[9];
	seek($ft, SEEK_SET, 0) or die "seek($ft): $!";
	truncate($ft, 0) or die "truncate($ft): $!";
	gzip(\$json => $ft) or die "gzip($ft): $GzipError";
	$ft->flush or die "flush($ft): $!";
	utime($mtime, $mtime, "$ft") or die "utime(..., $ft): $!";
}

sub dump_project_list ($$) {
	my ($self, $m) = @_;
	my $f = $self->{'-project-list'} // return;
	my $old = PublicInbox::Git::try_cat($f);
	my %new;

	open my $dh, '<', '.' or die "open(.): $!";
	chdir($self->{dst}) or die "chdir($self->{dst}): $!";
	my @local = grep { -e $_ ? ($new{$_} = undef) : 1 } split(/\n/s, $old);
	chdir($dh) or die "chdir(restore): $!";

	$new{substr($_, 1)} = 1 for keys %$m; # drop leading '/'
	my @list = sort keys %new;
	my @remote = grep { !defined($new{$_}) } @list;
	my %lnk = map { substr($_, 1) => undef } @{$self->{-new_symlinks}};
	@remote = grep { !exists($lnk{$_}) } @remote;

	warn <<EOM if @remote;
The following local repositories are ignored/gone from $self->{src}:
EOM
	warn "\t", $_, "\n" for @remote;
	warn <<EOM if @local;
The following repos in $f no longer exist on the filesystem:
EOM
	warn "\t", $_, "\n" for @local;

	my (undef, $dn, $bn) = File::Spec->splitpath($f);
	$self->{chg}->{nr_chg} += scalar(@remote) + scalar(@local);
	my $new = join("\n", @list, '');
	atomic_write($dn, $bn, $new) if $new ne $old;
}

# FIXME: this gets confused by single inbox instance w/ global manifest.js.gz
sub try_manifest {
	my ($self) = @_;
	my $uri = URI->new($self->{src});
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	$self->{-torsocks} //= $curl->torsocks($lei, $uri) or return;
	my $path = $uri->path;
	chop($path) eq '/' or die "BUG: $uri not canonicalized";
	$uri->path($path . '/manifest.js.gz');
	my $manifest = $self->{-manifest} // "$self->{dst}/manifest.js.gz";
	my %opt = (UNLINK => 1, SUFFIX => '.tmp', TMPDIR => 1);
	if (!$self->{dry_run} && $manifest =~ m!\A(.+?)/[^/]+\z! and -d $1) {
		$opt{DIR} = $1; # allows fast rename(2) w/o EXDEV
		delete $opt{TMPDIR};
	}
	my $ft = File::Temp->new(TEMPLATE => '.manifest-XXXX', %opt);
	my $cmd = $curl->for_uri($lei, $uri, qw(-R -o), $ft->filename);
	push(@$cmd, '-z', $manifest) if -f $manifest;
	my $mf_url = "$uri";
	%opt = map { $_ => $lei->{$_} } (0..2);
	my $cerr = run_reap($lei, $cmd, \%opt);
	if ($cerr) {
		return try_scrape($self) if ($cerr >> 8) == 22; # 404 missing
		return $lei->child_error($cerr, "@$cmd failed");
	}

	# bail out if curl -z/--timecond hit 304 Not Modified, $ft will be empty
	if (-f $manifest && !-s $ft) {
		$lei->child_error(127 << 8) if $lei->{opt}->{'exit-code'};
		return $lei->qerr("# $manifest unchanged");
	}

	my $m = eval { decode_manifest($ft, $ft, $uri) };
	if ($@) {
		warn $@;
		return try_scrape($self);
	}
	local $self->{chg} = {};
	local $self->{-local_manifest} = load_current_manifest($self);
	local $self->{-new_symlinks} = [];
	my ($path_pfx, $n, $multi) = multi_inbox($self, \$path, $m);
	return $lei->child_error(1, $multi) if !ref($multi);
	my $v2 = delete $multi->{v2};
	if ($v2) {
		for my $name (sort keys %$v2) {
			my $epochs = delete $v2->{$name};
			my %v2_epochs = map {
				$uri->path($n > 1 ? $path_pfx.$path.$_
						: $path_pfx.$_);
				my ($e) = ("$uri" =~ m!/([0-9]+)\.git\z!);
				$e // die "no [0-9]+\.git in `$uri'";
				$e => [ $uri->clone, $_ ];
			} @$epochs;
			("$uri" =~ m!\A(.+/)git/[0-9]+\.git\z!) or
				die "BUG: `$uri' !~ m!/git/[0-9]+.git!";
			local $self->{cur_src} = $1;
			local $self->{cur_dst} = $self->{dst};
			if ($n > 1 && $uri->path =~ m!\A\Q$path_pfx$path\E/(.+)/
							git/[0-9]+\.git\z!x) {
				$self->{cur_dst} .= "/$1";
			}
			index($self->{cur_dst}, "\n") >= 0 and die <<EOM;
E: `$self->{cur_dst}' must not contain newline
EOM
			clone_v2_prep($self, \%v2_epochs, $m);
			return if !keep_going($self);
		}
	}
	if (my $v1 = delete $multi->{v1}) {
		my $p = $path_pfx.$path;
		chop($p) if substr($p, -1, 1) eq '/';
		$uri->path($p);
		for my $name (@$v1) {
			my $task = bless { %$self }, __PACKAGE__;
			$task->{-ent} = $m->{$name} //
					die("BUG: no `$name' in manifest");
			$task->{cur_src} = "$uri";
			$task->{cur_dst} = $task->{dst};
			$task->{-key} = $name;
			if ($n > 1) {
				$task->{cur_dst} .= $name;
				$task->{cur_src} .= $name;
			}
			index($task->{cur_dst}, "\n") >= 0 and die <<EOM;
E: `$task->{cur_dst}' must not contain newline
EOM
			$task->{cur_src} .= '/';
			my $dep = $task->{-ent}->{reference} // '';
			push @{$TODO->{$dep}}, $task; # for clone_all
			$self->{any_want}->{$name} = 1;
		}
	}
	delete local $lei->{opt}->{epoch} if defined($v2);
	clone_all($self, $m);
	return if $self->{dry_run} || !keep_going($self);

	# set by clone_v2_prep/-I/--exclude
	my $mis = delete $self->{chg}->{fp_mismatch};
	if ($mis) {
		my $t = (stat($ft))[9];
		$t = strftime('%F %k:%M:%S %z', localtime($t));
		warn <<EOM;
W: Fingerprints for the following repositories do not match
W: $mf_url @ $t:
W: These repositories may have updated since $t:
EOM
		warn "\t", $_, "\n" for @$mis;
		warn <<EOM if !$self->{lei}->{opt}->{prune};
W: The above fingerprints may never match without --prune
EOM
	}
	dump_manifest($m => $ft) if delete($self->{chg}->{manifest}) || $mis;
	my $bad = delete $self->{chg}->{badlink};
	warn(<<EOM, map { ("\t", $_, "\n") } @$bad) if $bad;
W: The following exist and have not been converted to symlinks
EOM
	dump_project_list($self, $m);
	ft_rename($ft, $manifest, 0666);
	!$self->{chg}->{nr_chg} && $lei->{opt}->{'exit-code'} and
		$lei->child_error(127 << 8);
}

sub start_clone_url {
	my ($self) = @_;
	return try_manifest($self) if $self->{src} =~ m!\Ahttps?://!;
	die "TODO: non-HTTP/HTTPS clone of $self->{src} not supported, yet";
}

sub do_mirror { # via wq_io_do or public-inbox-clone
	my ($self) = @_;
	my $lei = $self->{lei};
	$self->{dry_run} = 1 if $lei->{opt}->{'dry-run'};
	umask($lei->{client_umask}) if defined $lei->{client_umask};
	$self->{-initial_clone} = 1 if !-d $self->{dst};
	local @PUH;
	if (defined(my $puh = $lei->{opt}->{'post-update-hook'})) {
		require Text::ParseWords;
		@PUH = map { [ Text::ParseWords::shellwords($_) ] } @$puh;
	}
	eval {
		my $ic = $lei->{opt}->{'inbox-config'} //= 'always';
		$ic =~ /\A(?:v1|v2|always|never)\z/s or die <<"";
--inbox-config must be one of `always', `v2', `v1', or `never'

		# we support these switches with '' (empty string).
		# defaults match example conf distributed with grokmirror
		my @pairs = qw(objstore objstore manifest manifest.js.gz
				project-list projects.list);
		while (@pairs) {
			my ($k, $default) = splice(@pairs, 0, 2);
			my $v = $lei->{opt}->{$k} // next;
			$v = $default if $v eq '';
			$v = "$self->{dst}/$v" if $v !~ m!\A\.{0,2}/!;
			$self->{"-$k"} = $v;
		}

		local $LIVE = {};
		local $TODO = {};
		local $FGRP_TODO = {};
		my $iv = $lei->{opt}->{'inbox-version'} //
			return start_clone_url($self);
		return clone_v1($self) if $iv == 1;
		die "bad --inbox-version=$iv\n" if $iv != 2;
		die <<EOM if $self->{src} !~ m!://!;
cloning local v2 inboxes not supported
EOM
		try_scrape($self, 1);
	};
	$lei->fail($@) if $@;
}

sub start {
	my ($cls, $lei, $src, $dst) = @_;
	my $self = bless { src => $src, dst => $dst }, $cls;
	$lei->request_umask;
	my ($op_c, $ops) = $lei->workers_start($self, 1);
	$lei->{wq1} = $self;
	$self->wq_io_do('do_mirror', []);
	$self->wq_close;
	$lei->wait_wq_events($op_c, $ops);
}

sub ipc_atfork_child {
	my ($self) = @_;
	$self->{lei}->_lei_atfork_child;
	$self->SUPER::ipc_atfork_child;
}

sub write_makefile {
	my ($dir, $ibx_ver) = @_;
	my $f = "$dir/Makefile";
	if (sysopen my $fh, $f, O_CREAT|O_EXCL|O_WRONLY) {
		print $fh <<EOM or die "print($f) $!";
# This is a v$ibx_ver public-inbox, see the public-inbox-v$ibx_ver-format(5)
# manpage for more information on the format.  This Makefile is
# intended as a familiar wrapper for users unfamiliar with
# public-inbox-* commands.
#
# See the respective manpages for public-inbox-fetch(1),
# public-inbox-index(1), etc for more information on
# some of the commands used by this Makefile.
#
# This Makefile will not be modified nor read by public-inbox,
# so you may edit it freely with your own convenience targets
# and notes.  public-inbox-fetch will recreate it if removed.
EOM
		print $fh <<'EOM' or die "print($f): $!";
# the default target:
help :
	@echo Common targets:
	@echo '    make fetch        - fetch from remote git repostorie(s)'
	@echo '    make update       - fetch and update index '
	@echo
	@echo Rarely needed targets:
	@echo '    make reindex      - may be needed for new features/bugfixes'
	@echo '    make compact      - rewrite Xapian storage to save space'
	@echo '    make index        - initial index after clone

fetch :
	public-inbox-fetch
update :
	@if ! public-inbox-fetch --exit-code; \
	then \
		c=$$?; \
		test $$c -eq 127 && exit 0; \
		exit $$c; \
	elif test -f msgmap.sqlite3 || test -f public-inbox/msgmap.sqlite3; \
	then \
		public-inbox-index; \
	else \
		echo 'public-inbox index not initialized'; \
		echo 'see public-inbox-index(1) man page'; \
	fi
index :
	public-inbox-index
reindex :
	public-inbox-index --reindex
compact :
	public-inbox-compact

.PHONY : help fetch update index reindex compact
EOM
		close $fh or die "close($f): $!";
	} else {
		die "open($f): $!" unless $!{EEXIST};
	}
}

1;
