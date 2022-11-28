# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# "lei add-external --mirror" support (also "public-inbox-clone");
package PublicInbox::LeiMirror;
use strict;
use v5.10.1;
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
use PublicInbox::LeiCurl;
use PublicInbox::OnDestroy;

our $LIVE; # pid => callback

sub _wq_done_wait { # dwaitpid callback (via wq_eof)
	my ($arg, $pid) = @_;
	my ($mrr, $lei) = @$arg;
	my $f = "$mrr->{dst}/mirror.done";
	if ($?) {
		$lei->child_error($?);
	} elsif (!$mrr->{dry_run} && !unlink($f)) {
		warn("unlink($f): $!\n") unless $!{ENOENT};
	} else {
		if (!$mrr->{dry_run} && $lei->{cmd} ne 'public-inbox-clone') {
			# calls _finish_add_external
			$lei->lazy_cb('add-external', '_finish_'
					)->($lei, $mrr->{dst});
		}
		$lei->qerr("# mirrored $mrr->{src} => $mrr->{dst}");
	}
	$lei->dclose;
}

# for old installations without manifest.js.gz
sub try_scrape {
	my ($self) = @_;
	my $uri = URI->new($self->{src});
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $cmd = $curl->for_uri($lei, $uri, '--compressed');
	my $opt = { 0 => $lei->{0}, 2 => $lei->{2} };
	my $fh = popen_rd($cmd, undef, $opt);
	my $html = do { local $/; <$fh> } // die "read(curl $uri): $!";
	close($fh) or return $lei->child_error($?, "@$cmd failed");

	# we grep with URL below, we don't want Subject/From headers
	# making us clone random URLs
	my @html = split(/<hr>/, $html);
	my @urls = ($html[-1] =~ m!\bgit clone --mirror ([a-z\+]+://\S+)!g);
	my $url = $uri->as_string;
	chop($url) eq '/' or die "BUG: $uri not canonicalized";

	# since this is for old instances w/o manifest.js.gz, try v1 first
	return clone_v1($self) if grep(m!\A\Q$url\E/*\z!, @urls);
	if (my @v2_urls = grep(m!\A\Q$url\E/[0-9]+\z!, @urls)) {
		my %v2_epochs = map {
			my ($n) = (m!/([0-9]+)\z!);
			$n => [ URI->new($_), '' ]
		} @v2_urls; # uniq
		clone_v2($self, \%v2_epochs);
		reap_live() while keys(%$LIVE);
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
	# --reference is tricky with multiple epochs...
	@cmd;
}

sub ft_rename ($$$) {
	my ($ft, $dst, $open_mode) = @_;
	my $fn = $ft->filename;
	my @st = stat($dst);
	my $mode = @st ? ($st[2] & 07777) : ($open_mode & ~umask);
	chmod($mode, $ft) or croak "E: chmod $fn: $!";
	require File::Copy;
	File::Copy::mv($fn, $dst) or croak "E: mv($fn => $ft): $!";
	$ft->unlink_on_destroy(0);
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
	my $jobs = $lei->{opt}->{jobs} // 1;
	reap_live() while keys(%$LIVE) >= $jobs;
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
	open my $fh, '>', $f or die "open($f): $!";
	print $fh $buf or die "print: $!";
	chmod(0444 & ~umask, $fh) or die "chmod($f): $!";
	my $mtime = delete $self->{'mtime._/text/config/raw'};
	$fh->flush or die "flush($f): $!";
	if (defined $mtime) {
		utime($mtime, $mtime, $fh) or die "utime($f): $!";
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
	my $f = "$dst/description";
	open my $fh, '+>>', $f or die "open($f): $!";
	seek($fh, 0, SEEK_SET) or die "seek($f): $!";
	my $d = do { local $/; <$fh> } // die "read($f): $!";
	my $orig = $d;
	while (defined($d) && ($d =~ m!^\(\$INBOX_DIR/description missing\)! ||
			$d =~ /^Unnamed repository/ || $d !~ /\S/)) {
		$d = delete($self->{'txt.description'});
	}
	$d //= 'mirror of '.($self->{cur_src} // $self->{src})."\n";
	return if $d eq $orig;
	seek($fh, 0, SEEK_SET) or die "seek($f): $!";
	truncate($fh, 0) or die "truncate($f): $!";
	print $fh $d or die "print($f): $!";
	close $fh or die "close($f): $!";
}

sub index_cloned_inbox {
	my ($self, $iv) = @_;
	my $lei = $self->{lei};
	eval { set_description($self) };
	warn $@ if $@;

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
		# force synchronous dwaitpid for v2:
		local $PublicInbox::DS::in_loop = 0;
		my $cfg = PublicInbox::Config->new(undef, $lei->{2});
		my $env = PublicInbox::Admin::index_prepare($opt, $cfg);
		local %ENV = (%ENV, %$env) if $env;
		PublicInbox::Admin::progress_prepare($opt, $lei->{2});
		PublicInbox::Admin::index_inbox($ibx, undef, $opt);
	}
	return if defined $self->{cur_dst};
	open my $x, '>', "$self->{dst}/mirror.done"; # for _wq_done_wait
}

sub run_reap {
	my ($lei, $cmd, $opt) = @_;
	$lei->qerr("# @$cmd");
	waitpid(spawn($cmd, undef, $opt), 0) // die "waitpid: $!";
	my $ret = $?;
	$? = 0; # don't let it influence normal exit
	$ret;
}

sub start_clone {
	my ($self, $cmd, $opt, $fini) = @_;
	my $jobs = $self->{lei}->{opt}->{jobs} // 1;
	reap_live() while keys(%$LIVE) >= $jobs;
	$self->{lei}->qerr("# @$cmd");
	return if $self->{dry_run};
	$LIVE->{spawn($cmd, undef, $opt)} = [ \&reap_clone, $self, $cmd, $fini ]
}

sub clone_v1 {
	my ($self, $nohang) = @_;
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $uri = URI->new($self->{cur_src} // $self->{src});
	defined($lei->{opt}->{epoch}) and
		die "$uri is a v1 inbox, --epoch is not supported\n";
	my $pfx = $curl->torsocks($lei, $uri) or return;
	my $dst = $self->{cur_dst} // $self->{dst};
	my $fini = PublicInbox::OnDestroy->new($$, \&v1_done, $self);
	my $cmd = [ @$pfx, clone_cmd($lei, my $opt = {}), "$uri", $dst ];
	my $ref = $self->{-ent} ? $self->{-ent}->{reference} : undef;
	defined($ref) && -e "$self->{dst}$ref" and
		push @$cmd, '--reference', "$self->{dst}$ref";
	start_clone($self, $cmd, $opt, $fini);

	$lei->{opt}->{'inbox-config'} =~ /\A(?:always|v1)\z/s and
		_get_txt_start($self, '_/text/config/raw', $fini);
	my $d = $self->{-ent} ? $self->{-ent}->{description} : undef;
	defined($d) ? ($self->{'txt.description'} = $d) :
		_get_txt_start($self, 'description', $fini);

	reap_live() until ($nohang || !keys(%$LIVE)); # for non-manifest clone
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
	if (defined $ent->{head}) {
		$f = "$edst/HEAD";
		open $fh, '>', $f or die "open($f): $!";
		print $fh $ent->{head}, "\n" or die "print($f): $!";
		close $fh or die "close($f): $!";
	}
}

sub reap_clone { # async, called via SIGCHLD
	my ($self, $cmd) = @_;
	my $cerr = $?;
	$? = 0; # don't let it influence normal exit
	if ($cerr) {
		kill('TERM', keys %$LIVE);
		$self->{lei}->child_error($cerr, "@$cmd failed");
	}
}

sub v1_done { # called via OnDestroy
	my ($self) = @_;
	return if $self->{dry_run} || !$LIVE;
	_write_inbox_config($self);
	my $dst = $self->{cur_dst} // $self->{dst};
	if (defined(my $o = $self->{-ent} ? $self->{-ent}->{owner} : undef)) {
		run_die([qw(git config -f), "$dst/config", 'gitweb.owner', $o]);
	}
	my $o = "$dst/objects";
	if (open(my $fh, '<', "$o/info/alternates")) {
		chomp(my @l = <$fh>);
		for (@l) { $_ = File::Spec->abs2rel($_, $o)."\n" }
		my $f = File::Temp->new(TEMPLATE => '.XXXX', DIR => "$o/info");
		print $f @l;
		$f->flush or die "flush($f): $!";
		rename($f->filename, "$o/info/alternates") or
			die "rename($f, $o/info/alternates): $!";
		$f->unlink_on_destroy(0);
	}
	write_makefile($dst, 1);
	index_cloned_inbox($self, 1);
}

sub v2_done { # called via OnDestroy
	my ($self) = @_;
	return if $self->{dry_run} || !$LIVE;
	_write_inbox_config($self);
	require PublicInbox::MultiGit;
	my $dst = $self->{cur_dst} // $self->{dst};
	my $mg = PublicInbox::MultiGit->new($dst, 'all.git', 'git');
	$mg->fill_alternates;
	for my $i ($mg->git_epochs) { $mg->epoch_cfg_set($i) }
	my $edst_owner = delete($self->{-owner}) // [];
	while (@$edst_owner) {
		my ($edst, $o) = splice(@$edst_owner);
		run_die [qw(git config -f), "$edst/config", 'gitweb.owner', $o];
	}
	for my $edst (@{delete($self->{-read_only}) // []}) {
		my @st = stat($edst) or die "stat($edst): $!";
		chmod($st[2] & 0555, $edst) or die "chmod(a-w, $edst): $!";
	}
	write_makefile($dst, 2);
	delete $self->{-locked} // die "BUG: $dst not locked"; # unlock
	index_cloned_inbox($self, 2);
}

sub reap_live {
	my $pid = waitpid(-1, 0) // die "waitpid(-1): $!";
	if (my $x = delete $LIVE->{$pid}) {
		my $cb = shift @$x;
		$cb->(@$x);
	} else {
		warn "reaped unknown PID=$pid ($?)\n";
	}
}

sub clone_v2 ($$;$) {
	my ($self, $v2_epochs, $m) = @_; # $m => manifest.js.gz hashref
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $first_uri = (map { $_->[0] } values %$v2_epochs)[0];
	my $pfx = $curl->torsocks($lei, $first_uri) or return;
	my $dst = $self->{cur_dst} // $self->{dst};
	my $want = parse_epochs($lei->{opt}->{epoch}, $v2_epochs);
	my $task = $m ? bless { %$self }, __PACKAGE__ : $self;
	my (@src_edst, @skip);
	for my $nr (sort { $a <=> $b } keys %$v2_epochs) {
		my ($uri, $key) = @{$v2_epochs->{$nr}};
		my $src = $uri->as_string;
		my $edst = $dst;
		$src =~ m!/([0-9]+)(?:\.git)?\z! or die <<"";
failed to extract epoch number from $src

		$1 + 0 == $nr or die "BUG: <$uri> miskeyed $1 != $nr";
		$edst .= "/git/$nr.git";
		$m->{$key} // die "BUG: `$key' not in manifest.js.gz";
		if (!$want || $want->{$nr}) {
			push @src_edst, $src, $edst;
			my $o = $m->{$key}->{owner};
			push(@{$task->{-owner}}, $edst, $o) if defined($o);
		} else { # create a placeholder so users only need to chmod +w
			init_placeholder($src, $edst, $m->{$key});
			push @{$task->{-read_only}}, $edst;
			push @skip, $key;
		}
	}
	# filter out the epochs we skipped
	$self->{-culled_manifest} = 1 if delete(@$m{@skip});

	(!$self->{dry_run} && !-d $dst) and File::Path::mkpath($dst);

	require PublicInbox::Lock;
	my $lk = bless { lock_path => "$dst/inbox.lock" }, 'PublicInbox::Lock';
	my $fini = PublicInbox::OnDestroy->new($$, \&v2_done, $task);

	$lei->{opt}->{'inbox-config'} =~ /\A(?:always|v2)\z/s and
		_get_txt_start($task, '_/text/config/raw', $fini);

	_get_txt_start($self, 'description', $fini);

	$task->{-locked} = $lk->lock_for_scope($$) if !$self->{dry_run};
	my @cmd = clone_cmd($lei, my $opt = {});
	while (@src_edst && !$lei->{child_error}) {
		my $cmd = [ @$pfx, @cmd, splice(@src_edst, 0, 2) ];
		start_clone($self, $cmd, $opt, $fini);
	}
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
		my $re = '(?:'.join('|', map {
				$self->{lei}->glob2re($_) // qr/\A\Q$_\E\z/
			} @$incl).')';
		my @gone = delete @$v2{grep(!/$re/, keys %$v2)};
		delete @$m{map { @$_ } @gone} and $self->{-culled_manifest} = 1;
		delete @$m{grep(!/$re/, @v1)} and $self->{-culled_manifest} = 1;
		@v1 = grep(/$re/, @v1);
	}
	if (defined $excl) {
		my $re = '(?:'.join('|', map {
				$self->{lei}->glob2re($_) // qr/\A\Q$_\E\z/
			} @$excl).')';
		my @gone = delete @$v2{grep(/$re/, keys %$v2)};
		delete @$m{map { @$_ } @gone} and $self->{-culled_manifest} = 1;
		delete @$m{grep(/$re/, @v1)} and $self->{-culled_manifest} = 1;
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
	my ($self, $todo, $m) = @_;
	# handle no-dependency repos, first
	for (@{delete($todo->{''}) // []}) {
		clone_v1($_, 1);
		return if $self->{lei}->{child_error};
	}
	# resolve references, deepest, first:
	while (scalar keys %$todo) {
		for my $x (keys %$todo) {
			# resolve multi-level references
			while (defined($m->{$x}->{reference})) {
				$x = $m->{$x}->{reference};
			}
			my $y = delete $todo->{$x} // next; # already done
			for (@$y) {
				clone_v1($_, 1);
				return if $self->{lei}->{child_error};
			}
			last; # restart %$todo iteration
		}
	}
}

# FIXME: this gets confused by single inbox instance w/ global manifest.js.gz
sub try_manifest {
	my ($self) = @_;
	my $uri = URI->new($self->{src});
	my $lei = $self->{lei};
	my $curl = $self->{curl} //= PublicInbox::LeiCurl->new($lei) or return;
	my $path = $uri->path;
	chop($path) eq '/' or die "BUG: $uri not canonicalized";
	$uri->path($path . '/manifest.js.gz');
	my $ft = File::Temp->new(TEMPLATE => '.manifest-XXXX',
				UNLINK => 1, TMPDIR => 1, SUFFIX => '.tmp');
	my $fn = $ft->filename;
	my $cmd = $curl->for_uri($lei, $uri, '-R', '-o', $fn);
	my %opt = map { $_ => $lei->{$_} } (0..2);
	my $cerr = run_reap($lei, $cmd, \%opt);
	local $LIVE;
	if ($cerr) {
		return try_scrape($self) if ($cerr >> 8) == 22; # 404 missing
		return $lei->child_error($cerr, "@$cmd failed");
	}
	my $m = eval { decode_manifest($ft, $fn, $uri) };
	if ($@) {
		warn $@;
		return try_scrape($self);
	}
	my ($path_pfx, $n, $multi) = multi_inbox($self, \$path, $m);
	return $lei->child_error(1, $multi) if !ref($multi);
	if (my $v2 = delete $multi->{v2}) {
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
			clone_v2($self, \%v2_epochs, $m);
			return if $self->{lei}->{child_error};
		}
	}
	if (my $v1 = delete $multi->{v1}) {
		my $p = $path_pfx.$path;
		chop($p) if substr($p, -1, 1) eq '/';
		$uri->path($p);
		my $todo = {};
		my %want = map { $_ => 1 } @$v1;
		for my $name (@$v1) {
			my $task = bless { %$self }, __PACKAGE__;
			$task->{-ent} = $m->{$name} //
					die("BUG: no `$name' in manifest");
			$task->{cur_src} = "$uri";
			$task->{cur_dst} = $task->{dst};
			if ($n > 1) {
				$task->{cur_dst} .= $name;
				$task->{cur_src} .= $name;
			}
			index($task->{cur_dst}, "\n") >= 0 and die <<EOM;
E: `$task->{cur_dst}' must not contain newline
EOM
			$task->{cur_src} .= '/';
			my $dep = $task->{-ent}->{reference} // '';
			$dep = '' if !$want{$dep};
			push @{$todo->{$dep}}, $task;
		}
		clone_all($self, $todo, $m);
	}
	reap_live() while keys(%$LIVE);
	return if $self->{lei}->{child_error} || $self->{dry_run};

	if (delete $self->{-culled_manifest}) { # set by clone_v2/-I/--exclude
		# write the smaller manifest if epochs were skipped so
		# users won't have to delete manifest if they +w an
		# epoch they no longer want to skip
		my $json = PublicInbox::Config->json->encode($m);
		my $mtime = (stat($fn))[9];
		gzip(\$json => $fn) or die "gzip: $GzipError";
		utime($mtime, $mtime, $fn) or die "utime(..., $fn): $!";
	}
	ft_rename($ft, "$self->{dst}/manifest.js.gz", 0666);
	open my $x, '>', "$self->{dst}/mirror.done"; # for _wq_done_wait
}

sub start_clone_url {
	my ($self) = @_;
	return try_manifest($self) if $self->{src} =~ m!\Ahttps?://!;
	die "TODO: non-HTTP/HTTPS clone of $self->{src} not supported, yet";
}

sub do_mirror { # via wq_io_do or public-inbox-clone
	my ($self) = @_;
	my $lei = $self->{lei};
	umask($lei->{client_umask}) if defined $lei->{client_umask};
	eval {
		my $ic = $lei->{opt}->{'inbox-config'} //= 'always';
		$ic =~ /\A(?:v1|v2|always|never)\z/s or die <<"";
--inbox-config must be one of `always', `v2', `v1', or `never'

		my $iv = $lei->{opt}->{'inbox-version'};
		if (defined $iv) {
			local $LIVE;
			return clone_v1($self) if $iv == 1;
			return try_scrape($self) if $iv == 2;
			die "bad --inbox-version=$iv\n";
		}
		return start_clone_url($self) if $self->{src} =~ m!://!;
		die "TODO: cloning local directories not supported, yet";
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
reindex :
	public-inbox-index --reindex
compact :
	public-inbox-compact

.PHONY : help fetch update reindex compact
EOM
		close $fh or die "close($f): $!";
	} else {
		die "open($f): $!" unless $!{EEXIST};
	}
}

1;
