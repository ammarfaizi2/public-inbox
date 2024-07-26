# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# ref: https://cr.yp.to/proto/maildir.html
#	https://wiki2.dovecot.org/MailboxFormat/Maildir
package PublicInbox::Watch;
use strict;
use v5.10.1;
use PublicInbox::Eml;
use PublicInbox::InboxWritable qw(eml_from_path);
use PublicInbox::MdirReader;
use PublicInbox::NetReader;
use PublicInbox::Filter::Base qw(REJECT);
use PublicInbox::Spamcheck;
use PublicInbox::DS qw(now add_timer awaitpid);
use PublicInbox::MID qw(mids);
use PublicInbox::ContentHash qw(content_hash);
use POSIX qw(_exit WNOHANG);
use constant { D_MAILDIR => 1, D_MH => 2 };

sub compile_watchheaders ($) {
	my ($ibx) = @_;
	my $watch_hdrs = [];
	if (my $whs = $ibx->{watchheader}) {
		for (@$whs) {
			my ($k, $v) = split(/:/, $_, 2);
			# XXX should this be case-insensitive?
			# Or, mutt-style, case-sensitive iff
			# a capital letter exists?
			push @$watch_hdrs, [ $k, qr/\Q$v\E/ ];
		}
	}
	if (my $list_ids = $ibx->{listid}) {
		for (@$list_ids) {
			# RFC2919 section 6 stipulates
			# "case insensitive equality"
			my $re = qr/<[ \t]*\Q$_\E[ \t]*>/i;
			push @$watch_hdrs, ['List-Id', $re ];
		}
	}
	$ibx->{-watchheaders} = $watch_hdrs if scalar @$watch_hdrs;
}

sub d_type_set ($$$) {
	my ($d_type, $dir, $is) = @_;
	my $isnt = D_MAILDIR;
	if ($is == D_MAILDIR) {
		$isnt = D_MH;
		$d_type->{"$dir/cur"} |= $is;
		$d_type->{"$dir/new"} |= $is;
	}
	warn <<EOM if ($d_type->{$dir} |= $is) & $isnt;
W: `$dir' is both Maildir and MH (non-fatal)
EOM
}

sub new {
	my ($class, $cfg) = @_;
	my (%d_map, %d_type);
	my (%imap, %nntp); # url => [inbox objects] or 'watchspam'
	my (@imap, @nntp);
	PublicInbox::Import::load_config($cfg);

	# "publicinboxwatch" is the documented namespace
	# "publicinboxlearn" is legacy but may be supported
	# indefinitely...
	foreach my $pfx (qw(publicinboxwatch publicinboxlearn)) {
		my $k = "$pfx.watchspam";
		my $dirs = $cfg->get_all($k) // next;
		for my $dir (@$dirs) {
			my $uri;
			if (is_maildir($dir)) {
				# skip "new", no MUA has seen it, yet.
				$d_map{"$dir/cur"} = 'watchspam';
				d_type_set \%d_type, $dir, D_MAILDIR;
			} elsif (is_mh($dir)) {
				$d_map{$dir} = 'watchspam';
				d_type_set \%d_type, $dir, D_MH;
			} elsif ($uri = imap_uri($dir)) {
				$imap{$$uri} = 'watchspam';
				push @imap, $uri;
			} elsif ($uri = nntp_uri($dir)) {
				$nntp{$$uri} = 'watchspam';
				push @nntp, $uri;
			} else {
				warn "unsupported $k=$dir\n";
			}
		}
	}
	my $k = 'publicinboxwatch.spamcheck';
	my $default = undef;
	my $spamcheck = PublicInbox::Spamcheck::get($cfg, $k, $default);
	$spamcheck = _spamcheck_cb($spamcheck) if $spamcheck;

	$cfg->each_inbox(sub {
		# need to make all inboxes writable for spam removal:
		my $ibx = $_[0] = PublicInbox::InboxWritable->new($_[0]);

		my $watches = $ibx->{watch} or return;

		$ibx->{indexlevel} //= $ibx->detect_indexlevel;
		$watches = PublicInbox::Config::_array($watches);
		for my $watch (@$watches) {
			my $uri;
			my $bool = $cfg->git_bool($watch);
			if (defined $bool && !$bool) {
				$ibx->{-watch_disabled} = 1;
			} elsif (is_maildir($watch)) {
				compile_watchheaders($ibx);
				my ($new, $cur) = ("$watch/new", "$watch/cur");
				my $cur_dst = $d_map{$cur} //= [];
				return if is_watchspam($cur, $cur_dst, $ibx);
				push @{$d_map{$new} //= []}, $ibx;
				push @$cur_dst, $ibx;
				d_type_set \%d_type, $watch, D_MAILDIR;
			} elsif (is_mh($watch)) {
				my $cur_dst = $d_map{$watch} //= [];
				return if is_watchspam($watch, $cur_dst, $ibx);
				compile_watchheaders($ibx);
				push(@$cur_dst, $ibx);
				d_type_set \%d_type, $watch, D_MH;
			} elsif ($uri = imap_uri($watch)) {
				my $cur_dst = $imap{$$uri} //= [];
				return if is_watchspam($uri, $cur_dst, $ibx);
				compile_watchheaders($ibx);
				push(@imap, $uri) if 1 == push(@$cur_dst, $ibx);
			} elsif ($uri = nntp_uri($watch)) {
				my $cur_dst = $nntp{$$uri} //= [];
				return if is_watchspam($uri, $cur_dst, $ibx);
				compile_watchheaders($ibx);
				push(@nntp, $uri) if 1 == push(@$cur_dst, $ibx);
			} else {
				warn "watch unsupported: $k=$watch\n";
			}
		}
	});

	my $d_re;
	if (scalar keys %d_map) {
		$d_re = join('|', map quotemeta, keys %d_map);
		$d_re = qr!\A($d_re)/!;
	}
	return unless $d_re || scalar(keys %imap) || scalar(keys %nntp);

	bless {
		max_batch => 10, # avoid hogging locks for too long
		spamcheck => $spamcheck,
		d_map => \%d_map,
		d_re => $d_re,
		d_type => \%d_type,
		pi_cfg => $cfg,
		imap => scalar keys %imap ? \%imap : undef,
		nntp => scalar keys %nntp? \%nntp : undef,
		imap_order => scalar(@imap) ? \@imap : undef,
		nntp_order => scalar(@nntp) ? \@nntp: undef,
		importers => {},
		opendirs => {}, # dirname => dirhandle (in progress scans)
		ops => [], # 'quit', 'full'
	}, $class;
}

sub _done_for_now {
	my ($self) = @_;
	local $PublicInbox::DS::in_loop = 0; # waitpid() synchronously
	for my $im (values %{$self->{importers}}) {
		next if !$im; # $im may be undef during cleanup
		eval { $im->done };
		warn "$im->{ibx}->{name} ->done: $@\n" if $@;
	}
}

sub remove_eml_i { # each_inbox callback
	my ($ibx, $self, $eml, $loc) = @_;
	return if $ibx->{-watch_disabled};

	eval {
		# try to avoid taking a lock or unnecessary spawning
		my $im = $self->{importers}->{"$ibx"};
		my $scrubbed;
		if ((!$im || !$im->active) && $ibx->over) {
			if (content_exists($ibx, $eml)) {
				# continue
			} elsif (my $scrub = $ibx->filter($im)) {
				$scrubbed = $scrub->scrub($eml, 1);
				if ($scrubbed && $scrubbed != REJECT &&
					  !content_exists($ibx, $scrubbed)) {
					return;
				}
			} else {
				return;
			}
		}

		$im //= _importer_for($self, $ibx); # may spawn fast-import
		$im->remove($eml, 'spam');
		$scrubbed //= do {
			my $scrub = $ibx->filter($im);
			$scrub ? $scrub->scrub($eml, 1) : undef;
		};
		if ($scrubbed && $scrubbed != REJECT) {
			$im->remove($scrubbed, 'spam');
		}
	};
	if ($@) {
		warn "error removing spam at: $loc from $ibx->{name}: $@\n";
		_done_for_now($self);
	}
}

sub _remove_spam {
	my ($self, $path) = @_;
	# path must be marked as (S)een
	$path =~ /:2,[A-R]*S[T-Za-z]*\z/ or return;
	my $eml = eml_from_path($path) or return;
	local $SIG{__WARN__} = PublicInbox::Eml::warn_ignore_cb();
	$self->{pi_cfg}->each_inbox(\&remove_eml_i, $self, $eml, $path);
}

sub import_eml ($$$) {
	my ($self, $ibx, $eml) = @_;

	# any header match means it's eligible for the inbox:
	if (my $watch_hdrs = $ibx->{-watchheaders}) {
		my $ok;
		for my $wh (@$watch_hdrs) {
			my @v = $eml->header_raw($wh->[0]);
			$ok = grep(/$wh->[1]/, @v) and last;
		}
		return unless $ok;
	}
	eval {
		my $im = _importer_for($self, $ibx);
		if (my $scrub = $ibx->filter($im)) {
			my $scrubbed = $scrub->scrub($eml) or return;
			$scrubbed == REJECT and return;
			$eml = $scrubbed;
		}
		$im->add($eml, $self->{spamcheck});
	};
	if ($@) {
		warn "$ibx->{name} add failed: $@\n";
		_done_for_now($self);
	}
}

sub _try_path {
	my ($self, $path) = @_;
	$path =~ $self->{d_re} or
		return warn("BUG? unrecognized path: $path\n");
	my $dir = $1;
	my $inboxes = $self->{d_map}->{$dir} //
		return warn("W: unmappable dir: $dir\n");
	my ($md_fl, $mh_seq);
	if ($self->{d_type}->{$dir} & D_MH) {
		$path =~ m!/([0-9]+)\z! ? ($mh_seq = $1) : return;
	}
	$self->{d_type}->{$dir} & D_MAILDIR and
		$md_fl = PublicInbox::MdirReader::maildir_path_flags($path);
	$md_fl // $mh_seq // return;
	return if ($md_fl // '') =~ /[DT]/; # no Drafts or Trash
	# n.b. none of the MH keywords are relevant for public mail,
	# mh_seq is only used to validate we're reading an email
	# and not treating .mh_sequences as an email

	my $warn_cb = $SIG{__WARN__} || \&CORE::warn;
	local $SIG{__WARN__} = sub {
		my $pfx = ($_[0] // '') =~ /^([A-Z]: )/g ? $1 : '';
		$warn_cb->($pfx, "path: $path\n", @_);
	};
	if (!ref($inboxes) && $inboxes eq 'watchspam') {
		return _remove_spam($self, $path);
	}
	foreach my $ibx (@$inboxes) {
		my $eml = eml_from_path($path) or next;
		import_eml($self, $ibx, $eml);
	}
}

sub quit_done ($) {
	my ($self) = @_;
	return unless $self->{quit};

	# don't have reliable wakeups, keep signalling
	my $live = grep { kill('QUIT', $_) } keys %{$self->{pids}};
	add_timer(0.01, \&quit_done, $self) if $live;
	$live == 0;
}

sub quit { # may be called in IMAP/NNTP children
	my ($self) = @_;
	$self->{quit} = 1;
	%{$self->{opendirs}} = ();
	_done_for_now($self);
	quit_done($self);
	if (my $dir_idle = delete $self->{dir_idle}) {
		$dir_idle->close if $dir_idle;
	}
	if (my $idle_mic = delete $self->{idle_mic}) { # IMAP child
		return unless $idle_mic->IsConnected && $idle_mic->Socket;
		eval { $idle_mic->done };
		if ($@ && $idle_mic->IsConnected && $idle_mic->Socket) {
			warn "IDLE DONE error: $@\n";
			eval { $idle_mic->disconnect };
			warn "IDLE LOGOUT error: $@\n" if $@;
		}
	}
}

sub watch_fs_init ($) {
	my ($self) = @_;
	my $done = sub {
		delete $self->{done_timer};
		_done_for_now($self);
	};
	my $cb = sub { # called by PublicInbox::DirIdle::event_step
		_try_path($self, $_[0]->fullname);
		$self->{done_timer} //= PublicInbox::DS::requeue($done);
	};
	require PublicInbox::DirIdle;
	# inotify_create + EPOLL_CTL_ADD
	my $dir_idle = $self->{dir_idle} = PublicInbox::DirIdle->new($cb);
	$dir_idle->add_watches([keys %{$self->{d_map}}]);
}

sub net_cb { # NetReader::(nntp|imap)_each callback
	my ($uri, $art, $kw, $eml, $self, $inboxes) = @_;
	return if grep(/\Adraft\z/, @$kw);
	local $self->{cur_uid} = $art; # IMAP UID or NNTP article
	if (ref($inboxes)) {
		my @ibx = @$inboxes;
		my $last = pop @ibx;
		for my $ibx (@ibx) {
			my $tmp = PublicInbox::Eml->new(\($eml->as_string));
			import_eml($self, $ibx, $tmp);
		}
		import_eml($self, $last, $eml);
	} elsif ($inboxes eq 'watchspam') {
		if ($uri->scheme =~ /\Aimaps?\z/ && !grep(/\Aseen\z/, @$kw)) {
			return;
		}
		$self->{pi_cfg}->each_inbox(\&remove_eml_i,
				$self, $eml, "$uri #$art");
	} else {
		die "BUG: destination unknown $inboxes";
	}
}

sub imap_fetch_all ($$) {
	my ($self, $uri) = @_;
	my $warn_cb = $SIG{__WARN__} || \&CORE::warn;
	$self->{incremental} = 1;
	$self->{on_commit} = [ \&_done_for_now, $self ];
	local $self->{cur_uid};
	local $SIG{__WARN__} = sub {
		my $pfx = ($_[0] // '') =~ /^([A-Z]: |# )/g ? $1 : '';
		my $uid = $self->{cur_uid};
		$warn_cb->("$pfx$uri", $uid ? (" UID:$uid") : (), "\n", @_);
	};
	PublicInbox::NetReader::imap_each($self, $uri, \&net_cb, $self,
					$self->{imap}->{$$uri});
}

sub imap_idle_once ($$$$) {
	my ($self, $mic, $intvl, $uri) = @_;
	my $i = $intvl //= (29 * 60);
	my $end = now() + $intvl;
	warn "# $uri idling for ${intvl}s\n";
	local $0 = "IDLE $0";
	return if $self->{quit};
	unless ($mic->idle) {
		return if $self->{quit};
		return "E: IDLE failed on $uri: $!";
	}
	$self->{idle_mic} = $mic; # for ->quit
	my @res;
	until ($self->{quit} || !$mic->IsConnected ||
			grep(/^\* [0-9]+ EXISTS/, @res) || $i <= 0) {
		@res = $mic->idle_data($i);
		$i = $end - now();
	}
	delete $self->{idle_mic};
	unless ($self->{quit}) {
		$mic->IsConnected or return "E: IDLE disconnected on $uri";
		$mic->done or return "E: IDLE DONE failed on $uri: $!";
	}
	undef;
}

# idles on a single URI
sub watch_imap_idle_1 ($$$) {
	my ($self, $uri, $intvl) = @_;
	my $sec = uri_section($uri);
	my $mic_arg = $self->{net_arg}->{$sec} or
			die "BUG: no Mail::IMAPClient->new arg for $sec";
	my $mic;
	local $0 = $uri->mailbox." $sec";
	until ($self->{quit}) {
		$mic //= PublicInbox::NetReader::mic_new(
					$self, $mic_arg, $sec, $uri);
		my $err;
		if ($mic && $mic->IsConnected) {
			local $self->{mics_cached}->{$sec} = $mic;
			my $m = imap_fetch_all($self, $uri);
			$m == $mic or die "BUG: wrong mic";
			$mic->IsConnected and
				$err = imap_idle_once($self, $mic, $intvl, $uri)
		} else {
			$err = "E: not connected: $!";
		}
		if ($err && !$self->{quit}) {
			warn $err, "\n";
			$mic = undef;
			sleep 60 unless $self->{quit};
		}
	}
}

sub watch_atfork_child ($) {
	my ($self) = @_;
	delete @$self{qw(dir_idle pids opendirs)};
	my $sig = delete $self->{sig};
	$sig->{CHLD} = $sig->{HUP} = $sig->{USR1} = 'DEFAULT';
	# TERM/QUIT/INT call ->quit, which works in both parent+child
	@SIG{keys %$sig} = values %$sig;
	PublicInbox::DS::sig_setmask(PublicInbox::DS::allowset($sig));
}

sub watch_atfork_parent ($) { _done_for_now($_[0]) }

sub imap_idle_requeue { # DS::add_timer callback
	my ($self, $uri, $intvl) = @_;
	return if $self->{quit};
	push @{$self->{idle_todo}}, $uri, $intvl;
	event_step($self);
}

sub imap_idle_reap { # awaitpid callback
	my ($pid, $self, $uri, $intvl) = @_;
	delete $self->{pids}->{$pid};
	return if $self->{quit};
	warn "W: PID=$pid on $uri died: \$?=$?\n" if $?;
	add_timer(60, \&imap_idle_requeue, $self, $uri, $intvl);
}

sub imap_idle_fork {
	my ($self, $uri, $intvl) = @_;
	return if $self->{quit};
	my $pid = PublicInbox::DS::fork_persist;
	if ($pid == 0) {
		watch_atfork_child($self);
		watch_imap_idle_1($self, $uri, $intvl);
		_exit(0);
	}
	$self->{pids}->{$pid} = undef;
	awaitpid($pid, \&imap_idle_reap, $self, $uri, $intvl);
}

sub event_step {
	my ($self) = @_;
	return if $self->{quit};
	my $idle_todo = $self->{idle_todo};
	if ($idle_todo && @$idle_todo) {
		watch_atfork_parent($self);
		eval {
			while (my ($uri, $intvl) = splice(@$idle_todo, 0, 2)) {
				imap_idle_fork($self, $uri, $intvl);
			}
		};
		die $@ if $@;
	}
	fs_scan_step($self) if $self->{d_re};
}

sub watch_imap_fetch_all ($$) {
	my ($self, $uris) = @_;
	for my $uri (@$uris) {
		imap_fetch_all($self, $uri);
		last if $self->{quit};
	}
}

sub watch_nntp_fetch_all ($$) {
	my ($self, $uris) = @_;
	$self->{incremental} = 1;
	$self->{on_commit} = [ \&_done_for_now, $self ];
	my $warn_cb = $SIG{__WARN__} || \&CORE::warn;
	local $self->{cur_uid};
	my $uri = '';
	local $SIG{__WARN__} = sub {
		my $pfx = ($_[0] // '') =~ /^([A-Z]: |# )/g ? $1 : '';
		my $art = $self->{cur_uid};
		$warn_cb->("$pfx$uri", $art ? (" ARTICLE $art") : (), "\n", @_);
	};
	for $uri (@$uris) {
		PublicInbox::NetReader::nntp_each($self, $uri, \&net_cb, $self,
					$self->{nntp}->{$$uri});
		last if $self->{quit};
	}
}

sub poll_fetch_fork { # DS::add_timer callback
	my ($self, $intvl, $uris) = @_;
	return if $self->{quit};
	watch_atfork_parent($self);
	my @nntp;
	my @imap = grep { # push() always returns > 0
		$_->scheme =~ m!\Aimaps?!i ? 1 : (push(@nntp, $_) < 0)
	} @$uris;
	my $pid = PublicInbox::DS::fork_persist;
	if ($pid == 0) {
		watch_atfork_child($self);
		watch_imap_fetch_all($self, \@imap) if @imap;
		watch_nntp_fetch_all($self, \@nntp) if @nntp;
		_exit(0);
	}
	$self->{pids}->{$pid} = undef;
	awaitpid($pid, \&poll_fetch_reap, $self, $intvl, $uris);
}

sub poll_fetch_reap { # awaitpid callback
	my ($pid, $self, $intvl, $uris) = @_;
	delete $self->{pids}->{$pid};
	return if $self->{quit};
	if ($?) {
		warn "W: PID=$pid died: \$?=$?\n", map { "$_\n" } @$uris;
	}
	warn("# will check $_ in ${intvl}s\n") for @$uris;
	add_timer($intvl, \&poll_fetch_fork, $self, $intvl, $uris);
}

sub watch_imap_init ($$) {
	my ($self, $poll) = @_;
	my $mics = PublicInbox::NetReader::imap_common_init($self) or return;
	my $idle = []; # [ uri1, intvl1, uri2, intvl2 ]
	for my $uri (@{$self->{imap_order}}) {
		my $sec = uri_section($uri);
		my $mic = $mics->{$sec};
		my $intvl = $self->{cfg_opt}->{$sec}->{pollInterval};
		if ($mic->has_capability('IDLE') && !$intvl) {
			$intvl = $self->{cfg_opt}->{$sec}->{idleInterval};
			push @$idle, $uri, $intvl;
		} else {
			push @{$poll->{$intvl || 120}}, $uri;
		}
	}
	if (scalar @$idle) {
		$self->{idle_todo} = $idle;
		PublicInbox::DS::requeue($self); # ->event_step to fork
	}
}

sub watch_nntp_init ($$) {
	my ($self, $poll) = @_;
	PublicInbox::NetReader::nntp_common_init($self);
	for my $uri (@{$self->{nntp_order}}) {
		my $sec = uri_section($uri);
		my $intvl = $self->{cfg_opt}->{$sec}->{pollInterval};
		push @{$poll->{$intvl || 120}}, $uri;
	}
}

sub quit_inprogress { !$_[0]->quit_done } # post_loop_do CB

sub watch { # main entry point
	my ($self, $sig) = @_;
	my $first_sig;
	$self->{sig} //= ($first_sig = $sig);
	my $poll = {}; # intvl_seconds => [ uri1, uri2 ]
	watch_imap_init($self, $poll) if $self->{imap};
	watch_nntp_init($self, $poll) if $self->{nntp};
	while (my ($intvl, $uris) = each %$poll) {
		# poll all URIs for a given interval sequentially
		add_timer(0, \&poll_fetch_fork, $self, $intvl, $uris);
	}
	watch_fs_init($self) if $self->{d_re};
	local @PublicInbox::DS::post_loop_do = (\&quit_inprogress, $self);
	PublicInbox::DS::event_loop($first_sig); # calls ->event_step
	_done_for_now($self);
}

sub trigger_scan {
	my ($self, $op) = @_;
	push @{$self->{ops}}, $op;
	PublicInbox::DS::requeue($self);
}

sub fs_scan_step {
	my ($self) = @_;
	return if $self->{quit};
	my $op = shift @{$self->{ops}};
	local $PublicInbox::DS::in_loop = 0; # waitpid() synchronously

	# continue existing scan
	my $opendirs = $self->{opendirs};
	my @dirnames = keys %$opendirs;
	foreach my $dir (@dirnames) {
		my $dh = delete $opendirs->{$dir};
		my $n = $self->{max_batch};
		while (my $fn = readdir($dh)) {
			_try_path($self, "$dir/$fn");
			last if --$n < 0;
		}
		$opendirs->{$dir} = $dh if $n < 0;
	}
	if ($op && $op eq 'full') {
		foreach my $dir (keys %{$self->{d_map}}) {
			next if $opendirs->{$dir}; # already in progress
			my $ok = opendir(my $dh, $dir);
			unless ($ok) {
				warn "failed to open $dir: $!\n";
				next;
			}
			my $n = $self->{max_batch};
			while (my $fn = readdir($dh)) {
				_try_path($self, "$dir/$fn");
				last if --$n < 0;
			}
			$opendirs->{$dir} = $dh if $n < 0;
		}
	}
	_done_for_now($self);
	# do we have more work to do?
	keys(%$opendirs) ? PublicInbox::DS::requeue($self)
		: warn("# full scan complete\n");
}

sub scan {
	my ($self, $op) = @_;
	push @{$self->{ops}}, $op;
	fs_scan_step($self);
}

sub _importer_for {
	my ($self, $ibx) = @_;
	my $importers = $self->{importers};
	my $im = $importers->{"$ibx"} ||= $ibx->importer(0);
	if (scalar(keys(%$importers)) > 2) {
		delete $importers->{"$ibx"};
		_done_for_now($self);
	}

	$importers->{"$ibx"} = $im;
}

# XXX consider sharing with V2Writable, this only requires read-only access
sub content_exists ($$) {
	my ($ibx, $eml) = @_;
	my $over = $ibx->over or return;
	my $mids = mids($eml);
	my $chash = content_hash($eml);
	my ($id, $prev);
	for my $mid (@$mids) {
		while (my $smsg = $over->next_by_mid($mid, \$id, \$prev)) {
			my $cmp = $ibx->smsg_eml($smsg) or return;
			return 1 if $chash eq content_hash($cmp);
		}
	}
	undef;
}

sub _spamcheck_cb {
	my ($sc) = @_;
	sub { # this gets called by (V2Writable||Import)->add
		my ($mime, $ibx) = @_;
		return if content_exists($ibx, $mime);
		my $tmp = '';
		if ($sc->spamcheck($mime, \$tmp)) {
			return PublicInbox::Eml->new(\$tmp);
		}
		warn $mime->header('Message-ID')." failed spam check\n";
		undef;
	}
}

sub is_maildir {
	$_[0] =~ s!\Amaildir:!! or return;
	$_[0] =~ tr!/!/!s;
	$_[0] =~ s!/\z!!;
	$_[0];
}

sub is_mh {
	$_[0] =~ s!\Amh:!!i or return;
	$_[0] =~ tr!/!/!s;
	$_[0] =~ s!/\z!!;
	$_[0];
}

sub is_watchspam {
	my ($cur, $ws, $ibx) = @_;
	if ($ws && !ref($ws) && $ws eq 'watchspam') {
		warn <<EOF;
E: $cur is a spam folder and cannot be used for `$ibx->{name}' input
EOF
		return 1;
	}
	undef;
}

sub folder_select { 'select' } # for PublicInbox::NetReader

1;
