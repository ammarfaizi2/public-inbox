# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Writes PublicInbox::Eml objects atomically to a mbox variant or Maildir
package PublicInbox::LeiToMail;
use strict;
use v5.10.1;
use parent qw(PublicInbox::IPC);
use PublicInbox::Eml;
use PublicInbox::ProcessPipe;
use PublicInbox::Spawn qw(spawn);
use PublicInbox::PktOp qw(pkt_do);
use Symbol qw(gensym);
use IO::Handle; # ->autoflush
use Fcntl qw(SEEK_SET SEEK_END O_CREAT O_EXCL O_WRONLY);

my %kw2char = ( # Maildir characters
	draft => 'D',
	flagged => 'F',
	forwarded => 'P', # passed
	answered => 'R',
	seen => 'S',
);

my %kw2status = (
	flagged => [ 'X-Status' => 'F' ],
	answered => [ 'X-Status' => 'A' ],
	seen => [ 'Status' => 'R' ],
	draft => [ 'X-Status' => 'T' ],
);

sub _mbox_hdr_buf ($$$) {
	my ($eml, $type, $smsg) = @_;
	$eml->header_set($_) for (qw(Lines Bytes Content-Length));

	my %hdr = (Status => []); # set Status, X-Status
	for my $k (@{$smsg->{kw} // []}) {
		if (my $ent = $kw2status{$k}) {
			push @{$hdr{$ent->[0]}}, $ent->[1];
		} else { # X-Label?
			warn "TODO: keyword `$k' not supported for mbox\n";
		}
	}
	# Messages are always 'O' (non-\Recent in IMAP), it saves
	# MUAs the trouble of rewriting the mbox if no other
	# changes are made.  We put 'O' at the end (e.g. "Status: RO")
	# to match mutt(1) output.
	$eml->header_set('Status', join('', sort(@{$hdr{Status}})). 'O');
	if (my $chars = delete $hdr{'X-Status'}) {
		$eml->header_set('X-Status', join('', sort(@$chars)));
	}
	my $buf = delete $eml->{hdr};

	# fixup old bug from import (pre-a0c07cba0e5d8b6a)
	$$buf =~ s/\A[\r\n]*From [^\r\n]*\r?\n//s;
	my $ident = $smsg->{blob} // 'lei';
	if (defined(my $pct = $smsg->{pct})) { $ident .= "=$pct" }

	substr($$buf, 0, 0, # prepend From line
		"From $ident\@$type Thu Jan  1 00:00:00 1970$eml->{crlf}");
	$buf;
}

sub atomic_append { # for on-disk destinations (O_APPEND, or O_EXCL)
	my ($lei, $buf) = @_;
	if (defined(my $w = syswrite($lei->{1} // return, $$buf))) {
		return if $w == length($$buf);
		$buf = "short atomic write: $w != ".length($$buf);
	} elsif ($!{EPIPE}) {
		return $lei->note_sigpipe(1);
	} else {
		$buf = "atomic write: $!";
	}
	$lei->fail($buf);
}

sub eml2mboxrd ($;$) {
	my ($eml, $smsg) = @_;
	my $buf = _mbox_hdr_buf($eml, 'mboxrd', $smsg);
	if (my $bdy = delete $eml->{bdy}) {
		$$bdy =~ s/^(>*From )/>$1/gm;
		$$buf .= $eml->{crlf};
		substr($$bdy, 0, 0, $$buf); # prepend header
		$buf = $bdy;
	}
	$$buf .= $eml->{crlf};
	$buf;
}

sub eml2mboxo {
	my ($eml, $smsg) = @_;
	my $buf = _mbox_hdr_buf($eml, 'mboxo', $smsg);
	if (my $bdy = delete $eml->{bdy}) {
		$$bdy =~ s/^From />From /gm;
		$$buf .= $eml->{crlf};
		substr($$bdy, 0, 0, $$buf); # prepend header
		$buf = $bdy;
	}
	$$buf .= $eml->{crlf};
	$buf;
}

sub _mboxcl_common ($$$) {
	my ($buf, $bdy, $crlf) = @_;
	# add Lines: so mutt won't have to add it on MUA close
	my $lines = $$bdy =~ tr!\n!\n!;
	$$buf .= 'Content-Length: '.length($$bdy).$crlf.
		'Lines: '.$lines.$crlf.$crlf;
	substr($$bdy, 0, 0, $$buf); # prepend header
	$_[0] = $bdy;
}

# mboxcl still escapes "From " lines
sub eml2mboxcl {
	my ($eml, $smsg) = @_;
	my $buf = _mbox_hdr_buf($eml, 'mboxcl', $smsg);
	my $crlf = $eml->{crlf};
	if (my $bdy = delete $eml->{bdy}) {
		$$bdy =~ s/^From />From /gm;
		_mboxcl_common($buf, $bdy, $crlf);
	}
	$$buf .= $crlf;
	$buf;
}

# mboxcl2 has no "From " escaping
sub eml2mboxcl2 {
	my ($eml, $smsg) = @_;
	my $buf = _mbox_hdr_buf($eml, 'mboxcl2', $smsg);
	my $crlf = $eml->{crlf};
	if (my $bdy = delete $eml->{bdy}) {
		_mboxcl_common($buf, $bdy, $crlf);
	}
	$$buf .= $crlf;
	$buf;
}

sub git_to_mail { # git->cat_async callback
	my ($bref, $oid, $type, $size, $arg) = @_;
	return warn("W: $oid is $type (!= blob)\n") if $type ne 'blob';
	return warn("E: $oid is empty\n") unless $size;
	my ($write_cb, $smsg) = @$arg;
	die "BUG: expected=$smsg->{blob} got=$oid" if $smsg->{blob} ne $oid;
	$write_cb->($bref, $smsg);
}

sub reap_compress { # dwaitpid callback
	my ($lei, $pid) = @_;
	my $cmd = delete $lei->{"pid.$pid"};
	return if $? == 0;
	$lei->fail("@$cmd failed", $? >> 8);
}

sub _post_augment_mbox { # open a compressor process from top-level process
	my ($self, $lei) = @_;
	my $zsfx = $self->{zsfx} or return;
	my $cmd = PublicInbox::MboxReader::zsfx2cmd($zsfx, undef, $lei);
	my ($r, $w) = @{delete $lei->{zpipe}};
	my $rdr = { 0 => $r, 1 => $lei->{1}, 2 => $lei->{2} };
	my $pid = spawn($cmd, undef, $rdr);
	my $pp = gensym;
	my $dup = bless { "pid.$pid" => $cmd }, ref($lei);
	$dup->{$_} = $lei->{$_} for qw(2 sock);
	tie *$pp, 'PublicInbox::ProcessPipe', $pid, $w, \&reap_compress, $dup;
	$lei->{1} = $pp;
}

# --augment existing output destination, with deduplication
sub _augment { # MboxReader eml_cb
	my ($eml, $lei) = @_;
	# ignore return value, just populate the skv
	$lei->{dedupe}->is_dup($eml);
}

sub _mbox_augment_kw_maybe {
	my ($eml, $lei, $lse, $augment) = @_;
	my $kw = PublicInbox::MboxReader::mbox_keywords($eml);
	update_kw_maybe($lei, $lse, $eml, $kw);
	_augment($eml, $lei) if $augment;
}

sub _mbox_write_cb ($$) {
	my ($self, $lei) = @_;
	my $ovv = $lei->{ovv};
	my $m = 'eml2'.$ovv->{fmt};
	my $eml2mbox = $self->can($m) or die "$self->$m missing";
	$lei->{1} // die "no stdout ($m, $ovv->{dst})"; # redirected earlier
	$lei->{1}->autoflush(1);
	my $atomic_append = !defined($ovv->{lock_path});
	my $dedupe = $lei->{dedupe};
	$dedupe->prepare_dedupe;
	my $lse = $lei->{lse}; # may be undef
	sub { # for git_to_mail
		my ($buf, $smsg, $eml) = @_;
		$eml //= PublicInbox::Eml->new($buf);
		return if $dedupe->is_dup($eml, $smsg);
		$lse->xsmsg_vmd($smsg) if $lse;
		$buf = $eml2mbox->($eml, $smsg);
		return atomic_append($lei, $buf) if $atomic_append;
		my $lk = $ovv->lock_for_scope;
		$lei->out($$buf);
	}
}

sub update_kw_maybe ($$$$) {
	my ($lei, $lse, $eml, $kw) = @_;
	return unless $lse;
	my $c = $lse->kw_changed($eml, $kw, my $docids = []);
	my $vmd = { kw => $kw };
	if (scalar @$docids) { # already in lei/store
		$lei->{sto}->ipc_do('set_eml_vmd', undef, $vmd, $docids) if $c;
	} elsif (my $xoids = $lei->{ale}->xoids_for($eml)) {
		# it's in an external, only set kw, here
		$lei->{sto}->ipc_do('set_xvmd', $xoids, $eml, $vmd);
	} else { # never-before-seen, import the whole thing
		# XXX this is critical in protecting against accidental
		# data loss without --augment
		$lei->{sto}->ipc_do('set_eml', $eml, $vmd);
	}
}

sub _md_update { # maildir_each_eml cb
	my ($f, $kw, $eml, $lei, $lse, $unlink) = @_;
	update_kw_maybe($lei, $lse, $eml, $kw);
	$unlink ? unlink($f) : _augment($eml, $lei);
}

# maildir_each_file callback, \&CORE::unlink doesn't work with it
sub _unlink { unlink($_[0]) }

sub _rand () {
	state $seq = 0;
	sprintf('%x,%x,%x,%x', rand(0xffffffff), time, $$, ++$seq);
}

sub _buf2maildir {
	my ($dst, $buf, $smsg) = @_;
	my $kw = $smsg->{kw} // [];
	my $sfx = join('', sort(map { $kw2char{$_} // () } @$kw));
	my $rand = ''; # chosen by die roll :P
	my ($tmp, $fh, $final, $ok);
	my $common = $smsg->{blob} // _rand;
	if (defined(my $pct = $smsg->{pct})) { $common .= "=$pct" }
	do {
		$tmp = $dst.'tmp/'.$rand.$common;
	} while (!($ok = sysopen($fh, $tmp, O_CREAT|O_EXCL|O_WRONLY)) &&
		$!{EEXIST} && ($rand = _rand.','));
	if ($ok && print $fh $$buf and close($fh)) {
		# ignore new/ and write only to cur/, otherwise MUAs
		# with R/W access to the Maildir will end up doing
		# a mass rename which can take a while with thousands
		# of messages.
		$dst .= 'cur/';
		$rand = '';
		do {
			$final = $dst.$rand.$common.':2,'.$sfx;
		} while (!($ok = link($tmp, $final)) && $!{EEXIST} &&
			($rand = _rand.','));
		die "link($tmp, $final): $!" unless $ok;
		unlink($tmp) or warn "W: failed to unlink $tmp: $!\n";
	} else {
		my $err = "Error writing $smsg->{blob} to $dst: $!\n";
		$_[0] = undef; # clobber dst
		unlink($tmp);
		die $err;
	}
}

sub _maildir_write_cb ($$) {
	my ($self, $lei) = @_;
	my $dedupe = $lei->{dedupe};
	$dedupe->prepare_dedupe if $dedupe;
	my $dst = $lei->{ovv}->{dst};
	my $lse = $lei->{lse}; # may be undef
	sub { # for git_to_mail
		my ($buf, $smsg, $eml) = @_;
		$dst // return $lei->fail; # dst may be undef-ed in last run
		$buf //= \($eml->as_string);
		$lse->xsmsg_vmd($smsg) if $lse;
		return _buf2maildir($dst, $buf, $smsg) if !$dedupe;
		$eml //= PublicInbox::Eml->new($$buf); # copy buf
		return if $dedupe->is_dup($eml, $smsg);
		undef $eml;
		_buf2maildir($dst, $buf, $smsg);
	}
}

sub _imap_write_cb ($$) {
	my ($self, $lei) = @_;
	my $dedupe = $lei->{dedupe};
	$dedupe->prepare_dedupe if $dedupe;
	my $imap_append = $lei->{net}->can('imap_append');
	my $mic = $lei->{net}->mic_get($self->{uri});
	my $folder = $self->{uri}->mailbox;
	my $lse = $lei->{lse}; # may be undef
	sub { # for git_to_mail
		my ($bref, $smsg, $eml) = @_;
		$mic // return $lei->fail; # mic may be undef-ed in last run
		if ($dedupe) {
			$eml //= PublicInbox::Eml->new($$bref); # copy bref
			return if $dedupe->is_dup($eml, $smsg);
		}
		$lse->xsmsg_vmd($smsg) if $lse;
		eval { $imap_append->($mic, $folder, $bref, $smsg, $eml) };
		if (my $err = $@) {
			undef $mic;
			die $err;
		}
	}
}

sub write_cb { # returns a callback for git_to_mail
	my ($self, $lei) = @_;
	# _mbox_write_cb, _maildir_write_cb or _imap_write_cb
	my $m = "_$self->{base_type}_write_cb";
	$self->$m($lei);
}

sub new {
	my ($cls, $lei) = @_;
	my $fmt = $lei->{ovv}->{fmt};
	my $dst = $lei->{ovv}->{dst};
	my $self = bless {}, $cls;
	if ($fmt eq 'maildir') {
		require PublicInbox::MdirReader;
		$self->{base_type} = 'maildir';
		-e $dst && !-d _ and die
				"$dst exists and is not a directory\n";
		$lei->{ovv}->{dst} = $dst .= '/' if substr($dst, -1) ne '/';
	} elsif (substr($fmt, 0, 4) eq 'mbox') {
		require PublicInbox::MboxReader;
		(-d $dst || (-e _ && !-w _)) and die
			"$dst exists and is not a writable file\n";
		$self->can("eml2$fmt") or die "bad mbox format: $fmt\n";
		$self->{base_type} = 'mbox';
	} elsif ($fmt =~ /\Aimaps?\z/) { # TODO .onion support
		require PublicInbox::NetWriter;
		my $net = PublicInbox::NetWriter->new;
		$net->add_url($dst);
		$net->{quiet} = $lei->{opt}->{quiet};
		my $err = $net->errors($dst);
		return $lei->fail($err) if $err;
		require PublicInbox::URIimap; # TODO: URI cast early
		$self->{uri} = PublicInbox::URIimap->new($dst);
		$self->{uri}->mailbox or die "No mailbox: $dst";
		$lei->{net} = $net;
		$self->{base_type} = 'imap';
	} else {
		die "bad mail --format=$fmt\n";
	}
	$self->{dst} = $dst;
	$lei->{dedupe} = $lei->{lss} // do {
		my $dd_cls = 'PublicInbox::'.
			($lei->{opt}->{save} ? 'LeiSavedSearch' : 'LeiDedupe');
		eval "require $dd_cls";
		die "$dd_cls: $@" if $@;
		$dd_cls->new($lei);
	};
	$self;
}

sub _pre_augment_maildir {
	my ($self, $lei) = @_;
	my $dst = $lei->{ovv}->{dst};
	for my $x (qw(tmp new cur)) {
		my $d = $dst.$x;
		next if -d $d;
		require File::Path;
		File::Path::mkpath($d);
		-d $d or die "$d is not a directory";
	}
}

sub _do_augment_maildir {
	my ($self, $lei) = @_;
	return if defined($lei->{opt}->{save});
	my $dst = $lei->{ovv}->{dst};
	my $lse = $lei->{opt}->{'import-before'} ? $lei->{lse} : undef;
	my $mdr = PublicInbox::MdirReader->new;
	if ($lei->{opt}->{augment}) {
		my $dedupe = $lei->{dedupe};
		if ($dedupe && $dedupe->prepare_dedupe) {
			$mdr->{shard_info} = $self->{shard_info};
			$mdr->maildir_each_eml($dst, \&_md_update, $lei, $lse);
			$dedupe->pause_dedupe;
		}
	} elsif ($lse) {
		$mdr->{shard_info} = $self->{shard_info};
		$mdr->maildir_each_eml($dst, \&_md_update, $lei, $lse, 1);
	} else {# clobber existing Maildir
		$mdr->maildir_each_file($dst, \&_unlink);
	}
}

sub _imap_augment_or_delete { # PublicInbox::NetReader::imap_each cb
	my ($url, $uid, $kw, $eml, $lei, $lse, $delete_mic) = @_;
	update_kw_maybe($lei, $lse, $eml, $kw);
	if ($delete_mic) {
		$lei->{net}->imap_delete_1($url, $uid, $delete_mic);
	} else {
		_augment($eml, $lei);
	}
}

sub _do_augment_imap {
	my ($self, $lei) = @_;
	return if defined($lei->{opt}->{save});
	my $net = $lei->{net};
	my $lse = $lei->{opt}->{'import-before'} ? $lei->{lse} : undef;
	if ($lei->{opt}->{augment}) {
		my $dedupe = $lei->{dedupe};
		if ($dedupe && $dedupe->prepare_dedupe) {
			$net->imap_each($self->{uri}, \&_imap_augment_or_delete,
					$lei, $lse);
			$dedupe->pause_dedupe;
		}
	} elsif ($lse) {
		my $delete_mic;
		$net->imap_each($self->{uri}, \&_imap_augment_or_delete,
					$lei, $lse, \$delete_mic);
		$delete_mic->expunge if $delete_mic;
	} elsif (!$self->{-wq_worker_nr}) { # undef or 0
		# clobber existing IMAP folder
		$net->imap_delete_all($self->{uri});
	}
}

sub _pre_augment_mbox {
	my ($self, $lei) = @_;
	my $dst = $lei->{ovv}->{dst};
	my $out;
	my $devfd = $lei->path_to_fd($dst) // die "bad $dst";
	if ($devfd >= 0) {
		$out = $lei->{$devfd};
	} else { # normal-looking path
		if (-p $dst) {
			open $out, '>', $dst or die "open($dst): $!";
		} elsif (-f _ || !-e _) {
			require PublicInbox::MboxLock;
			my $m = $lei->{opt}->{'lock'} //
					PublicInbox::MboxLock->defaults;
			$self->{mbl} = PublicInbox::MboxLock->acq($dst, 1, $m);
			$out = $self->{mbl}->{fh};
		} else {
			die "$dst is not a file or FIFO\n";
		}
		$lei->{old_1} = $lei->{1}; # keep for spawning MUA
	}
	# Perl does SEEK_END even with O_APPEND :<
	$self->{seekable} = seek($out, 0, SEEK_SET);
	if (!$self->{seekable} && !$!{ESPIPE} && !defined($devfd)) {
		die "seek($dst): $!\n";
	}
	if (!$self->{seekable}) {
		my $imp_before = $lei->{opt}->{'import-before'};
		die "--import-before specified but $dst is not seekable\n"
			if $imp_before && !ref($imp_before);
		die "--augment specified but $dst is not seekable\n" if
			$lei->{opt}->{augment};
	}
	if ($self->{zsfx} = PublicInbox::MboxReader::zsfx($dst)) {
		pipe(my ($r, $w)) or die "pipe: $!";
		$lei->{zpipe} = [ $r, $w ];
		$lei->{ovv}->{lock_path} and
			die 'BUG: unexpected {ovv}->{lock_path}';
		$lei->{ovv}->ovv_out_lk_init;
	} elsif (!$self->{seekable} && !$lei->{ovv}->{lock_path}) {
		$lei->{ovv}->ovv_out_lk_init;
	}
	$lei->{1} = $out;
	undef;
}

sub _do_augment_mbox {
	my ($self, $lei) = @_;
	return unless $self->{seekable};
	my $opt = $lei->{opt};
	return if defined($opt->{save});
	my $out = $lei->{1};
	my ($fmt, $dst) = @{$lei->{ovv}}{qw(fmt dst)};
	return unless -s $out;
	unless ($opt->{augment} || $opt->{'import-before'}) {
		truncate($out, 0) or die "truncate($dst): $!";
		return;
	}
	my $rd;
	if (my $zsfx = $self->{zsfx}) {
		$rd = PublicInbox::MboxReader::zsfxcat($out, $zsfx, $lei);
	} else {
		open($rd, '+>>&', $out) or die "dup: $!";
	}
	my $dedupe;
	if ($opt->{augment}) {
		$dedupe = $lei->{dedupe};
		$dedupe->prepare_dedupe if $dedupe;
	}
	if ($opt->{'import-before'}) { # the default
		my $lse = $lei->{lse};
		PublicInbox::MboxReader->$fmt($rd, \&_mbox_augment_kw_maybe,
						$lei, $lse, $opt->{augment});
		if (!$opt->{augment} and !truncate($out, 0)) {
			die "truncate($dst): $!";
		}
	} else { # --augment --no-import-before
		PublicInbox::MboxReader->$fmt($rd, \&_augment, $lei);
	}
	# maybe some systems don't honor O_APPEND, Perl does this:
	seek($out, 0, SEEK_END) or die "seek $dst: $!";
	$dedupe->pause_dedupe if $dedupe;
}

sub pre_augment { # fast (1 disk seek), runs in same process as post_augment
	my ($self, $lei) = @_;
	# _pre_augment_maildir, _pre_augment_mbox
	my $m = $self->can("_pre_augment_$self->{base_type}") or return;
	$m->($self, $lei);
}

sub do_augment { # slow, runs in wq worker
	my ($self, $lei) = @_;
	# _do_augment_maildir, _do_augment_mbox, or _do_augment_imap
	my $m = "_do_augment_$self->{base_type}";
	$self->$m($lei);
}

# fast (spawn compressor or mkdir), runs in same process as pre_augment
sub post_augment {
	my ($self, $lei, @args) = @_;
	my $wait = $lei->{opt}->{'import-before'} ?
			$lei->{sto}->ipc_do('checkpoint', 1) : 0;
	# _post_augment_mbox
	my $m = $self->can("_post_augment_$self->{base_type}") or return;
	$m->($self, $lei, @args);
}

# called by every single l2m worker process
sub do_post_auth {
	my ($self) = @_;
	my $lei = $self->{lei};
	# lei_xsearch can start as soon as all l2m workers get here
	pkt_do($lei->{pkt_op_p}, 'incr_start_query') or
		die "incr_start_query: $!";
	my $aug;
	if (lock_free($self)) { # all workers do_augment
		my $mod = $self->{-wq_nr_workers};
		my $shard = $self->{-wq_worker_nr};
		if (my $net = $lei->{net}) {
			$net->{shard_info} = [ $mod, $shard ];
		} else { # Maildir
			$self->{shard_info} = [ $mod, $shard ];
		}
		$aug = '+'; # incr_post_augment
	} elsif ($self->{-wq_worker_nr} == 0) { # 1st worker do_augment
		$aug = '.'; # do_post_augment
	}
	if ($aug) {
		local $0 = 'do_augment';
		eval { do_augment($self, $lei) };
		$lei->fail($@) if $@;
		pkt_do($lei->{pkt_op_p}, $aug) == 1 or
				die "do_post_augment trigger: $!";
	}
	# done augmenting, connect the compressor pipe for each worker
	if (my $zpipe = delete $lei->{zpipe}) {
		$lei->{1} = $zpipe->[1];
		close $zpipe->[0];
	}
	$self->{wcb} = $self->write_cb($lei);
}

sub ipc_atfork_child {
	my ($self) = @_;
	my $lei = $self->{lei};
	$lei->_lei_atfork_child;
	$lei->{auth}->do_auth_atfork($self) if $lei->{auth};
	$SIG{__WARN__} = PublicInbox::Eml::warn_ignore_cb();
	$self->SUPER::ipc_atfork_child;
}

sub lock_free {
	$_[0]->{base_type} =~ /\A(?:maildir|imap|jmap)\z/ ? 1 : 0;
}

# wakes up the MUA when complete so it can refresh messages list
sub poke_dst {
	my ($self) = @_;
	if ($self->{base_type} eq 'maildir') {
		my $t = time + 1;
		utime($t, $t, $self->{dst} . 'cur');
	}
}

sub write_mail { # via ->wq_io_do
	my ($self, $smsg, $eml) = @_;
	return $self->{wcb}->(undef, $smsg, $eml) if $eml;
	$self->{lei}->{ale}->git->cat_async($smsg->{blob}, \&git_to_mail,
				[$self->{wcb}, $smsg]);
}

sub wq_atexit_child {
	my ($self) = @_;
	delete $self->{wcb};
	$self->{lei}->{ale}->git->async_wait_all;
}

# called in top-level lei-daemon when LeiAuth is done
sub net_merge_complete {
	my ($self) = @_;
	$self->wq_broadcast('do_post_auth');
	$self->wq_close(1);
}

no warnings 'once'; # the following works even when LeiAuth is lazy-loaded
*net_merge_all = \&PublicInbox::LeiAuth::net_merge_all;
1;
