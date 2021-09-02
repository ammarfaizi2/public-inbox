# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# internal command for dealing with inotify, kqueue vnodes, etc
package PublicInbox::LeiNoteEvent;
use strict;
use v5.10.1;
use parent qw(PublicInbox::IPC);

my $flush_timer;
our $to_flush; # { cfgpath => $lei }

sub flush_lei ($) {
	my ($lei) = @_;
	if (my $lne = delete $lei->{cfg}->{-lei_note_event}) {
		$lne->wq_close(1, undef, $lei); # runs _lei_wq_eof;
	} elsif ($lei->{sto}) { # lms_clear_src calls only:
		$lei->sto_done_request;
	}
}

# we batch up writes and flush every 5s (matching Linux default
# writeback behavior) since MUAs can trigger a storm of inotify events
sub flush_task { # PublicInbox::DS timer callback
	undef $flush_timer;
	my $todo = $to_flush // return;
	$to_flush = undef;
	for my $lei (values %$todo) { flush_lei($lei) }
}

# sets a timer to flush
sub note_event_arm_done ($) {
	my ($lei) = @_;
	$flush_timer //= PublicInbox::DS::add_timer(5, \&flush_task);
	$to_flush->{$lei->{cfg}->{'-f'}} //= $lei;
}

sub eml_event ($$$$) {
	my ($self, $eml, $vmd, $state) = @_;
	my $sto = $self->{lei}->{sto};
	my $lse = $self->{lse} //= $sto->search;
	if ($state =~ /\Aimport-(?:rw|ro)\z/) {
		$sto->ipc_do('set_eml', $eml, $vmd);
	} elsif ($state =~ /\Aindex-(?:rw|ro)\z/) {
		my $xoids = $self->{lei}->ale->xoids_for($eml);
		$sto->ipc_do('index_eml_only', $eml, $vmd, $xoids);
	} elsif ($state =~ /\Atag-(?:rw|ro)\z/) {
		my $c = $lse->kw_changed($eml, $vmd->{kw}, my $docids = []);
		if (scalar @$docids) { # already in lei/store
			$sto->ipc_do('set_eml_vmd', undef, $vmd, $docids) if $c;
		} elsif (my $xoids = $self->{lei}->ale->xoids_for($eml)) {
			# it's in an external, only set kw, here
			$sto->ipc_do('set_xvmd', $xoids, $eml, $vmd);
		} # else { totally unknown: ignore
	} else {
		warn "unknown state: $state (in $self->{lei}->{cfg}->{'-f'})\n";
	}
}

sub maildir_event { # via wq_io_do
	my ($self, $fn, $vmd, $state) = @_;
	my $eml = PublicInbox::InboxWritable::eml_from_path($fn) // return;
	eml_event($self, $eml, $vmd, $state);
}

sub lei_note_event {
	my ($lei, $folder, $new_cur, $bn, $fn, @rest) = @_;
	die "BUG: unexpected: @rest" if @rest;
	my $cfg = $lei->_lei_cfg or return; # gone (race)
	my $sto = $lei->_lei_store or return; # gone
	return flush_lei($lei) if $folder eq 'done'; # special case
	my $lms = $sto->search->lms or return;
	my $err = $lms->arg2folder($lei, [ $folder ]);
	return if $err->{fail};
	undef $lms;
	my $state = $cfg->get_1("watch.$folder", 'state') // 'tag-rw';
	return if $state eq 'pause';
	$lei->ale; # prepare
	$sto->write_prepare($lei);
	if ($new_cur eq '') {
		$sto->ipc_do('lms_clear_src', $folder, \$bn);
		return note_event_arm_done($lei);
	}
	require PublicInbox::MdirReader;
	my $self = $cfg->{-lei_note_event} //= do {
		my $wq = bless {}, __PACKAGE__;
		# MUAs such as mutt can trigger massive rename() storms so
		# use all CPU power available:
		my $jobs = $wq->detect_nproc // 1;
		my ($op_c, $ops) = $lei->workers_start($wq, $jobs);
		$lei->wait_wq_events($op_c, $ops);
		note_event_arm_done($lei);
		$lei->{lne} = $wq;
	};
	if ($folder =~ /\Amaildir:/i) {
		my $fl = PublicInbox::MdirReader::maildir_basename_flags($bn)
			// return;
		return if index($fl, 'T') >= 0;
		my $kw = PublicInbox::MdirReader::flags2kw($fl);
		my $vmd = { kw => $kw, sync_info => [ $folder, \$bn ] };
		$self->wq_io_do('maildir_event', [], $fn, $vmd, $state);
	} # else: TODO: imap
}

sub ipc_atfork_child {
	my ($self) = @_;
	$self->{lei}->_lei_atfork_child(1); # persistent, for a while
	$self->SUPER::ipc_atfork_child;
}

sub lne_done_wait {
	my ($arg, $pid) = @_;
	my ($self, $lei) = @$arg;
	$lei->can('wq_done_wait')->($arg, $pid);
}

sub _lei_wq_eof { # EOF callback for main lei daemon
	my ($lei) = @_;
	my $lne = delete $lei->{lne} or return $lei->fail;
	$lei->sto_done_request;
	$lne->wq_wait_old(\&lne_done_wait, $lei);
}

1;
