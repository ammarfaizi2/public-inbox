# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# front-end for the "lei import" sub-command
package PublicInbox::LeiImport;
use strict;
use v5.10.1;
use parent qw(PublicInbox::IPC PublicInbox::LeiInput);
use PublicInbox::InboxWritable qw(eml_from_path);
use PublicInbox::Compat qw(uniqstr);

# /^input_/ subs are used by (or override) PublicInbox::LeiInput superclass

sub input_eml_cb { # used by PublicInbox::LeiInput::input_fh
	my ($self, $eml, $vmd) = @_;
	my $xoids = $self->{lei}->{ale}->xoids_for($eml);
	if (my $all_vmd = $self->{all_vmd}) {
		@$vmd{keys %$all_vmd} = values %$all_vmd;
	}
	$self->{lei}->{sto}->wq_do('set_eml', $eml, $vmd, $xoids);
}

sub input_mbox_cb { # MboxReader callback
	my ($eml, $self) = @_;
	my $vmd;
	if ($self->{-import_kw}) {
		my $kw = PublicInbox::MboxReader::mbox_keywords($eml);
		$vmd = { kw => $kw } if scalar(@$kw);
	}
	input_eml_cb($self, $eml, $vmd);
}

sub pmdir_cb { # called via wq_io_do from LeiPmdir->each_mdir_fn
	my ($self, $f, $fl) = @_;
	my ($folder, $bn) = ($f =~ m!\A(.+?)/(?:new|cur)/([^/]+)\z!) or
		die "BUG: $f was not from a Maildir?\n";
	my $kw = PublicInbox::MdirReader::flags2kw($fl);
	substr($folder, 0, 0) = 'maildir:'; # add prefix
	my $lse = $self->{lse} //= $self->{lei}->{sto}->search;
	my $lms = $self->{-lms_rw} //= $self->{lei}->lms; # may be 0 or undef
	my @oidbin = $lms ? $lms->name_oidbin($folder, $bn) : ();
	@oidbin > 1 and warn("W: $folder/*/$$bn not unique:\n",
				map { "\t".unpack('H*', $_)."\n" } @oidbin);
	my @docids = sort { $a <=> $b } uniqstr
			map { $lse->over->oidbin_exists($_) } @oidbin;
	my $vmd = $self->{-import_kw} ? { kw => $kw } : undef;
	if (scalar @docids) {
		$lse->kw_changed(undef, $kw, \@docids) or return;
	}
	if (my $eml = eml_from_path($f)) {
		$vmd->{sync_info} = [ $folder, \$bn ] if $self->{-mail_sync};
		$self->input_eml_cb($eml, $vmd);
	}
}

sub input_net_cb { # imap_each / nntp_each
	my ($uri, $uid, $kw, $eml, $self) = @_;
	if (defined $eml) {
		my $vmd = $self->{-import_kw} ? { kw => $kw } : undef;
		$vmd->{sync_info} = [ $$uri, $uid ] if $self->{-mail_sync};
		$self->input_eml_cb($eml, $vmd);
	} elsif (my $ikw = $self->{lei}->{ikw}) { # old message, kw only
		# we send $uri as a bare SCALAR and not a URIimap ref to
		# reduce socket traffic:
		$ikw->wq_io_do('ck_update_kw', [], $$uri, $uid, $kw);
	}
}

sub do_import_index ($$@) {
	my ($self, $lei, @inputs) = @_;
	my $sto = $lei->_lei_store(1);
	$sto->write_prepare($lei);
	$self->{-import_kw} = $lei->{opt}->{kw} // 1;
	$self->{all_vmd} = $lei->{vmd_mod} if keys %{$lei->{vmd_mod}};
	$lei->ale; # initialize for workers to read (before LeiPmdir->new)
	$self->{-mail_sync} = $lei->{opt}->{'mail-sync'} // 1;
	$self->prepare_inputs($lei, \@inputs) or return;

	my $j = $lei->{opt}->{jobs} // 0;
	$j =~ /\A([0-9]+),[0-9]+\z/ and $j = $1 + 0;
	$j ||= scalar(@{$self->{inputs}}) || 1;
	my $ikw;
	my $net = $lei->{net};
	if ($net) {
		# $j = $net->net_concurrency($j); TODO
		if ($lei->{opt}->{incremental} // 1) {
			$net->{incremental} = 1;
			$net->{-lms_rw} = $lei->lms // 0;
			if ($self->{-import_kw} && $net->{-lms_rw} &&
					!$lei->{opt}->{'new-only'} &&
					$net->{imap_order}) {
				require PublicInbox::LeiImportKw;
				$ikw = PublicInbox::LeiImportKw->new($lei);
				$net->{each_old} = 1;
			}
		}
	} else {
		my $nproc = $self->detect_nproc;
		$j = $nproc if $j > $nproc;
	}
	($lei->{opt}->{'new-only'} && (!$net || !$net->{imap_order})) and
		warn "# --new-only is only for IMAP\n";
	$lei->{-eml_noisy} = 1;
	$lei->{-err_type} = 'non-fatal';
	$lei->wq1_start($self, $j);
}

sub lei_import { # the main "lei import" method
	my ($lei, @inputs) = @_;
	my $self = bless {}, __PACKAGE__;
	do_import_index($self, $lei, @inputs);
}

sub _complete_import {
	my ($lei, @argv) = @_;
	my $has_arg = @argv;
	my ($pfx, $cur, $match_cb) = $lei->complete_url_prepare(\@argv);
	my @try = $has_arg ? ($pfx.$cur, $argv[-1]) : ($argv[-1]);
	push(@try, undef) if defined $try[-1];
	my (@f, @k);
	for (@try) {
		@k = $lei->url_folder_cache->keys($_, 1) and last;
	}
	my @L = eval { $lei->_lei_store->search->all_terms('L') };
	push(@k, map { "+L:$_" } @L);
	if (my $lms = $lei->lms) {
		for (@try) {
			@f = $lms->folders($_, 1) and last;
		}
		push @k, @f;
	}
	my @m = map { $match_cb->($_) } @k;
	@m ? @m : @k;
}

no warnings 'once';
*ipc_atfork_child = \&PublicInbox::LeiInput::input_only_atfork_child;
*net_merge_all_done = \&PublicInbox::LeiInput::input_only_net_merge_all_done;

1;
