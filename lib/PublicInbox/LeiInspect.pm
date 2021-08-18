# Copyright (C) 2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# "lei inspect" general purpose inspector for stuff in SQLite and
# Xapian.  Will eventually be useful with plain public-inboxes,
# not just lei/store.  This is totally half-baked at the moment
# but useful for testing.
package PublicInbox::LeiInspect;
use strict;
use v5.10.1;
use PublicInbox::Config;

sub inspect_blob ($$) {
	my ($lei, $oidhex) = @_;
	my $ent = {};
	if (my $lse = $lei->{lse}) {
		my $oidbin = pack('H*', $oidhex);
		my @docids = $lse ? $lse->over->oidbin_exists($oidbin) : ();
		$ent->{'lei/store'} = \@docids if @docids;
		my $lms = $lse->lms;
		if (my $loc = $lms ? $lms->locations_for($oidbin) : undef) {
			$ent->{'mail-sync'} = $loc;
		}
	}
	$ent;
}

sub inspect_imap_uid ($$) {
	my ($lei, $uid_uri) = @_;
	my $ent = {};
	my $lse = $lei->{lse} or return $ent;
	my $lms = $lse->lms or return $ent;
	my $oidhex = $lms->imap_oid($lei, $uid_uri);
	if (ref(my $err = $oidhex)) { # art2folder error
		$lei->qerr(@{$err->{qerr}}) if $err->{qerr};
	}
	$ent->{$$uid_uri} = $oidhex;
	$ent;
}

sub inspect_sync_folder ($$) {
	my ($lei, $folder) = @_;
	my $ent = {};
	my $lms = $lei->lms or return $ent;
	my $folders = [ $folder ];
	my $err = $lms->arg2folder($lei, $folders);
	if ($err) {
		if ($err->{fail}) {
			$lei->qerr("# no folders match $folder (non-fatal)");
			@$folders = ();
		}
		$lei->qerr(@{$err->{qerr}}) if $err->{qerr};
	}
	for my $f (@$folders) {
		$ent->{$f} = $lms->location_stats($f); # may be undef
	}
	$ent
}

sub inspect_docid ($$;$) {
	my ($lei, $docid, $ent) = @_;
	require PublicInbox::Search;
	$ent //= {};
	my $xdb;
	if ($xdb = delete $ent->{xdb}) { # from inspect_num
	} elsif (defined(my $dir = $lei->{opt}->{dir})) {
		no warnings 'once';
		$xdb = $PublicInbox::Search::X{Database}->new($dir);
	} else {
		$xdb = $lei->{lse}->xdb;
	}
	$xdb or return $lei->fail('no Xapian DB');
	my $doc = $xdb->get_document($docid); # raises
	my $data = $doc->get_data;
	$ent->{docid} = $docid;
	$ent->{data_length} = length($data);
	$ent->{description} = $doc->get_description;
	$ent->{$_} = $doc->$_ for (qw(termlist_count values_count));
	my $cur = $doc->termlist_begin;
	my $end = $doc->termlist_end;
	for (; $cur != $end; $cur++) {
		my $tn = $cur->get_termname;
		$tn =~ s/\A([A-Z]+)// or warn "$tn no prefix! (???)";
		my $term = ($1 // '');
		push @{$ent->{terms}->{$term}}, $tn;
	}
	@$_ = sort(@$_) for values %{$ent->{terms} // {}};
	$cur = $doc->values_begin;
	$end = $doc->values_end;
	for (; $cur != $end; $cur++) {
		my $n = $cur->get_valueno;
		my $v = $cur->get_value;
		my $iv = PublicInbox::Search::sortable_unserialise($v);
		$v = $iv + 0 if defined $iv;
		# not using ->[$n] since we may have large gaps in $n
		$ent->{'values'}->{$n} = $v;
	}
	$ent;
}

sub dir2ibx ($$) {
	my ($lei, $dir) = @_;
	if (-f "$dir/ei.lock") {
		require PublicInbox::ExtSearch;
		PublicInbox::ExtSearch->new($dir);
	} elsif (-f "$dir/inbox.lock" || -d "$dir/public-inbox") {
		require PublicInbox::Inbox; # v2, v1
		bless { inboxdir => $dir }, 'PublicInbox::Inbox';
	} else {
		$lei->fail("no (indexed) inbox or extindex at $dir");
	}
}

sub inspect_num ($$) {
	my ($lei, $num) = @_;
	my ($docid, $ibx);
	my $ent = { num => $num };
	if (defined(my $dir = $lei->{opt}->{dir})) {
		$ibx = dir2ibx($lei, $dir) or return;
		if ($ent->{xdb} = $ibx->xdb) {
			my $num2docid = $lei->{lse}->can('num2docid');
			$docid = $num2docid->($ibx, $num);
		}
	} else {
		$ibx = $lei->{lse};
		$lei->{lse}->xdb; # set {nshard} for num2docid
		$docid = $lei->{lse}->num2docid($num);
	}
	if ($ibx && $ibx->over) {
		my $smsg = $ibx->over->get_art($num);
		$ent->{smsg} = { %$smsg } if $smsg;
	}
	defined($docid) ? inspect_docid($lei, $docid, $ent) : $ent;
}

sub inspect_mid ($$) {
	my ($lei, $mid) = @_;
	my ($ibx, $over);
	my $ent = { mid => $mid };
	if (defined(my $dir = $lei->{opt}->{dir})) {
		my $num2docid = $lei->{lse}->can('num mid => [ $mid ] 2docid');
		$ibx = dir2ibx($lei, $dir) or return;
		# $ent->{xdb} = $ibx->xdb //
			# return $lei->fail("no Xapian DB for $dir");
	} else {
		$ibx = $lei->{lse};
		$lei->{lse}->xdb; # set {nshard} for num2docid
	}
	if ($ibx && $ibx->over) {
		my ($id, $prev);
		while (my $smsg = $ibx->over->next_by_mid($mid, \$id, \$prev)) {
			push @{$ent->{smsg}}, { %$smsg }
		}
	}
	$ent;
}

sub inspect1 ($$$) {
	my ($lei, $item, $more) = @_;
	my $ent;
	if ($item =~ /\Ablob:(.+)/) {
		$ent = inspect_blob($lei, $1);
	} elsif ($item =~ m!\Aimaps?://!i) {
		require PublicInbox::URIimap;
		my $uri = PublicInbox::URIimap->new($item);
		if (defined($uri->uid)) {
			$ent = inspect_imap_uid($lei, $uri);
		} else {
			$ent = inspect_sync_folder($lei, $item);
		}
	} elsif ($item =~ m!\A(?:maildir|mh):!i || -d $item) {
		$ent = inspect_sync_folder($lei, $item);
	} elsif ($item =~ m!\Adocid:([0-9]+)\z!) {
		$ent = inspect_docid($lei, $1 + 0);
	} elsif ($item =~ m!\Anum:([0-9]+)\z!) {
		$ent = inspect_num($lei, $1 + 0);
	} elsif ($item =~ m!\A(?:mid|m):(.+)\z!) {
		$ent = inspect_mid($lei, $1);
	} else { # TODO: more things
		return $lei->fail("$item not understood");
	}
	$lei->out($lei->{json}->encode($ent));
	$lei->out(',') if $more;
	1;
}

sub lei_inspect {
	my ($lei, @argv) = @_;
	$lei->{json} = ref(PublicInbox::Config::json())->new->utf8->canonical;
	$lei->{lse} = ($lei->{opt}->{external} // 1) ? do {
		my $sto = $lei->_lei_store;
		$sto ? $sto->search : undef;
	} : undef;
	if ($lei->{opt}->{pretty} || -t $lei->{1}) {
		$lei->{json}->pretty(1)->indent(2);
	}
	$lei->start_pager if -t $lei->{1};
	$lei->{1}->autoflush(0);
	my $multi = scalar(@argv) > 1;
	$lei->out('[') if $multi;
	while (defined(my $x = shift @argv)) {
		inspect1($lei, $x, scalar(@argv)) or return;
	}
	$lei->out(']') if $multi;
}

sub _complete_inspect {
	my ($lei, @argv) = @_;
	my $sto = $lei->_lei_store or return;
	my $lms = $sto->search->lms or return;
	my $match_cb = $lei->complete_url_prepare(\@argv);
	map { $match_cb->($_) } $lms->folders;
}

1;
