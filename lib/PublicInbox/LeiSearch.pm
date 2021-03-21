# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# read-only counterpart for PublicInbox::LeiStore
package PublicInbox::LeiSearch;
use strict;
use v5.10.1;
use parent qw(PublicInbox::ExtSearch);
use PublicInbox::Search qw(xap_terms);
use PublicInbox::ContentHash qw(content_digest content_hash);
use PublicInbox::MID qw(mids mids_in);

# get combined docid from over.num:
# (not generic Xapian, only works with our sharding scheme)
sub num2docid ($$) {
	my ($self, $num) = @_;
	my $nshard = $self->{nshard};
	($num - 1) * $nshard + $num % $nshard + 1;
}

sub msg_keywords {
	my ($self, $num) = @_; # num_or_mitem
	my $xdb = $self->xdb; # set {nshard};
	my $docid = ref($num) ? $num->get_docid : num2docid($self, $num);
	my $kw = xap_terms('K', $xdb, $docid);
	warn "E: #$docid ($num): $@\n" if $@;
	wantarray ? sort(keys(%$kw)) : $kw;
}

sub xsmsg_vmd {
	my ($self, $smsg) = @_;
	return if $smsg->{kw};
	my $xdb = $self->xdb; # set {nshard};
	my %kw;
	$kw{flagged} = 1 if delete($smsg->{lei_q_tt_flagged});
	my @num = $self->over->blob_exists($smsg->{blob});
	for my $num (@num) { # there should only be one...
		my $kw = xap_terms('K', $xdb, num2docid($self, $num));
		%kw = (%kw, %$kw);
	}
	$smsg->{kw} = [ sort keys %kw ] if scalar(keys(%kw));
}

# when a message has no Message-IDs at all, this is needed for
# unsent Draft messages, at least
sub content_key ($) {
	my ($eml) = @_;
	my $dig = content_digest($eml);
	my $chash = $dig->clone->digest;
	my $mids = mids_in($eml,
			qw(Message-ID X-Alt-Message-ID Resent-Message-ID));
	unless (@$mids) {
		$eml->{-lei_fake_mid} = $mids->[0] =
				PublicInbox::Import::digest2mid($dig, $eml);
	}
	($chash, $mids);
}

sub _cmp_1st { # git->cat_async callback
	my ($bref, $oid, $type, $size, $cmp) = @_; # cmp: [chash, xoids, smsg]
	if ($bref && content_hash(PublicInbox::Eml->new($bref)) eq $cmp->[0]) {
		$cmp->[1]->{$oid} = $cmp->[2]->{num};
	}
}

# returns { OID => num } mapping for $eml matches
# The `num' hash value only makes sense from LeiSearch itself
# and is nonsense from the PublicInbox::LeiALE subclass
sub xoids_for {
	my ($self, $eml, $min) = @_;
	my ($chash, $mids) = content_key($eml);
	my @overs = ($self->over // $self->overs_all);
	my $git = $self->git;
	my $xoids = {};
	for my $mid (@$mids) {
		for my $o (@overs) {
			my ($id, $prev);
			while (my $cur = $o->next_by_mid($mid, \$id, \$prev)) {
				next if $cur->{bytes} == 0 ||
					$xoids->{$cur->{blob}};
				$git->cat_async($cur->{blob}, \&_cmp_1st,
						[ $chash, $xoids, $cur ]);
				if ($min && scalar(keys %$xoids) >= $min) {
					$git->cat_async_wait;
					return $xoids;
				}
			}
		}
	}
	$git->cat_async_wait;
	scalar(keys %$xoids) ? $xoids : undef;
}

# returns true if $eml is indexed by lei/store and keywords don't match
sub kw_changed {
	my ($self, $eml, $new_kw_sorted) = @_;
	my $xoids = xoids_for($self, $eml, 1) // return;
	my ($num) = values %$xoids;
	my @cur_kw = msg_keywords($self, $num);
	join("\0", @$new_kw_sorted) eq join("\0", @cur_kw) ? 0 : 1;
}

1;
