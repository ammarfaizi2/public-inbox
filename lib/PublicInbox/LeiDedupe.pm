# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::LeiDedupe;
use strict;
use v5.10.1;
use PublicInbox::ContentHash qw(content_hash git_sha);
use Digest::SHA ();

# n.b. mutt sets most of these headers not sure about Bytes
our @OID_IGNORE = qw(Status X-Status Content-Length Lines Bytes);

# best-effort regeneration of OID when augmenting existing results
sub _regen_oid ($) {
	my ($eml) = @_;
	my @stash; # stash away headers we shouldn't have in git
	for my $k (@OID_IGNORE) {
		my @v = $eml->header_raw($k) or next;
		push @stash, [ $k, \@v ];
		$eml->header_set($k); # restore below
	}
	my $dig = git_sha(1, $eml);
	for my $kv (@stash) { # restore stashed headers
		my ($k, @v) = @$kv;
		$eml->header_set($k, @v);
	}
	$dig->digest;
}

sub _oidbin ($) { defined($_[0]) ? pack('H*', $_[0]) : undef }

sub smsg_hash ($) {
	my ($smsg) = @_;
	my $dig = Digest::SHA->new(256);
	my $x = join("\0", @$smsg{qw(from to cc ds subject references mid)});
	utf8::encode($x);
	$dig->add($x);
	$dig->digest;
}

# the paranoid option
sub dedupe_oid ($) {
	my ($skv) = @_;
	(sub { # may be called in a child process
		my ($eml, $oidhex) = @_;
		$skv->set_maybe(_oidbin($oidhex) // _regen_oid($eml), '');
	}, sub {
		my ($smsg) = @_;
		$skv->set_maybe(_oidbin($smsg->{blob}), '');
	});
}

# dangerous if there's duplicate messages with different Message-IDs
sub dedupe_mid ($) {
	my ($skv) = @_;
	(sub { # may be called in a child process
		my ($eml, $oidhex) = @_;
		# lei supports non-public drafts w/o Message-ID
		my $mid = $eml->header_raw('Message-ID') // _oidbin($oidhex) //
			content_hash($eml);
		$skv->set_maybe($mid, '');
	}, sub {
		my ($smsg) = @_;
		my $mid = $smsg->{mid};
		$mid = undef if $mid eq '';
		$mid //= smsg_hash($smsg) // _oidbin($smsg->{blob});
		$skv->set_maybe($mid, '');
	});
}

# our default deduplication strategy (used by v2, also)
sub dedupe_content ($) {
	my ($skv) = @_;
	(sub { # may be called in a child process
		my ($eml) = @_; # $oidhex = $_[1], ignored
		$skv->set_maybe(content_hash($eml), '');
	}, sub {
		my ($smsg) = @_;
		$skv->set_maybe(smsg_hash($smsg), '');
	});
}

# no deduplication at all
sub true { 1 }
sub dedupe_none ($) { (\&true, \&true) }

sub new {
	my ($cls, $lei) = @_;
	my $dd = $lei->{opt}->{dedupe} // 'content';
	my $dst = $lei->{ovv}->{dst};

	# allow "none" to bypass Eml->new if writing to directory:
	return if ($dd eq 'none' && substr($dst // '', -1) eq '/');
	my $m = "dedupe_$dd";
	$cls->can($m) or die "unsupported dedupe strategy: $dd\n";
	my $skv;
	if ($dd ne 'none') {
		require PublicInbox::SharedKV;
		$skv = PublicInbox::SharedKV->new;
	}
	# [ $skv, $eml_cb, $smsg_cb, "dedupe_$dd" ]
	bless [ $skv, undef, undef, $m ], $cls;
}

# returns true on seen messages according to the deduplication strategy,
# returns false if unseen
sub is_dup {
	my ($self, $eml, $smsg) = @_;
	!$self->[1]->($eml, $smsg ? $smsg->{blob} : undef);
}

sub is_smsg_dup {
	my ($self, $smsg) = @_;
	!$self->[2]->($smsg);
}

sub prepare_dedupe {
	my ($self) = @_;
	my $skv = $self->[0];
	$self->[1] or @$self[1,2] = $self->can($self->[3])->($skv);
	$skv ? $skv->dbh : undef;
}

sub pause_dedupe {
	my ($self) = @_;
	my $skv = $self->[0] or return;
	$skv->dbh_release;
	delete($skv->{dbh}) if $skv;
}

sub has_entries {
	my $skv = $_[0]->[0] or return undef;
	$skv->has_entries;
}

1;
