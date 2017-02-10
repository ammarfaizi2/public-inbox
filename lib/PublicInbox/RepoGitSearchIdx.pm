# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Qrefs/(tags|heads)/foo => 40-byte SHA1 hex of commit
# Q$SHA1HEX_OF_COMMIT
#
# Indexes any git repository with Xapian; intended for code;
# see PublicInbox::SearchIdx for a mail-specific indexer
package PublicInbox::RepoGitSearchIdx;
use strict;
use warnings;
use base qw(PublicInbox::RepoGitSearch); # base is read-only
use POSIX qw(strftime);
use PublicInbox::Git;
use PublicInbox::GitIdx;
use constant {
	Z40 => ('0' x 40),
	STATE_GPGSIG => -0x80000000,
	DEBUG => !!$ENV{DEBUG},
	BATCH_BYTES => 1_000_000,
};

sub new {
	my ($class, $git_dir, $repo_dir) = @_;
	require Search::Xapian::WritableDatabase;
	my $self = $class->SUPER::new($git_dir, $repo_dir);
	my $git = $self->{git} = PublicInbox::Git->new($git_dir);
	$self->{want_refs_re} = qr!^refs/(?:heads|tags)/!;
	$self->{'umask'} = git_umask_for($git);
	$self;
}

sub xdb ($) {
	my ($self) = @_;
	$self->{xdb} ||= with_umask($self->{'umask'}, sub {
		my $xdir = $self->{xdir};
		unless (-d $xdir) {
			require File::Path;
			File::Path::mkpath($xdir);
		}
		Search::Xapian::WritableDatabase->new($xdir,
				Search::Xapian::DB_CREATE_OR_OPEN);
	});
}

sub doc_new ($$) {
	my ($type, $unique_id) = @_;
	my $doc = Search::Xapian::Document->new;
	$doc->add_term('T'.$type);
	$doc->add_term($unique_id);
	$doc;
}

sub add_val ($$$) {
	my ($doc, $col, $num) = @_;
	$num = Search::Xapian::sortable_serialise($num);
	$doc->add_value($col, $num);
}

sub each_term_val ($$$$) {
	my ($doc, $pfx, $re, $cb) = @_;
	my $end = $doc->termlist_end;
	my $i = $doc->termlist_begin;
	$i->skip_to($pfx);
	while ($i != $end) {
		my $val = $i->get_termname;
		$val =~ s/$re// and $cb->($val);
		$i->inc;
	}
	undef;
}

sub get_doc ($$$$) {
	my ($self, $id_ref, $type, $oid) = @_;
	my $doc;
	my $doc_id = $self->find_unique_docid('Q'.$oid);
	if (defined $doc_id) {
		$doc = $self->{xdb}->get_document($doc_id);
	} else {
		$doc = doc_new($type, 'Q'.$oid);
	}
	$$id_ref = $doc_id;
	$doc;
}

# increments and returns update generation counter
sub update_id ($) {
	my ($self) = @_;
	my $db = $self->{xdb};
	my $update_id = int($db->get_metadata('last_update_id') || 0);
	$db->set_metadata('last_update_id', ++$update_id);
	$update_id;
}

sub replace_or_add ($$$) {
	my ($db, $doc_id, $doc) = @_;
	# update our ref:
	if (defined $doc_id) {
		$db->replace_document($doc_id, $doc);
	} else {
		$doc_id = $db->add_document($doc);
	}
	$doc_id;
}

sub decor_update {
	my ($self, $doc, $decor, $oid) = @_;

	# load all current refs
	my $want = $self->{want_refs_re};
	($decor) = ($decor =~ m!\((.+)\)!);
	foreach (split(/, /, $decor)) {
		my ($sym, $refname, $tag);
		if (/^(\S+) -> (\S+)\z/) {
			($sym, $refname) = ($1, $2);
		} elsif (s/^tag: //) {
			$refname = $_;
			$tag = 1; # XXX use this
		} else {
			$refname = $_;
		}
		if ($refname =~ $want) {
			$self->{-active_refs}->{$refname} = $oid;
		}
		# TODO: handle $sym, and do something with tags
	}
}

sub term_generator ($) { # write-only
	my ($self) = @_;

	$self->{term_generator} ||= eval {
		my $tg = Search::Xapian::TermGenerator->new;
		$tg->set_stemmer($self->stemmer);
		$tg;
	};
}

sub index_text_inc ($$$) {
	my ($tg, $text, $pfx) = @_;
	$tg->index_text($text, 1, $pfx);
	$tg->increase_termpos;
}

sub index_blob_id ($$$) {
	my ($tg, $blob_id, $pfx) = @_;
	index_text_inc($tg, $blob_id, $pfx) if $blob_id ne Z40;
}

sub each_log_line ($$) {
	my ($self, $range) = @_;
	my $log = $self->{git}->popen(qw(log --decorate=full --pretty=raw
			--no-color --no-abbrev --no-notes
			-r --raw -p
			), $range, '--');
	my $db = $self->{xdb};
	my ($doc, $doc_id);
	my $tg = term_generator($self);
	my $state = 0; # 1: subject, 2: body, 3: diff, 4: diff -c
	my $tip;
	my $hex = '[a-f0-9]+';
	my ($cc_ins, $cc_del);
	my $batch = BATCH_BYTES;

	local $/ = "\n";
	while (defined(my $l = <$log>)) {
		$batch -= bytes::length($l);
		# prevent memory growth from Xapian
		if ($batch <= 0) {
			$db->flush;
			$batch = BATCH_BYTES;
		}
		if ($l =~ /^commit (\S+)(\s+\([^\)]+\))?/) {
			my ($oid, $decor) = ($1, $2);
			replace_or_add($db, $doc_id, $doc) if $doc;
			$tip ||= $oid;
			$state = 0;
			$cc_ins = $cc_del = undef;

			$doc = get_doc($self, \$doc_id, 'commit', $oid);
			decor_update($self, $doc, $decor, $oid) if $decor;
			# old commit
			last if defined $doc_id;

			# new commit:
			$tg->set_document($doc);
			$doc->set_data($oid);
			$doc->add_term('Q' . $oid);
			index_text_inc($tg, $oid, 'Q');
		} elsif ($l =~ /^parent (\S+)/) {
			my $parent = $1;
			index_text_inc($tg, $parent, 'XP');
		} elsif ($l =~ /^author ([^<]*?<[^>]+>) (\d+)/) {
			my ($au, $at) = ($1, $2);
			index_text_inc($tg, $au, 'A');
			add_val($doc, PublicInbox::RepoGitSearch::AD,
				strftime('%Y%m%d', gmtime($at)));
		} elsif ($l =~ /^committer ([^<]*?<[^>]+>) (\d+)/) {
			my ($cu, $ct) = ($1, $2);
			index_text_inc($tg, $cu, 'XC');
			add_val($doc, PublicInbox::RepoGitSearch::CD,
				strftime('%Y%m%d', gmtime($ct)));
		} elsif ($l =~ /^gpgsig /) {
			$state = STATE_GPGSIG;
		} elsif ($l =~ /^mergetag /) {
			$state = -1;
		} elsif ($state < 0) { # inside mergetag or gpgsig
			if ($l eq " \n") { # paragraph
				$state--;
				$tg->increase_termpos;
			} elsif ($l eq "-----BEGIN PGP SIGNATURE-----\n") {
				# no point in indexing a PGP signature
				$state = STATE_GPGSIG;
			} elsif ($state == -2) { # mergetag subject
				$tg->index_text($l, 1);
				$tg->increase_termpos;
			} elsif ($state < -2 && $state > STATE_GPGSIG) {
				$tg->index_text($l); # mergetag body
			} elsif ($l eq "\n") {
				# end of mergetag, onto normal commit message
				$tg->increase_termpos;
				$state = 0;
			} elsif ($l =~ /^ (?:tag|tagger|type) /) {
				# ignored
			} elsif (DEBUG) {
				if ($state <= STATE_GPGSIG) {
				# skip
				} else {
					warn "unhandled mergetag: $l";
				}
			}
		} elsif ($state < 3 && $l =~ s/^    //) { # subject and body
			if ($state > 0) {
				$l =~ /\S/ ? $tg->index_text($l, 1)
						: $tg->increase_termpos;
				$state = 2;
			} else {
				$state = 1;
				$tg->index_text($l, 1, 'S') if $l ne "\n";
			}
		} elsif ($l =~ /^:\d{6} \d{6} ($hex) ($hex) (\S+)\t+(.+)/o) {
			# --raw output (regular)
			my ($pre, $post, $chg, $names) = ($1, $2, $3, $4);
			index_blob_id($tg, $pre, 'XPRE');
			index_blob_id($tg, $post, 'XPOST');
		} elsif ($l =~ /^(::+)(?:\d{6} )+ ($hex .+)? (\S+)\t+(.+)/o) {
			# --raw output (combined)
			my ($colons, $blobs, $chg, $names) = ($1, $2, $3, $4);
			my @blobs = split(/ /, $blobs);
			my $post = pop @blobs;
			my $n = length($colons);
			if (scalar(@blobs) != $n) {
				die "combined raw parsed wrong:\n$l\n//\n";
			}
			index_blob_id($tg, $_, 'XPRE') foreach @blobs;
			index_blob_id($tg, $post, 'XPOST');
			unless ($cc_ins) {
				$n--;
				$cc_ins = qr/^ {0,$n}[\+]\s*(.*)/;
				$cc_del = qr/^ {0,$n}[\-]\s*(.*)/;
			}
		} elsif ($l =~ m!^diff --git (?:"?a/.+?) (?:"?b/.+)!) {
			# regular diff, filenames handled by --raw
			$state = 3;
		} elsif ($l =~ /^diff --(?:cc|combined) (?:.+)/) {
			# combined diff, filenames handled by --raw
			$state = 4;
		} elsif ($l =~ /^@@ (?:\S+) (?:\S+) @@(.*)/) {
			my $hunk_hdr = $1;
			# regular hunk header context
			$hunk_hdr =~ /\S/ and
					index_text_inc($tg, $hunk_hdr, 'XDHH');
		# not currently handled:
		} elsif ($l =~ /^index (?:$hex)\.\.(?:$hex)/o) {
		} elsif ($l =~ /^index (?:$hex,[^\.]+)\.\.(?:$hex)(.*)$/o) {
			#--cc
		} elsif ($l =~ /^(?:@@@+) (?:\S+.*\S+) @@@+\z/) { # --cc
		} elsif ($l =~ /^(?:old|new) mode/) {
		} elsif ($l =~ /^(?:deleted|new) file mode/) {
		} elsif ($l =~ /^tree (?:\S+)/) {
		} elsif ($l =~ /^(?:copy|rename) (?:from|to) /) {
		} elsif ($l =~ /^(?:dis)?similarity index /) {
		} elsif ($l =~ /^\\ No newline at end of file/) {
		} elsif ($l =~ /^Binary files .* differ/) {
		} elsif ($l =~ /^--- /) { # preimage filename
		} elsif ($l =~ /^\+\+\+ /) { # postimage filename
		} elsif ($state == 3) { # diff --git
			if ($l =~ s/^\+//) {
				index_text_inc($tg, $l, 'XDFB');
			} elsif ($l =~ s/^\-//) {
				index_text_inc($tg, $l, 'XDFA');
			} elsif ($l =~ s/^ //) {
				index_text_inc($tg, $l, 'XDCTX');
			} elsif (DEBUG) {
				if ($l eq "\n") {
				} else {
					warn "unhandled diff -u $l";
				}
			}
		} elsif ($state == 4) { # diff --cc/combined
			if ($l =~ $cc_ins) {
				index_text_inc($tg, $1, 'XDFB');
			} elsif ($l =~ $cc_del) {
				index_text_inc($tg, $1, 'XDFA');
			} elsif ($l =~ s/^ //) {
				index_text_inc($tg, $l, 'XDCTX');
			} elsif (DEBUG) {
				if ($l eq "\n") {
				} else {
					warn "unhandled diff --cc $l";
				}
			}
		} elsif (DEBUG) {
			warn  "wtf $state $l\n" if $l ne "\n";
		}
	}
	replace_or_add($db, $doc_id, $doc) if $doc;
	$tip;
}

sub index_top_ref ($$$) {
	my ($self, $refname, $end) = @_;
	my $doc_id;
	my $db = xdb($self);
	my $ref_doc = get_doc($self, \$doc_id, 'ref', $refname);
	my $begin = defined $doc_id ? $ref_doc->get_data : '';
	my $active = $self->{-active_refs} = { $refname => undef };
	my $git = $self->{git};

	# check for discontiguous branches (from "push --force")
	if ($begin ne '') {
		my $base = $git->qx(qw(merge-base), $begin, $end);
		chomp $base;
		if ($base ne $begin) {
			warn "$refname updated with force\n";
			# TODO: cleanup_forced_update($self, $refname);
			$begin = '';
		}
	}
	my $range = $begin eq '' ? $end : "$begin^0..$end^0";
	my $tip = each_log_line($self, $range);
	my $progress = $self->{progress};
	if (defined $tip) {
		$ref_doc->set_data($tip);
		print $progress "$refname => $tip\n" if $progress;
		replace_or_add($db, $doc_id, $ref_doc);
	}

	# update all decorated refs which got snowballed into this one
	delete $active->{$refname};
	foreach my $ref (keys %$active) {
		$ref_doc = get_doc($self, \$doc_id, 'ref', $ref);
		$ref_doc->set_data($active->{$ref});
		if ($progress) {
			print $progress "$ref => $active->{$ref} ($refname)\n";
		}
		replace_or_add($db, $doc_id, $ref_doc);
	}
	$db->flush;
}

# main entry sub:
sub index_sync {
	my ($self, $opts) = @_;
	$self->{progress} = $opts->{progress};
	my $db = xdb($self);
	$self->{-update_id} = update_id($self);
	# go for most recent refs, first, since that reduces the amount
	# of work we have to do.
	my $refs = $self->{git}->popen(qw(for-each-ref --sort=-creatordate));
	local $/ = "\n";
	while (defined(my $line = <$refs>)) {
		chomp $line;
		my ($oid, $type, $refname) = split(/\s+/, $line);
		next unless $refname =~ $self->{want_refs_re};
		next unless $type eq 'commit' || $type eq 'tag';
		index_top_ref($self, $refname, $oid);
	}
}

1;
