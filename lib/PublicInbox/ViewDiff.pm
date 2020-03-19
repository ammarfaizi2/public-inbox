# Copyright (C) 2019-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# used by PublicInbox::View
# This adds CSS spans for diff highlighting.
# It also generates links for ViewVCS + SolverGit to show
# (or reconstruct) blobs.

package PublicInbox::ViewDiff;
use 5.010_001;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(flush_diff);
use URI::Escape qw(uri_escape_utf8);
use PublicInbox::Hval qw(ascii_html to_attr);
use PublicInbox::Git qw(git_unquote);

sub UNSAFE () { "^A-Za-z0-9\-\._~/" }

my $OID_NULL = '0{7,40}';
my $OID_BLOB = '[a-f0-9]{7,40}';
my $LF = qr!\n!;
my $ANY = qr![^\n]!;
my $FN = qr!(?:"?[^/\n]+/[^\n]+|/dev/null)!;

# cf. git diff.c :: get_compact_summary
my $DIFFSTAT_COMMENT = qr/\((?:new|gone|(?:(?:new|mode) [\+\-][lx]))\)/;

# link to line numbers in blobs
sub diff_hunk ($$$$) {
	my ($dst, $dctx, $ca, $cb) = @_;
	my ($oid_a, $oid_b, $spfx) = @$dctx{qw(oid_a oid_b spfx)};

	if (defined($spfx) && defined($oid_a) && defined($oid_b)) {
		my ($n) = ($ca =~ /^-([0-9]+)/);
		$n = defined($n) ? do { ++$n; "#n$n" } : '';

		$$dst .= qq(@@ <a\nhref="$spfx$oid_a/s/$dctx->{Q}$n">$ca</a>);

		($n) = ($cb =~ /^\+([0-9]+)/);
		$n = defined($n) ? do { ++$n; "#n$n" } : '';
		$$dst .= qq( <a\nhref="$spfx$oid_b/s/$dctx->{Q}$n">$cb</a> @@);
	} else {
		$$dst .= "@@ $ca $cb @@";
	}
}

sub oid ($$$) {
	my ($dctx, $spfx, $oid) = @_;
	defined($spfx) ? qq(<a\nhref="$spfx$oid/s/$dctx->{Q}">$oid</a>) : $oid;
}

# returns true if diffstat anchor written, false otherwise
sub anchor0 ($$$$) {
	my ($dst, $ctx, $fn, $rest) = @_;

	my $orig = $fn;

	# normal git diffstat output is impossible to parse reliably
	# without --numstat, and that isn't the default for format-patch.
	# So only do best-effort handling of renames for common cases;
	# which works well in practice. If projects put "=>", or trailing
	# spaces in filenames, oh well :P
	$fn =~ s/(?: *$DIFFSTAT_COMMENT)? *\z//so;
	$fn =~ s/{(?:.+) => (.+)}/$1/ or $fn =~ s/.* => (.+)/$1/;
	$fn = git_unquote($fn);

	# long filenames will require us to walk backwards in anchor1
	if ($fn =~ s!\A\.\.\./?!!) {
		$ctx->{-long_path}->{$fn} = qr/\Q$fn\E\z/s;
	}

	if (my $attr = to_attr($ctx->{-apfx}.$fn)) {
		$ctx->{-anchors}->{$attr} = 1;
		my $spaces = ($orig =~ s/( +)\z//) ? $1 : '';
		$$dst .= " <a\nid=i$attr\nhref=#$attr>" .
			ascii_html($orig) . '</a>' . $spaces .
			$ctx->{-linkify}->to_html($rest);
		return 1;
	}
	undef;
}

# returns "diff --git" anchor destination, undef otherwise
sub anchor1 ($$) {
	my ($ctx, $pb) = @_;
	my $attr = to_attr($ctx->{-apfx}.$pb) or return;

	my $ok = delete $ctx->{-anchors}->{$attr};

	# unlikely, check the end of all long path names we captured:
	unless ($ok) {
		my $lp = $ctx->{-long_path} or return;
		foreach my $fn (keys %$lp) {
			$pb =~ $lp->{$fn} or next;

			delete $lp->{$fn};
			$attr = to_attr($ctx->{-apfx}.$fn) or return;
			$ok = delete $ctx->{-anchors}->{$attr} or return;
			last;
		}
	}
	$ok ? "<a\nhref=#i$attr\nid=$attr>diff</a> --git" : undef
}

sub diff_header ($$$$) {
	my ($dst, $x, $ctx, $top) = @_;
	my (undef, undef, $pa, $pb) = splice(@$top, 0, 4); # ignore oid_{a,b}
	my $spfx = $ctx->{-spfx};
	my $dctx = { spfx => $spfx };

	# get rid of leading "a/" or "b/" (or whatever --{src,dst}-prefix are)
	$pa = (split('/', git_unquote($pa), 2))[1] if $pa ne '/dev/null';
	$pb = (split('/', git_unquote($pb), 2))[1] if $pb ne '/dev/null';
	if ($pa eq $pb && $pb ne '/dev/null') {
		$dctx->{Q} = "?b=".uri_escape_utf8($pb, UNSAFE);
	} else {
		my @q;
		if ($pb ne '/dev/null') {
			push @q, 'b='.uri_escape_utf8($pb, UNSAFE);
		}
		if ($pa ne '/dev/null') {
			push @q, 'a='.uri_escape_utf8($pa, UNSAFE);
		}
		$dctx->{Q} = '?'.join('&amp;', @q);
	}

	# linkify early and all at once, since we know the following
	# subst ops on $$x won't need further escaping:
	$$x = $ctx->{-linkify}->to_html($$x);

	# no need to capture oid_a and oid_b on add/delete,
	# we just linkify OIDs directly via s///e in conditional
	if (($$x =~ s/^(index $OID_NULL\.\.)($OID_BLOB)\b/
			$1 . oid($dctx, $spfx, $2)/emos) ||
		($$x =~ s/^index ($OID_BLOB)(\.\.$OID_NULL)\b/
			'index ' . oid($dctx, $spfx, $1) . $2/emos)) {
	} elsif ($$x =~ /^index ($OID_BLOB)\.\.($OID_BLOB)/mos) {
		# modification-only, not add/delete:
		# linkify hunk headers later using oid_a and oid_b
		@$dctx{qw(oid_a oid_b)} = ($1, $2);
	} else {
		warn "BUG? <$$x> had no ^index line";
	}
	$$x =~ s!^diff --git!anchor1($ctx, $pb) // 'diff --git'!emos;
	$$dst .= qq(<span\nclass="head">);
	$$dst .= $$x;
	$$dst .= '</span>';
	$dctx;
}

sub diff_before_or_after ($$$) {
	my ($dst, $ctx, $x) = @_;
	my $linkify = $ctx->{-linkify};
	for my $y (split(/(^---\n)/sm, $$x)) {
		if ($y =~ /\A---\n\z/s) {
			$$dst .= "---\n"; # all HTML is "\r\n" => "\n"
		} elsif ($y =~ /^ [0-9]+ files? changed, /sm) {
			# ok, looks like a diffstat, go line-by-line:
			for my $l (split(/^/m, $y)) {
				if ($l =~ /^ (.+)( +\| .*\z)/s) {
					anchor0($dst, $ctx, $1, $2) and next;
				}
				$$dst .= $linkify->to_html($l);
			}
		} else { # commit message, notes, etc
			$$dst .= $linkify->to_html($y);
		}
	}
}

# callers must do CRLF => LF conversion before calling this
sub flush_diff ($$$) {
	my ($dst, $ctx, $cur) = @_;

	my @top = split(/(
		(?:	# begin header stuff, don't capture filenames, here,
			# but instead wait for the --- and +++ lines.
			(?:^diff\x20--git\x20$FN\x20$FN$LF)

			# old mode || new mode || copy|rename|deleted|...
			(?:^[a-z]$ANY+$LF)*
		)? # end of optional stuff, everything below is required
		^index\x20($OID_BLOB)\.\.($OID_BLOB)$ANY*$LF
		^---\x20($FN)$LF
		^\+{3}\x20($FN)$LF)/smxo, $$cur);
	$$cur = undef;

	my $linkify = $ctx->{-linkify};
	my $dctx; # {}, keys: Q, oid_a, oid_b

	while (defined(my $x = shift @top)) {
		if (scalar(@top) >= 4 &&
				$top[1] =~ /\A$OID_BLOB\z/os &&
				$top[0] =~ /\A$OID_BLOB\z/os) {
			$dctx = diff_header($dst, \$x, $ctx, \@top);
		} elsif ($dctx) {
			my $after = '';
			for my $s (split(/((?:(?:^\+[^\n]*\n)+)|
					(?:(?:^-[^\n]*\n)+)|
					(?:^@@ [^\n]+\n))/xsm, $x)) {
				if (!defined($dctx)) {
					$after .= $s;
				} elsif ($s =~ s/\A@@ (\S+) (\S+) @@//) {
					$$dst .= qq(<span\nclass="hunk">);
					diff_hunk($dst, $dctx, $1, $2);
					$$dst .= $linkify->to_html($s);
					$$dst .= '</span>';
				} elsif ($s =~ /\A\+/) {
					$$dst .= qq(<span\nclass="add">);
					$$dst .= $linkify->to_html($s);
					$$dst .= '</span>';
				} elsif ($s =~ /\A-- $/sm) { # email sig starts
					$dctx = undef;
					$after .= $s;
				} elsif ($s =~ /\A-/) {
					$$dst .= qq(<span\nclass="del">);
					$$dst .= $linkify->to_html($s);
					$$dst .= '</span>';
				} else {
					$$dst .= $linkify->to_html($s);
				}
			}
			diff_before_or_after($dst, $ctx, \$after) unless $dctx;
		} else {
			diff_before_or_after($dst, $ctx, \$x);
		}
	}
}

1;
