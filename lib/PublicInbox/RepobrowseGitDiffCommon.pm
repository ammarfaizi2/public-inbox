# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# common git diff-related code
package PublicInbox::RepobrowseGitDiffCommon;
use strict;
use warnings;
use PublicInbox::RepobrowseGit qw/git_unquote git_commit_title/;
use PublicInbox::Hval qw/utf8_html to_attr/;
use base qw/Exporter/;
our @EXPORT_OK = qw/git_diffstat_emit
	git_diff_ab_index git_diff_ab_hdr git_diff_ab_hunk/;
our @EXPORT = qw/git_diff_sed_init git_diff_sed_close git_diff_sed_run
	DSTATE_INIT DSTATE_STAT DSTATE_LINES/;

# index abcdef89..01234567
sub git_diff_ab_index ($$$) {
	my ($xa, $xb, $end) = @_;
	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	$end = utf8_html($end);
	"index $xa..$xb$end";
}

# diff --git a/foo.c b/bar.c
sub git_diff_ab_hdr ($$$) {
	my ($req, $fa, $fb) = @_;
	my $html_a = utf8_html($fa);
	my $html_b = utf8_html($fb);
	$fa = git_unquote($fa);
	$fb = git_unquote($fb);
	$fa =~ s!\Aa/!!;
	$fb =~ s!\Ab/!!;
	my $anchor = to_attr($fb);
	delete $req->{anchors}->{$anchor};
	$fa = $req->{fa} = PublicInbox::Hval->utf8($fa);
	$fb = $req->{fb} = PublicInbox::Hval->utf8($fb);
	$req->{path_a} = $fa->as_path;
	$req->{path_b} = $fb->as_path;

	# not wasting bandwidth on links here
	# links in hunk headers are far more useful with line offsets
	qq(<a\nid="$anchor">diff</a> --git $html_a $html_b);
}

# diff (--cc|--combined)
sub git_diff_cc_hdr {
	my ($req, $combined, $path) = @_;
	my $html_path = utf8_html($path);
	$path = git_unquote($path);
	my $anchor = to_attr($path);
	delete $req->{anchors}->{$anchor};
	my $cc = $req->{cc} = PublicInbox::Hval->utf8($path);
	$req->{path_cc} = $cc->as_path;
	qq(<a\nid="$anchor">diff</a> --$combined $html_path);
}

# @@ -1,2 +3,4 @@ (regular diff)
sub git_diff_ab_hunk ($$$$) {
	my ($req, $ca, $cb, $ctx) = @_;
	my ($na) = ($ca =~ /\A-(\d+)/);
	my ($nb) = ($cb =~ /\A\+(\d+)/);

	# we add "rel=nofollow" here to reduce load on search engines, here
	my $rel = $req->{relcmd};
	my $rv = '@@ ';
	if (defined($na) && $na == 0) { # new file
		$rv .= $ca;
	} else {
		$na = defined $na ? "#n$na" : '';
		my $p = $req->{p}->[0];
		$rv .= qq(<a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$req->{path_a}?id=$p$na">);
		$rv .= "$ca</a>";
	}
	$rv .= ' ';
	if (defined($nb) && $nb == 0) { # deleted file
		$rv .= $cb;
	} else {
		my $h = $req->{h};
		$nb = defined $nb ? "#n$nb" : '';
		$rv .= qq(<a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$req->{path_b}?id=$h$nb">);
		$rv .= "$cb</a>";
	}
	$rv . ' @@' . utf8_html($ctx);
}

# index abcdef09,01234567..76543210
sub git_diff_cc_index {
	my ($req, $before, $last, $end) = @_;
	$end = utf8_html($end);
	my @before = split(',', $before);
	$req->{pobj_cc} = \@before;

	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	"index $before..$last$end";
}

# @@@ -1,2 -3,4 +5,6 @@@ (combined diff)
sub git_diff_cc_hunk {
	my ($req, $at, $offs, $ctx) = @_;
	my @offs = split(' ', $offs);
	my $last = pop @offs;
	my @p = @{$req->{p}};
	my @pobj = @{$req->{pobj_cc}};
	my $path = $req->{path_cc};
	my $rel = $req->{relcmd};
	my $rv = $at;

	# special 'cc' action as we don't have reliable paths from parents
	my $ppath = "${rel}cc/$path";
	foreach my $off (@offs) {
		my $p = shift @p;
		my $obj = shift @pobj; # blob SHA-1
		my ($n) = ($off =~ /\A-(\d+)/); # line number

		if ($n == 0) { # new file (does this happen with --cc?)
			$rv .= " $off";
		} else {
			$rv .= " <a\nhref=\"$ppath?id=$p&obj=$obj#n$n\">";
			$rv .= "$off</a>";
		}
	}

	# we can use the normal 'tree' endpoint for the result
	my ($n) = ($last =~ /\A\+(\d+)/); # line number
	if ($n == 0) { # deleted file (does this happen with --cc?)
		$rv .= " $last";
	} else {
		my $h = $req->{h};
		$rv .= qq( <a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$path?id=$h#n$n">$last</a>);
	}
	$rv .= " $at" . utf8_html($ctx);
}

sub git_diffstat_rename ($$$) {
	my ($req, $from, $to) = @_;
	my $anchor = to_attr(git_unquote($to));
	$req->{anchors}->{$anchor} = $to;
	my @from = split('/', $from);
	my @to = split('/', $to);
	my $orig_to = $to;
	my ($base, @base);
	while (@to && @from && $to[0] eq $from[0]) {
		push @base, shift(@to);
		shift @from;
	}

	$base = utf8_html(join('/', @base)) if @base;
	$from = utf8_html(join('/', @from));
	$to = PublicInbox::Hval->utf8(join('/', @to), $orig_to);
	my $tp = $to->as_path;
	my $th = $to->as_html;
	$to = qq(<a\nhref="#$anchor">$th</a>);
	@base ? "$base/{$from =&gt; $to}" : "$from =&gt; $to";
}

sub git_diffstat_emit ($$$) { # XXX deprecated
	my ($req, $fh, undef) = @_;
	my @stat = split("\0", $_[2]); # avoiding copy for $_[2]
	my $nr = 0;
	my ($nadd, $ndel) = (0, 0);
	my $s = '';
	while (defined(my $l = shift @stat)) {
		$l =~ s/\n?(\S+)\t+(\S+)\t+// or next;
		my ($add, $del) = ($1, $2);
		if ($add =~ /\A\d+\z/) {
			$nadd += $add;
			$ndel += $del;
			$add = "+$add";
			$del = "-$del";
		}
		my $num = sprintf('% 6s/%-6s', $del, $add);
		if (length $l) {
			my $anchor = to_attr(git_unquote($l));
			$req->{anchors}->{$anchor} = $l;
			$l = utf8_html($l);
			$l = qq(<a\nhref="#$anchor">$l</a>);
		} else {
			my $from = shift @stat;
			my $to = shift @stat;
			$l = git_diffstat_rename($req, $from, $to);
		}
		++$nr;
		$s .= ' '.$num."\t".$l."\n";
	}
	$s .= "\n $nr ";
	$s .= $nr == 1 ? 'file changed, ' : 'files changed, ';
	$s .= $nadd;
	$s .= $nadd == 1 ? ' insertion(+), ' : ' insertions(+), ';
	$s .= $ndel;
	$s .= $ndel == 1 ? " deletion(-)\n\n" : " deletions(-)\n\n";
	$fh->write($s);
}

sub DSTATE_INIT () { 0 }
sub DSTATE_STAT () { 1 }
sub DSTATE_LINES () { 2 }

sub git_diff_sed_init ($) {
	my ($req) = @_;
	$req->{dbuf} = '';
	$req->{ndiff} = $req->{nchg} = $req->{nadd} = $req->{ndel} = 0;
	$req->{dstate} = DSTATE_INIT;
}

sub git_diff_sed_stat ($$) {
	my ($dst, $req) = @_;
	my @stat = split(/\0/, $req->{dbuf}, -1);
	my $eos;
	my $nchg = \($req->{nchg});
	my $nadd = \($req->{nadd});
	my $ndel = \($req->{ndel});
	if (!$req->{dstat_started}) {
		$req->{dstat_started} = 1;
		if ($req->{mhelp}) {
			if ($stat[0] eq '') {
				shift @stat;
			} else {
				warn
'initial merge diffstat line was not empty';
			}
		} else {
			$stat[0] =~ s/\A\n//s or warn
'failed to remove initial newline from diffstat';
		}
	}
	while (defined(my $l = shift @stat)) {
		if ($l eq '') {
			$eos = 1 if $stat[0] && $stat[0] =~ /\Ad/; # "diff --"
			last;
		} elsif ($l =~ /\Adiff /) {
			unshift @stat, $l;
			$eos = 1;
			last;
		}
		$l =~ /\A(\S+)\t+(\S+)\t+(.*)/ or next;
		my ($add, $del, $fn) = ($1, $2, $3);
		if ($fn ne '') { # normal modification
			# TODO: discard diffs if they are too big
			# gigantic changes with many files may still OOM us
			my $anchor = to_attr(git_unquote($fn));
			$req->{anchors}->{$anchor} = $fn;
			$l = utf8_html($fn);
			$l = qq(<a\nhref="#$anchor">$l</a>);
		} else { # rename
			# incomplete...
			if (scalar(@stat) < 2) {
				unshift @stat, $l;
				last;
			}
			my $from = shift @stat;
			my $to = shift @stat;
			$l = git_diffstat_rename($req, $from, $to);
		}

		# text changes show numerically, Binary does not
		if ($add =~ /\A\d+\z/) {
			$$nadd += $add;
			$$ndel += $del;
			$add = "+$add";
			$del = "-$del";
		}
		++$$nchg;
		my $num = sprintf('% 6s/%-6s', $del, $add);
		$$dst .= " $num\t$l\n";
	}

	$req->{dbuf} = join("\0", @stat);
	return unless $eos;

	$req->{dstate} = DSTATE_LINES;
	$$dst .= "\n $$nchg ";
	$$dst .= $$nchg  == 1 ? 'file changed, ' : 'files changed, ';
	$$dst .= $$nadd;
	$$dst .= $$nadd == 1 ? ' insertion(+), ' : ' insertions(+), ';
	$$dst .= $$ndel;
	$$dst .= $$ndel == 1 ? " deletion(-)\n\n" : " deletions(-)\n\n";
}

sub git_diff_sed_lines ($$) {
	my ($dst, $req) = @_;

	# TODO: discard diffs if they are too big

	my @dlines = split(/\n/, $req->{dbuf}, -1);
	$req->{dbuf} = '';

	if (my $help = delete $req->{mhelp}) {
		$$dst .= $help; # CC_MERGE
	}

	# don't touch the last line, it may not be terminated
	$req->{dbuf} .= pop @dlines;

	my $ndiff = \($req->{ndiff});
	my $cmt = '[a-f0-9]+';
	while (defined(my $l = shift @dlines)) {
		if ($l =~ m{\Adiff --git ("?a/.+) ("?b/.+)\z}) { # regular
			$$dst .= git_diff_ab_hdr($req, $1, $2) . "\n";
		} elsif ($l =~ m{\Adiff --(cc|combined) (.+)\z}) {
			$$dst .= git_diff_cc_hdr($req, $1, $2) . "\n";
		} elsif ($l =~ /\Aindex ($cmt)\.\.($cmt)(.*)\z/o) { # regular
			$$dst .= git_diff_ab_index($1, $2, $3) . "\n";
		} elsif ($l =~ /\A@@ (\S+) (\S+) @@(.*)\z/) { # regular
			$$dst .= git_diff_ab_hunk($req, $1, $2, $3) . "\n";
		} elsif ($l =~ /\Aindex ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) {  #--cc
			$$dst .= git_diff_cc_index($req, $1, $2, $3) . "\n";
		} elsif ($l =~ /\A(@@@+) (\S+.*\S+) @@@+(.*)\z/) { # --cc
			$$dst .= git_diff_cc_hunk($req, $1, $2, $3) . "\n";
		} else {
			$$dst .= utf8_html($l) . "\n";
		}
		++$$ndiff;
	}
}

sub git_diff_sed_run ($$) {
	my ($dst, $req) = @_;
	$req->{dstate} == DSTATE_STAT and git_diff_sed_stat($dst, $req);
	$req->{dstate} == DSTATE_LINES and git_diff_sed_lines($dst, $req);
	undef;
}

sub git_diff_sed_close ($$) {
	my ($dst, $req) = @_;
	$$dst .= utf8_html(delete $req->{dbuf});
	undef;
}

1;
