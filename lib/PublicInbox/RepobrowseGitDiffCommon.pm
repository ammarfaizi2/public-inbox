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
	qq(<a\nhref=#D\nid="$anchor">diff</a> --git $html_a $html_b);
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

sub git_diffstat_emit ($$$) {
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

1;
