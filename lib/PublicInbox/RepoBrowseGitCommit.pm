# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::RepoBrowseGitCommit;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);
use PublicInbox::Git;
use PublicInbox::RepoBrowseGit qw(git_unquote);

use constant GIT_FMT => '--pretty=format:'.join('%n',
	'%H', '%h', '%s', '%an <%ae>', '%ai', '%cn <%ce>', '%ci',
	'%t', '%p', '%D', '%b%x00');

*ascii_html = *PublicInbox::Hval::ascii_html;

sub git_commit_stream {
	my ($req, $q, $H, $log, $fh) = @_;
	chomp(my $h = <$log>); # abbreviated commit
	my $l;
	my $s = PublicInbox::Hval->new_oneline($l = <$log>)->as_html; # subject
	my $au = PublicInbox::Hval->new_oneline($l = <$log>)->as_html; # author
	chomp(my $ad = <$log>);
	my $cu = PublicInbox::Hval->new_oneline($l = <$log>)->as_html;
	chomp(my $cd = <$log>);
	chomp(my $t = <$log>); # tree
	chomp(my $p = <$log>); # parents
	my @p = split(' ', $p);
	chomp(my $D = <$log>); # TODO: decorate

	my $rel = $req->{relcmd};
	my $x = "<html><head><title>$s</title></head><body>" .
		PublicInbox::Hval::PRE .
		"   commit $H" .
		"   author $au\t$ad\n" .
		"committer $cu\t$cd\n" .
		"     tree <a\nhref=\"${rel}tree?id=$t\">$t</a>\n";
	if (scalar(@p) == 1) {
		$x .= '   parent ';
		$x .= qq(<a\nhref="${rel}commit?id=$p[0]">$p[0]</a>\n);
	} elsif (scalar(@p) > 1) {
		foreach my $p (@p) {
			$x .= '    merge ';
			$x .= "<a\nhref=\"${rel}commit?id=$p\">$p</a> (";
			$x .= "<a\nhref=\"${rel}diff?id=$p&id2=$h\">";
			$x .= "diff</a>)\n";
		}
	}
	$fh->write($x .= "\n$s\n\n");

	# body:
	local $/ = "\0";
	$l = <$log>;
	chomp $l;
	$x = PublicInbox::Hval->new_bin($l)->as_html; # body
	$fh->write($x."\n");

	git_show_diffstat($fh, $log);

	# diff
	local $/ = "\n";
	my $cmt = '[a-f0-9]+';
	my $diff = { h => $h, p => \@p, rel => $rel };
	while (defined($l = <$log>)) {
		if ($l =~ m{^diff --git ("?a/.+) ("?b/.+)$}) { # regular
			$l = git_diff_ab_hdr($diff, $1, $2) . "\n";
		} elsif ($l =~ m{^diff --cc (.+)$}) { # --cc
			$l = git_diff_cc_hdr($diff, $1) . "\n";
		} elsif ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
			$l = git_diff_ab_index($diff, $1, $2, $3) . "\n";
		} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
			$l = git_diff_ab_hunk($diff, $1, $2, $3) . "\n";
		} elsif ($l =~ /^\+/) { # added hunk
			chomp $l;
			$l = '<b>'.PublicInbox::Hval->new_bin($l)->as_html.
				"</b>\n";
		} elsif ($l =~ /^index ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) { # --cc
			$l = git_diff_cc_index($diff, $1, $2, $3) . "\n";
		} elsif ($l =~ /^(@@@+) (\S+.*\S+) @@@+(.*)$/) { # --cc
			$l = git_diff_cc_hunk($diff, $1, $2, $3) . "\n";
		} else {
			$l = PublicInbox::Hval->new_bin($l)->as_html;
		}
		$fh->write($l);
	}
	$fh->write('</pre></body></html>');
}

sub call_git_commit {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $path = $repo_info->{path};

	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';
	my $git = $repo_info->{git} ||= PublicInbox::Git->new($path);
	my $log = $git->popen(qw(show -z --numstat -p --no-notes --no-color
				 --abbrev=16),
				 GIT_FMT, $id);
	my $H = <$log>;
	defined $H or return;
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type'=>'text/html']]);
		git_commit_stream($req, $q, $H, $log, $fh);
		$fh->close;
	}
}

sub git_blob_hrefs {
	my ($rel, @ids) = @_;
	map { "<a\nhref=\"${rel}tree?id=$_\"" } @ids;
}

sub git_blob_links {
	my ($hrefs, $labels) = @_;
	my $i = 0;
	map { $hrefs->[$i++].">$_</a>" } @$labels;
}

sub git_show_diffstat {
	my ($fh, $log) = @_;
	local $/ = "\0\0";
	my $l = <$log>;
	chomp $l;
	my @stat = split("\0", $l);
	my $nr = 0;
	my ($nadd, $ndel) = (0, 0);
	while (defined($l = shift @stat)) {
		$l =~ s/\n?(\S+)\t+(\S+)\t+// or next;
		my ($add, $del) = ($1, $2);
		if ($add =~ /\A\d+\z/) {
			$nadd += $add;
			$ndel += $del;
			$add = "+$add";
			$del = "-$del";
		}
		my $num = sprintf('% 6s/%-6s', $del, $add);
		unless (length $l) {
			my $from = shift @stat;
			my $to = shift @stat;
			$l = "$from => $to";
		}
		$l = PublicInbox::Hval->new_bin($l)->as_html;
		++$nr;
		$fh->write($num."\t".$l."\n");
	}
	$l = "\n$nr ";
	$l .= $nr == 1 ? 'file changed, ' : 'files changed, ';
	$l .= $nadd;
	$l .= $nadd == 1 ? ' insertion(+), ' : ' insertions(+), ';
	$l .= $ndel;
	$l .= $ndel == 1 ? " deletion(-)\n\n" : " deletions(-)\n\n";
	$fh->write($l);
}

# index abcdef89..01234567
sub git_diff_ab_index {
	my ($diff, $xa, $xb, $end) = @_;
	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	$end = PublicInbox::Hval->new_bin($end)->as_html;
	"index $xa..<b>$xb</b>$end";
}

# diff --git a/foo.c b/bar.c
sub git_diff_ab_hdr {
	my ($diff, $fa, $fb) = @_;
	my $html_a = PublicInbox::Hval->new_bin($fa)->as_html;
	my $html_b = PublicInbox::Hval->new_bin($fb)->as_html;
	$fa = git_unquote($fa);
	$fb = git_unquote($fb);
	$fa =~ s!\Aa/!!;
	$fb =~ s!\Ab/!!;
	$fa = $diff->{fa} = PublicInbox::Hval->new_bin($fa);
	$fb = $diff->{fb} = PublicInbox::Hval->new_bin($fb);
	$diff->{path_a} = $fa->as_path;
	$diff->{path_b} = $fb->as_path;

	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	"diff --git $html_a <b>$html_b</b>";
}

# @@ -1,2 +3,4 @@ (regular diff)
sub git_diff_ab_hunk {
	my ($diff, $ca, $cb, $ctx) = @_;
	my ($na) = ($ca =~ /\A-(\d+),/);
	my ($nb) = ($cb =~ /\A\+(\d+),/);

	my $rel = $diff->{rel};
	my $p = $diff->{p}->[0];
	my $h = $diff->{h};

	'@@ ' .
	"<a\nhref=\"${rel}tree/$diff->{path_a}?id=$p#n$na\">$ca</a> " .
	"<a\nhref=\"${rel}tree/$diff->{path_b}?id=$h#n$nb\"><b>$cb</b></a> " .
	'@@' . PublicInbox::Hval->new_bin($ctx)->as_html;
}

sub git_diff_cc_hdr {
	my ($diff, $path) = @_;
	my $html_path = PublicInbox::Hval->new_bin($path)->as_html;
	my $cc = $diff->{cc} = PublicInbox::Hval->new_bin(git_unquote($path));
	$diff->{path_cc} = $cc->as_path;
	"diff --cc <b>$html_path</b>";
}

# index abcdef09,01234567..76543210
sub git_diff_cc_index {
	my ($diff, $before, $last, $end) = @_;
	$end = PublicInbox::Hval->new_bin($end)->as_html;
	$diff->{pobj_cc} = [ split(',', $before) ];

	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	"index $before..<b>$last</b>$end";
}

# @@@ -1,2 -3,4 +5,6 @@@ (combined diff)
sub git_diff_cc_hunk {
	my ($diff, $at, $offs, $ctx) = @_;
	my @offs = split(' ', $offs);
	my $last = pop @offs;
	my @p = @{$diff->{p}};
	my @pobj = @{$diff->{pobj_cc}};
	my $path = $diff->{path_cc};
	my $rel = $diff->{rel};
	my $rv = $at;

	# special 'cc' action as we don't have reliable paths from parents
	my $ppath = "${rel}cc/$path";
	foreach my $off (@offs) {
		my $p = shift @p;
		my $obj = shift @pobj; # blob SHA-1
		my ($n) = ($off =~ /\A-(\d+),/); # line number
		$rv .= " <a\nhref=\"$ppath?id=$p&obj=$obj#n$n\">$off</a>";
	}

	# we can use the normal 'tree' endpoint for the result
	my ($n) = ($last =~ /\A\+(\d+),/); # line number
	my $h = $diff->{h};
	$rv .= " <a\nhref=\"${rel}tree/$path?id=$h#n$n\"><b>$last</b></a>";
	$rv .= " $at" . PublicInbox::Hval->new_bin($ctx)->as_html;
}

1;
