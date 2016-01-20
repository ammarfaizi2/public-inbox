# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /commit/ endpoint for git repositories
#
# anchors used:
#	D - diffstat
#	P - parents
#	...and various filenames from to_attr
# The 'D' and 'P' anchors may conflict with odd filenames, but we won't
# punish the common case with extra bytes if somebody uses 'D' or 'P'
# in filenames.

package PublicInbox::RepobrowseGitCommit;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::Hval qw(utf8_html to_attr);
use PublicInbox::RepobrowseGit qw(git_unquote git_commit_title);

use constant GIT_FMT => '--pretty=format:'.join('%n',
	'%H', '%h', '%s', '%an <%ae>', '%ai', '%cn <%ce>', '%ci',
	'%t', '%p', '%D', '%b%x00');

sub git_commit_stream {
	my ($self, $req, $q, $H, $log, $fh) = @_;
	chomp(my $h = <$log>); # abbreviated commit
	my $l;
	chomp(my $s = utf8_html($l = <$log>)); # subject
	chomp(my $au = utf8_html($l = <$log>)); # author
	chomp(my $ad = <$log>);
	chomp(my $cu = utf8_html($l = <$log>));
	chomp(my $cd = <$log>);
	chomp(my $t = <$log>); # tree
	chomp(my $p = <$log>); # parents
	my @p = split(' ', $p);
	chomp(my $D = <$log>); # TODO: decorate
	my $git = $req->{repo_info}->{git};

	my $rel = $req->{relcmd};
	my $qs = $q->qs(id => $h);
	chomp $H;
	my $x = $self->html_start($req, $s) . "\n" .
		qq(   commit $H (<a\nhref="${rel}patch$qs">patch</a>)\n) .
		qq(     tree <a\nrel=nofollow\nhref="${rel}tree?id=$h">$t</a>);

	# extra show path information, if any
	my $extra = $req->{extra};
	my $path = '';
	if (@$extra) {
		my @t;
		my $ep;
		$x .= ' -- ';
		$x .= join('/', map {
			push @t, $_;
			my $e = PublicInbox::Hval->utf8($_, join('/', @t));
			$ep = $e->as_path;
			my $eh = $e->as_html;
			$ep = "${rel}tree/$ep?id=$h";
			qq(<a\nrel=nofollow\nhref="$ep">$eh</a>);
		} @$extra);
		$path = "/$ep";
	}

	$x .= "\n   author $au\t$ad\ncommitter $cu\t$cd\n";
	my $np = scalar @p;
	if ($np == 1) {
		my $p = $p[0];
		$x .= git_parent_line('   parent', $p, $q, $git, $rel, $path);
	} elsif ($np > 1) {
		my @common = ($q, $git, $rel, $path);
		my @t = @p;
		my $p = shift @t;
		$x .= git_parent_line('  parents', $p, @common);
		foreach $p (@t) {
			$x .= git_parent_line('         ', $p, @common);
		}
	}
	$fh->write($x .= "\n<b>$s</b>\n\n");

	# body:
	local $/ = "\0";
	$l = <$log>;
	chomp $l;
	$fh->write(utf8_html($l)."<a\nid=D>---</a>\n");
	my $diff = { anchors => {}, h => $h, p => \@p, rel => $rel };
	git_show_diffstat($diff, $req, $h, $fh, $log);
	my $help;
	$help = " This is a merge, showing combined diff:\n\n" if ($np > 1);

	# diff
	local $/ = "\n";
	my $cmt = '[a-f0-9]+';
	my ($cc_ins, $cc_del);
	while (defined($l = <$log>)) {
		if ($help) {
			$fh->write($help);
			$help = undef;
		}
		if ($l =~ m{^diff --git ("?a/.+) ("?b/.+)$}) { # regular
			$l = git_diff_ab_hdr($diff, $1, $2) . "\n";
		} elsif ($l =~ m{^diff --(cc|combined) (.+)$}) {
			$l = git_diff_cc_hdr($diff, $1, $2) . "\n";
		} elsif ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
			$l = git_diff_ab_index($diff, $1, $2, $3) . "\n";
		} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
			$l = git_diff_ab_hunk($diff, $1, $2, $3) . "\n";

		} elsif ($l =~ /^\+{1,3}\s*/ || ($cc_ins && $l =~ $cc_ins)) {
			$l = git_diff_ins($l) . "\n";
		} elsif ($l =~ s/^(\-{1,3}\s*)// ||
					($cc_del && $l =~ s/$cc_del//)) {
			$l = git_diff_del($1, $l) . "\n";
		} elsif ($l =~ /^index ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) { # --cc
			$l = git_diff_cc_index($diff, $1, $2, $3) . "\n";
			$cc_ins ||= $diff->{cc_ins};
			$cc_del ||= $diff->{cc_del};
		} elsif ($l =~ /^(@@@+) (\S+.*\S+) @@@+(.*)$/) { # --cc
			$l = git_diff_cc_hunk($diff, $1, $2, $3) . "\n";
		} else {
			$l = utf8_html($l);
		}
		$fh->write($l);
	}

	if ($help) {
		$fh->write(" This is a merge, combined diff is empty.\n");
	}

	show_unchanged($fh, $diff, $qs);
	$fh->write('</pre></body></html>');
}

sub call_git_commit {
	my ($self, $req) = @_;

	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';

	my $expath = $req->{expath};
	if ($expath ne '') {
		my $relup = join('', map { '../' } @{$req->{extra}});
		my $qs = $q->qs;
		return $self->r(301, $req, "$relup$qs#".to_attr($expath));
	}

	my $git = $req->{repo_info}->{git};
	my @cmd = (qw(show -z --numstat -p --encoding=UTF-8
			--no-notes --no-color -c), $git->abbrev);

	my $log = $git->popen(@cmd, GIT_FMT, $id, '--');
	my $H = <$log>;

	# maybe the path didn't exist, yet, zip them back up
	return git_commit_404($req, $q) unless defined $H;
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type'=>'text/html']]);
		git_commit_stream($self, $req, $q, $H, $log, $fh);
		$fh->close;
	}
}

sub git_commit_404 {
	my ($req, $q) = @_;
	my $x = 'Missing commit or path';
	my $pfx = "$req->{relcmd}commit";

	my $try = 'try';
	$x = "<html><head><title>$x</title></head><body><pre><b>$x</b>\n\n";
	my $qs = $q->qs(id => '');
	$x .= "<a\nhref=\"$pfx$qs\">$try the latest commit in HEAD</a>\n";
	$x .= '</pre></body>';

	[ 404, ['Content-Type'=>'text/html'], [ $x ] ];
}

sub git_show_diffstat {
	my ($diff, $req, $h, $fh, $log) = @_;
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
		if (length $l) {
			my $anchor = to_attr(git_unquote($l));
			$diff->{anchors}->{$anchor} = $l;
			$l = utf8_html($l);
			$l = qq(<a\nhref="#$anchor">$l</a>);
		} else {
			my $from = shift @stat;
			my $to = shift @stat;
			$l = git_diffstat_rename($diff, $h, $from, $to);
		}
		++$nr;
		$fh->write(' '.$num."\t".$l."\n");
	}
	$l = "\n $nr ";
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
	$end = utf8_html($end);
	"index $xa..<b>$xb</b>$end";
}

# diff --git a/foo.c b/bar.c
sub git_diff_ab_hdr {
	my ($diff, $fa, $fb) = @_;
	my $html_a = utf8_html($fa);
	my $html_b = utf8_html($fb);
	$fa = git_unquote($fa);
	$fb = git_unquote($fb);
	$fa =~ s!\Aa/!!;
	$fb =~ s!\Ab/!!;
	my $anchor = to_attr($fb);
	delete $diff->{anchors}->{$anchor};
	$fa = $diff->{fa} = PublicInbox::Hval->utf8($fa);
	$fb = $diff->{fb} = PublicInbox::Hval->utf8($fb);
	$diff->{path_a} = $fa->as_path;
	$diff->{path_b} = $fb->as_path;

	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	qq(<a\nhref=#D\nid="$anchor">diff</a> --git $html_a <b>$html_b</b>);
}

# @@ -1,2 +3,4 @@ (regular diff)
sub git_diff_ab_hunk {
	my ($diff, $ca, $cb, $ctx) = @_;
	my ($na) = ($ca =~ /\A-(\d+)/);
	my ($nb) = ($cb =~ /\A\+(\d+)/);

	my $rel = $diff->{rel};
	my $rv = '@@ ';
	if ($na == 0) { # new file
		$rv .= $ca;
	} else {
		my $p = $diff->{p}->[0];
		$rv .= qq(<a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$diff->{path_a}?id=$p#n$na">);
		$rv .= "$ca</a>";
	}
	$rv .= ' ';
	if ($nb == 0) { # deleted file
		$rv .= $cb;
	} else {
		my $h = $diff->{h};
		$rv .= qq(<a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$diff->{path_b}?id=$h#n$nb">);
		$rv .= "<b>$cb</b></a>";
	}
	$rv . ' @@' . utf8_html($ctx);
}

sub git_diff_cc_hdr {
	my ($diff, $combined, $path) = @_;
	my $html_path = utf8_html($path);
	$path = git_unquote($path);
	my $anchor = to_attr($path);
	delete $diff->{anchors}->{$anchor};
	my $cc = $diff->{cc} = PublicInbox::Hval->utf8($path);
	$diff->{path_cc} = $cc->as_path;
	qq(<a\nhref=#D\nid="$anchor">diff</a> --$combined <b>$html_path</b>);
}

# index abcdef09,01234567..76543210
sub git_diff_cc_index {
	my ($diff, $before, $last, $end) = @_;
	$end = utf8_html($end);
	my @before = split(',', $before);
	$diff->{pobj_cc} = \@before;
	unless ($diff->{cc_ins}) {
		my $n = scalar(@before) - 1;
		$diff->{cc_ins} = qr/^ {0,$n}[\+]\s*/;
		$diff->{cc_del} = qr/^( {0,$n}[\-]\s*)/;
	}

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
		$rv .= " <b>$last</b>";
	} else {
		my $h = $diff->{h};
		$rv .= qq( <a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$path?id=$h#n$n">);
		$rv .= "<b>$last</b></a>";
	}
	$rv .= " $at" . utf8_html($ctx);
}

sub git_diffstat_rename {
	my ($diff, $h, $from, $to) = @_;
	my $anchor = to_attr(git_unquote($to));
	$diff->{anchors}->{$anchor} = $to;
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

# It would be nice to be able to use colors for showing diff hunks.
# Unfortunately, the default green+red colors in common web viewers
# (gitweb, cgit, etc) are difficult to read for some people, myself
# included.
#
# We cannot rely on CSS styling since it is unsafe and incompatible with
# some browsers.
#
# Tried using semantic tags (ins and del).  Unfortunately, alignment
# gets completely screwed in text-only browsers.  On common GUI browsers,
# <del> renders unreadably by default (strike-throughs) and <ins> is
# visually too noisy (with underlines).  So we'll bold added lines and
# leave deleted lines alone (prefixed with '-')
sub git_diff_ins {
	my ($l) = @_;
	chomp $l;
	'<b>'.utf8_html($l).'</b>';
}

sub git_diff_del {
	my ($pfx, $l) = @_;
	chomp $l;
	# underline fallbacks look ugly with leading whitespace
	$pfx. '<i>'.utf8_html($l).'</i>';
}

sub git_parent_line {
	my ($pfx, $p, $q, $git, $rel, $path) = @_;
	my $qs = $q->qs(id => $p);
	my $t = git_commit_title($git, $p);
	$t = defined $t ? utf8_html($t) : '';
	$pfx . qq( <a\nid=P\nhref="${rel}commit$path$qs">$p</a> $t\n);
}

# do not break anchor links if the combined diff doesn't show changes:
sub show_unchanged {
	my ($fh, $diff, $qs) = @_;

	my @unchanged = sort keys %{$diff->{anchors}};
	return unless @unchanged;
	my $anchors = $diff->{anchors};
	my $s = "\n There are uninteresting changes from this merge.\n" .
		qq( See <a\nhref="#P">parents</a>, ) .
		"or view final state(s) below:\n";
	my $rel = $diff->{rel};
	foreach my $anchor (@unchanged) {
		my $fn = $anchors->{$anchor};
		my $p = PublicInbox::Hval->utf8(git_unquote($fn));
		$p = $p->as_path;
		$fn = utf8_html($fn);
		$s .= qq(\t<a\nrel=nofollow);
		$s .= qq(\nid="$anchor"\nhref="${rel}tree/$p$qs">);
		$s .= "$fn</a>\n";
	}
	$fh->write($s);
}

1;
