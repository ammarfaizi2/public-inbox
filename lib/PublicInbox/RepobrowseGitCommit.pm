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
use PublicInbox::RepobrowseGitDiffCommon qw/git_diffstat_emit
	git_diff_ab_index git_diff_ab_hdr git_diff_ab_hunk/;

use constant GIT_FMT => '--pretty=format:'.join('%n',
	'%H', '%h', '%s', '%an <%ae>', '%ai', '%cn <%ce>', '%ci',
	'%t', '%p', '%D', '%b%x00');

use constant CC_EMPTY => " This is a merge, and the combined diff is empty.\n";
use constant CC_MERGE => " This is a merge, showing combined diff:\n\n";

sub commit_header {
	my ($self, $req) = @_;
	my $res = delete $req->{res} or die "BUG: missing res\n";
	my ($H, $h, $s, $au, $ad, $cu, $cd, $t, $p, $D, $rest) =
		split("\n", $req->{dbuf}, 11);
	$s = utf8_html($s);
	$au = utf8_html($au);
	$cu = utf8_html($cu);
	my @p = split(' ', $p);
	my $fh = $req->{fh} = $res->([200, ['Content-Type'=>'text/html']]);

	my $rel = $req->{relcmd};
	my $q = $req->{'q'};
	my $qs = $req->{qs} = $q->qs(id => $h);
	my $x = $self->html_start($req, $s) . "\n" .
		qq(   commit $H (<a\nhref="${rel}patch$qs">patch</a>)\n) .
		qq(     tree <a\nrel=nofollow\nhref="${rel}tree?id=$h">$t</a>);

	my $git = $req->{repo_info}->{git};
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
		$req->{help} = CC_MERGE;
		my @common = ($q, $git, $rel, $path);
		my @t = @p;
		my $p = shift @t;
		$x .= git_parent_line('  parents', $p, @common);
		foreach $p (@t) {
			$x .= git_parent_line('         ', $p, @common);
		}
	}
	$x .= "\n<b>";
	$x .= $s;
	$x .= "</b>\n\n";
	my $bx00;
	($bx00, $req->{dbuf}) = split("\0", $rest, 2);
	$fh->write($x .= utf8_html($bx00) . "<a\nid=D>---</a>\n");
	$req->{anchors} = {};
	$req->{h} = $h;
	$req->{p} = \@p;
}

sub git_diff_cc_line_i ($$) {
	my ($req, $l) = @_;
	my $cmt = '[a-f0-9]+';

	if ($l =~ m{^diff --git ("?a/.+) ("?b/.+)$}) { # regular
		git_diff_ab_hdr($req, $1, $2) . "\n";
	} elsif ($l =~ m{^diff --(cc|combined) (.+)$}) {
		git_diff_cc_hdr($req, $1, $2) . "\n";
	} elsif ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
		git_diff_ab_index($1, $2, $3) . "\n";
	} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
		git_diff_ab_hunk($req, $1, $2, $3) . "\n";
	} elsif ($l =~ /^index ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) { # --cc
		git_diff_cc_index($req, $1, $2, $3) . "\n";
	} elsif ($l =~ /^(@@@+) (\S+.*\S+) @@@+(.*)$/) { # --cc
		git_diff_cc_hunk($req, $1, $2, $3) . "\n";
	} else {
		utf8_html($l) . "\n";
	}
}

sub git_commit_stream ($$$$) {
	my ($self, $req, $fail, $end) = @_;
	my $dbuf = \($req->{dbuf});
	my $off = length($$dbuf);
	my $n = $req->{rpipe}->sysread($$dbuf, 8192, $off);
	return $fail->() unless defined $n;
	return $end->() if $n == 0;
	my $res = $req->{res};
	if ($res) {
		return if index($$dbuf, "\0") < 0;
		commit_header($self, $req);
		return if $$dbuf eq '';
	}
	my $fh = $req->{fh};
	if (!$req->{diff_state}) {
		my ($stat, $buf) = split("\0\0", $$dbuf, 2);
		return unless defined $buf;
		$$dbuf = $buf;
		git_diffstat_emit($req, $fh, $stat);
		$req->{diff_state} = 1;
	}
	my @buf = split("\n", $$dbuf, -1);
	$$dbuf = pop @buf; # last line, careful...
	if (@buf) {
		my $s = delete($req->{help}) || '';
		$s .= git_diff_cc_line_i($req, $_) foreach @buf;
		$fh->write($s) if $s ne '';
	}
}

sub call_git_commit {
	my ($self, $req) = @_;

	my $q = PublicInbox::RepobrowseGitQuery->new($req->{env});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';

	my $expath = $req->{expath};
	if ($expath ne '') {
		my $relup = join('', map { '../' } @{$req->{extra}});
		my $qs = $q->qs;
		return $self->r(301, $req, "$relup$qs#".to_attr($expath));
	}

	my $git = $req->{repo_info}->{git};
	my $cmd = [ qw(show -z --numstat -p --encoding=UTF-8
			--no-notes --no-color -c),
			$git->abbrev, GIT_FMT, $id, '--' ];
	$req->{rpipe} = $git->popen($cmd, undef, { 2 => $git->err_begin });
	my $env = $req->{env};
	my $err = $env->{'psgi.errors'};
	my $vin;
	$req->{dbuf} = '';
	my $end = sub {
		if (my $fh = delete $req->{fh}) {
			my $dbuf = delete $req->{dbuf};
			if (!$req->{diff_state}) {
				my ($stat, $buf) = split("\0\0", $dbuf, 2);
				$dbuf = defined $buf ? $buf : '';
				git_diffstat_emit($req, $fh, $stat);
				$req->{diff_state} = 1;
			}
			my @buf = split("\n", $dbuf, -1);
			if (@buf) {
				my $s = delete($req->{help}) || '';
				$s .= git_diff_cc_line_i($req, $_) foreach @buf;
				$fh->write($s) if $s ne '';
			}
			$fh->write(CC_EMPTY) if delete($req->{help});
			show_unchanged($req, $fh);
			$fh->write('</pre></body></html>');
			$fh->close;
		} elsif (my $res = delete $req->{res}) {
			git_commit_404($req, $res);
		}
		if (my $rpipe = delete $req->{rpipe}) {
			$rpipe->close; # _may_ be Danga::Socket::close
		}
		# zero the error file for now, be careful about printing
		# $id to psgi.errors w/o sanitizing...
		$git->err;
	};
	my $fail = sub {
		if ($!{EAGAIN} || $!{EINTR}) {
			select($vin, undef, undef, undef) if defined $vin;
			# $vin is undef on async, so this is a noop on EAGAIN
			return;
		}
		my $e = $!;
		$end->();
		$err->print("git show ($git->{git_dir}): $e\n");
	};
	$req->{'q'} = $q;
	my $cb = sub { # read git-show output and stream to client
		git_commit_stream($self, $req, $fail, $end);
	};
	if (my $async = $env->{'pi-httpd.async'}) {
		$req->{rpipe} = $async->($req->{rpipe}, $cb);
		sub { $req->{res} = $_[0] } # let Danga::Socket handle the rest
	} else { # synchronous loop for other PSGI servers
		$vin = '';
		vec($vin, fileno($req->{rpipe}), 1) = 1;
		sub {
			$req->{res} = $_[0];
			while ($req->{rpipe}) { $cb->() }
		}
	}
}

sub git_commit_404 {
	my ($req, $res) = @_;
	my $x = 'Missing commit or path';
	my $pfx = "$req->{relcmd}commit";

	my $try = 'try';
	$x = "<html><head><title>$x</title></head><body><pre><b>$x</b>\n\n";
	my $qs = $req->{'q'}->qs(id => '');
	$x .= "<a\nhref=\"$pfx$qs\">$try the latest commit in HEAD</a>\n";
	$x .= '</pre></body>';

	$res->([404, ['Content-Type'=>'text/html'], [ $x ]]);
}

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

sub git_parent_line {
	my ($pfx, $p, $q, $git, $rel, $path) = @_;
	my $qs = $q->qs(id => $p);
	my $t = git_commit_title($git, $p);
	$t = defined $t ? utf8_html($t) : '';
	$pfx . qq( <a\nid=P\nhref="${rel}commit$path$qs">$p</a> $t\n);
}

# do not break anchor links if the combined diff doesn't show changes:
sub show_unchanged {
	my ($req, $fh) = @_;

	my @unchanged = sort keys %{$req->{anchors}};
	return unless @unchanged;
	my $anchors = $req->{anchors};
	my $s = "\n There are uninteresting changes from this merge.\n" .
		qq( See the <a\nhref="#P">parents</a>, ) .
		"or view final state(s) below:\n\n";
	my $rel = $req->{relcmd};
	my $qs = $req->{qs};
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
