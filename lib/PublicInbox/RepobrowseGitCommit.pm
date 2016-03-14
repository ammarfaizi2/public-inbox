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

sub git_commit_stream {
	my ($self, $req) = @_;
	my $log = $req->{log};
	my $H = <$log>;
	defined $H or return git_commit_404($req);
	my $fh = delete($req->{res})->([200, ['Content-Type'=>'text/html']]);
	$req->{fh} = $fh;
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
	my $q = $req->{'q'};
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
	$fh->write($x .= "\n$s\n\n");

	# body:
	local $/ = "\0";
	$l = <$log>;
	chomp $l;
	$fh->write(utf8_html($l)."<a\nid=D>---</a>\n");
	$req->{anchors} = {};
	$req->{h} = $h;
	$req->{p} = \@p;
	$req->{rel} = $rel;
	{
		local $/ = "\0\0";
		my $l = <$log>;
		chomp $l;
		git_diffstat_emit($req, $fh, $l);
	}
	my $help;
	$help = " This is a merge, showing combined diff:\n\n" if ($np > 1);

	# diff
	local $/ = "\n";
	my $cmt = '[a-f0-9]+';
	while (defined($l = <$log>)) {
		if ($help) {
			$fh->write($help);
			$help = undef;
		}
		if ($l =~ m{^diff --git ("?a/.+) ("?b/.+)$}) { # regular
			$l = git_diff_ab_hdr($req, $1, $2) . "\n";
		} elsif ($l =~ m{^diff --(cc|combined) (.+)$}) {
			$l = git_diff_cc_hdr($req, $1, $2) . "\n";
		} elsif ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
			$l = git_diff_ab_index($1, $2, $3) . "\n";
		} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
			$l = git_diff_ab_hunk($req, $1, $2, $3) . "\n";
		} elsif ($l =~ /^index ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) { # --cc
			$l = git_diff_cc_index($req, $1, $2, $3) . "\n";
		} elsif ($l =~ /^(@@@+) (\S+.*\S+) @@@+(.*)$/) { # --cc
			$l = git_diff_cc_hunk($req, $1, $2, $3) . "\n";
		} else {
			$l = utf8_html($l);
		}
		$fh->write($l);
	}

	if ($help) {
		$fh->write(" This is a merge, combined diff is empty.\n");
	}

	show_unchanged($fh, $req, $qs);
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
	my $cmd = [ qw(show -z --numstat -p --encoding=UTF-8
			--no-notes --no-color -c),
			$git->abbrev, GIT_FMT, $id, '--' ];
	$req->{log} = $git->popen($cmd, undef, { 2 => $git->err_begin });
	$req->{end} = sub {
		$req->{cb} = $req->{end} = undef;
		if (my $fh = delete $req->{fh}) {
			$fh->close;
		} elsif (my $res = delete $req->{res}) {
			$res->(r(500));
		}
		if (my $log = delete $req->{log}) {
			$log->close; # _may_ be Danga::Socket::close
		}
		# zero the error file for now, be careful about printing
		# $id to psgi.errors w/o sanitizing...
		$git->err;
	};
	$req->{'q'} = $q;
	$req->{cb} = sub { # read git-show output and stream to client
		git_commit_stream($self, $req);
		$req->{end}->();
	};
	sub {
		$req->{res} = $_[0];
		$req->{cb}->();
	}
}

sub git_commit_404 {
	my ($req) = @_;
	my $x = 'Missing commit or path';
	my $pfx = "$req->{relcmd}commit";

	my $try = 'try';
	$x = "<html><head><title>$x</title></head><body><pre><b>$x</b>\n\n";
	my $qs = $req->{'q'}->qs(id => '');
	$x .= "<a\nhref=\"$pfx$qs\">$try the latest commit in HEAD</a>\n";
	$x .= '</pre></body>';

	delete($req->{res})->([404, ['Content-Type'=>'text/html'], [ $x ]]);
}

sub git_diff_cc_hdr {
	my ($diff, $combined, $path) = @_;
	my $html_path = utf8_html($path);
	$path = git_unquote($path);
	my $anchor = to_attr($path);
	delete $diff->{anchors}->{$anchor};
	my $cc = $diff->{cc} = PublicInbox::Hval->utf8($path);
	$diff->{path_cc} = $cc->as_path;
	qq(<a\nhref=#D\nid="$anchor">diff</a> --$combined $html_path);
}

# index abcdef09,01234567..76543210
sub git_diff_cc_index {
	my ($diff, $before, $last, $end) = @_;
	$end = utf8_html($end);
	my @before = split(',', $before);
	$diff->{pobj_cc} = \@before;

	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	"index $before..$last$end";
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
		$rv .= " $last";
	} else {
		my $h = $diff->{h};
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
