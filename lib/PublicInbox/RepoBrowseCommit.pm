# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::RepoBrowseCommit;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);
use PublicInbox::Git;

use constant GIT_FMT => '--pretty=format:'.join('%n',
	'%H', '%s', '%an <%ae>', '%ai', '%cn <%ce>', '%ci',
	'%t', '%p', '%D', '%b%x00');

sub git_commit_stream {
	my ($req, $q, $H, $log, $fh) = @_;
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
		"<pre\nstyle='white-space:pre-wrap'>" .
		"   commit $H" .
		"   author $au\t$ad\n" .
		"committer $cu\t$cd\n" .
		"     tree <a\nhref=\"${rel}tree?id=$t\">$t</a>\n";
	if (scalar(@p) == 1) {
		$x .= '   parent ';
		$x .= qq(<a\nhref="${rel}commit?id=$p[0]">$p[0]</a>\n);
	} elsif (scalar(@p) > 1) {
		$x .= '    merge ';
		$x .= join(' ', map {
			qq(<a\nhref="${rel}commit?id=$_">$_</a>)
			} @p);
		$x .= "\n";
	}
	$fh->write($x .= "\n$s\n\n");

	# body:
	local $/ = "\0";
	$x = PublicInbox::Hval->new($l = <$log>)->as_html; # body
	$fh->write($x);

	# diff
	local $/ = "\n";
	my $cmt = '[a-f0-9]+';
	my @href;
	while (defined($l = <$log>)) {
		if ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
			my $end = $3;
			my @l = ($1, $2);
			@href = git_blob_hrefs($rel, @l);
			@l = git_blob_links(\@href, \@l);
			$l = "index $l[0]..$l[1]$end";
		} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
			my $ctx = $3;
			my @l = ($1, $2);
			@l = git_blob_links(\@href, \@l);
			$l = "@@ $l[0] $l[1] @@".$ctx;
		} elsif ($l =~ /^index ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) { # --cc
			my @l = (split(',', $1), $2);
			my $end = $3;
			@href = git_blob_hrefs($rel, @l);
			@l = git_blob_links(\@href, \@l);
			my $res = pop @l;
			$l = 'index '.join(',', @l)."..$res$end";
		} elsif ($l =~ /^(@@@+) (\S+.*\S+) @@@+(.*)$/) { # --cc
			my ($at, $ctx) = ($1, $3);
			my @l = split(' ', $2);
			@l = git_blob_links(\@href, \@l);
			$l = join(' ', $at, @l, $at) . $ctx;
		} else {
			$l = PublicInbox::Hval->new($l)->as_html;
		}
		$fh->write($l . "\n");
	}
	$fh->write('</pre></body></html>');
}

sub call_git {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $path = $repo_info->{path};

	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';
	my $git = $repo_info->{git} ||= PublicInbox::Git->new($path);
	my $log = $git->popen(qw(show --no-notes --no-color
				 --abbrev=16 --irreversible-delete),
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
	map { "<a\nhref=\"${rel}blob?id=$_\"" } @ids;
}

sub git_blob_links {
	my ($hrefs, $labels) = @_;
	my $i = 0;
	map { $hrefs->[$i++].">$_</a>" } @$labels;
}

1;
