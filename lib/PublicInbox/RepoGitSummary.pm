# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# The main summary/landing page of a git repository viewer
package PublicInbox::RepoGitSummary;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepoBase);
use PublicInbox::Qspawn;

sub call_git_summary {
	my ($self, $req) = @_;
	my $git = $req->{-repo}->{git};
	my $env = $req->{env};
	sub {
		my ($res) = @_; # Plack streaming callback
		for_each_ref($self, $req, $res, $req->{-repo}->tip);
	}
}

use constant EACH_REF_FMT => '--format=' .
		join(' ', map { "%($_)" }
		qw(refname objecttype objectname creatordate:short subject));

sub for_each_ref {
	my ($self, $req, $res, $head_ref) = @_;
	my $count = 10; # TODO: configurable
	my $fh;
	my $repo = $req->{-repo};
	my $git = $repo->{git};
	my $refs = $git->popen(qw(for-each-ref --sort=-creatordate),
				EACH_REF_FMT, "--count=$count",
				qw(refs/heads/ refs/tags/));
	$fh = $res->([200, ['Content-Type', 'text/html; charset=UTF-8']]);
	# ref names are unpredictable in length and requires tables :<
	$fh->write($self->html_start($req,
				"$repo->{repo}: overview") .
			'</pre><table>');

	my $rel = $req->{relcmd};
	while (<$refs>) {
		my ($ref, $type, $hex, $date, $s) = split(' ', $_, 5);
		my $x = $ref eq $head_ref ? ' (HEAD)' : '';
		$ref =~ s!\Arefs/(?:heads|tags)/!!;
		$ref = PublicInbox::Hval->utf8($ref);
		my $h = $ref->as_html;
		$ref = $ref->as_href;
		my $sref;
		if ($type eq 'tag') {
			$h = "<b>$h</b>";
			$sref = $ref = $rel . 'tag/' . $ref;
		} elsif ($type eq 'commit') {
			$sref = $rel . 'commit/' . $ref;
			$ref = $rel . 'log/' . $ref;
		} else {
			# no point in wasting code to support tagged
			# trees/blobs...
			next;
		}
		chomp $s;
		$fh->write(qq(<tr><td><tt><a\nhref="$ref">$h</a>$x</tt></td>) .
			qq(<td><tt>$date <a\nhref="$sref">) . utf8_html($s) .
			'</a></tt></td></tr>');

	}
	$fh->write('</table>');

	# some people will use README.md or even README.sh here...
	my $readme = $repo->{readme};
	defined $readme or $readme = [ 'README', 'README.md' ];
	$readme = [ $readme ] if (ref($readme) ne 'ARRAY');
	foreach my $r (@$readme) {
		my $doc = $git->cat_file('HEAD:'.$r);
		defined $doc or next;
		$fh->write('<pre>' . readme_path_links($req, $rel, $r) .
			" (HEAD)\n\n" . utf8_html($$doc) . '</pre>');
	}
	$fh->write('</body></html>');
	$fh->close;
}

sub readme_path_links {
	my ($req, $rel, $readme) = @_;
	my @path = split(m!/+!, $readme);
	my $tip = $req->{-repo}->tip;
	my $s = "tree <a\nhref=\"${rel}tree/$tip\">root</a>/";
	my @t;
	$s .= join('/', (map {
		push @t, $_;
		my $e = PublicInbox::Hval->utf8($_, join('/', @t));
		my $ep = $e->as_path;
		my $eh = $e->as_html;
		$e = "<a\nhref=\"${rel}tree/$tip/$ep\">$eh</a>";
		# bold the last one
		scalar(@t) == scalar(@path) ? "<b>$e</b>" : $e;
	} @path));
}

1;
