# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# The main summary/landing page of a git repository viewer
package PublicInbox::RepobrowseGitSummary;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepobrowseBase);

sub call_git_summary {
	my ($self, $req) = @_;
	sub {
		my ($res) = @_; # Plack streaming callback
		emit_summary($self, $req, $res);
	}
}

use constant EACH_REF_FMT => '--format=' .
		join(' ', map { "%($_)" }
		qw(refname objecttype objectname creatordate:short subject));

sub emit_summary {
	my ($self, $req, $res) = @_;
	my $repo_info = $req->{repo_info};
	my $git = $repo_info->{git};
	my $count = 10; # TODO: configurable
	my $fh;

	# n.b. we would use %(HEAD) in for-each-ref --format if we could
	# rely on git 1.9.0+, but it's too soon for that in early 2016...
	chomp(my $head_ref = $git->qx(qw(symbolic-ref HEAD)));

	my $refs = $git->popen(qw(for-each-ref --sort=-creatordate),
				EACH_REF_FMT, "--count=$count",
				qw(refs/heads/ refs/tags/));
	$fh = $res->([200, ['Content-Type'=>'text/html; charset=UTF-8']]);
	# ref names are unpredictable in length and requires tables :<
	$fh->write($self->html_start($req,
				"$repo_info->{repo}: overview") .
			'</pre><table>');

	my $rel = $req->{relcmd};
	foreach (<$refs>) {
		my ($ref, $type, $hex, $date, $s) = split(' ', $_, 5);
		my $x = $ref eq $head_ref ? ' (HEAD)' : '';
		$ref =~ s!\Arefs/(?:heads|tags)/!!;
		$ref = PublicInbox::Hval->utf8($ref);
		my $h = $ref->as_html;
		$ref = $ref->as_href;
		my $sref;
		if ($type eq 'tag') {
			$h = "<b>$h</b>";
			$sref = $ref = $rel . 'tag?h=' . $ref;
		} elsif ($type eq 'commit') {
			$sref = $rel . 'commit?h=' . $ref;
			$ref = $rel . 'log?h=' . $ref;
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
	my $readme = $repo_info->{readme};
	defined $readme or $readme = 'README';
	my $doc = $git->cat_file('HEAD:'.$readme);
	if (defined $doc) {
		$fh->write('<pre>' .
			readme_path_links($rel, $readme) . " (HEAD)\n\n");
		$fh->write(utf8_html($$doc));
		$fh->write('</pre>');
	}
	$fh->write('</body></html>');
	$fh->close;
}

sub readme_path_links {
	my ($rel, $readme) = @_;
	my @path = split(m!/+!, $readme);

	my $s = "tree <a\nhref=\"${rel}tree\">root</a>/";
	my @t;
	$s .= join('/', (map {
		push @t, $_;
		my $e = PublicInbox::Hval->utf8($_, join('/', @t));
		my $ep = $e->as_path;
		my $eh = $e->as_html;
		$e = "<a\nhref=\"${rel}tree/$ep\">$eh</a>";
		# bold the last one
		scalar(@t) == scalar(@path) ? "<b>$e</b>" : $e;
	} @path));
}

1;
