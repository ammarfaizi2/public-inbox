# Copyright (C) 2015-2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepobrowseGitPlain;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::RepobrowseGitBlob;
use PublicInbox::Hval qw(utf8_html);

sub call_git_plain {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $q = PublicInbox::RepobrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';

	if (length(my $expath = $req->{expath})) {
		$id .= ":$expath";
	} else {
		$id .= ':';
	}
	my ($cat, $hex, $type, $size) = $git->cat_file_begin($id);
	return unless defined $cat;

	my ($r, $buf);
	my $left = $size;
	if ($type eq 'blob') {
		$type = git_blob_mime_type($self, $req, $cat, \$buf, \$left);
	} elsif ($type eq 'commit' || $type eq 'tag') {
		$type = 'text/plain';
	} elsif ($type eq 'tree') {
		$git->cat_file_finish($left);
		return git_tree_plain($req, $git, $hex);
	} else {
		$type = 'application/octet-stream';
	}
	git_blob_stream_response($git, $cat, $size, $type, $buf, $left);
}

# This should follow the cgit DOM structure in case anybody depends on it,
# not using <pre> here as we don't expect people to actually view it much
sub git_tree_plain {
	my ($req, $git, $hex) = @_;

	my @ex = @{$req->{extra}};
	my $rel = $req->{relcmd};
	my $title = utf8_html(join('/', '', @ex, ''));
	my $tslash = $req->{tslash};
	my $pfx = $tslash ? './' : 'plain/';
	my $t = "<h2>$title</h2><ul>";
	if (@ex) {
		if ($tslash) {
			$t .= qq(<li><a\nhref="../">../</a></li>);
		} else  {
			$t .= qq(<li><a\nhref="./">../</a></li>);
			my $last = PublicInbox::Hval->utf8($ex[-1])->as_href;
			$pfx = "$last/";
		}
	}
	my $ls = $git->popen(qw(ls-tree --name-only -z --abbrev=12), $hex);
	sub {
		my ($res) = @_;
		my $fh = $res->([ 200, ['Content-Type' => 'text/html']]);
		$fh->write("<html><head><title>$title</title></head><body>".
				$t);

		local $/ = "\0";
		while (defined(my $n = <$ls>)) {
			chomp $n;
			$n = PublicInbox::Hval->utf8($n);
			my $ref = $n->as_path;
			$n = $n->as_html;

			$fh->write(qq(<li><a\nhref="$pfx$ref">$n</a></li>))
		}
		$fh->write('</ul></body></html>');
		$fh->close;
	}
}

1;
