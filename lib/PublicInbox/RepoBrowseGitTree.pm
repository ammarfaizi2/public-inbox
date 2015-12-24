# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoBrowseGitTree;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);
use PublicInbox::Git;
use PublicInbox::Hval qw(utf8_html);

my %GIT_MODE = (
	'100644' => ' ', # blob
	'100755' => 'x', # executable blob
	'040000' => 'd', # tree
	'120000' => 'l', # symlink
	'160000' => 'g', # commit (gitlink)
);

sub git_tree_stream {
	my ($self, $req, $res) = @_; # res: Plack callback
	my $repo_info = $req->{repo_info};
	my $dir = $repo_info->{path};
	my @extra = @{$req->{extra}};
	my $tslash;
	if (@extra && $extra[-1] eq '') { # no trailing slash
		pop @extra;
		$tslash = 1;
	}
	my $tree_path = join('/', @extra);
	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';

	my $obj = "$id:$tree_path";
	my $git = $repo_info->{git} ||= PublicInbox::Git->new($dir);
	my ($hex, $type, $size) = $git->check($obj);

	if (!defined($type) || ($type ne 'blob' && $type ne 'tree')) {
		return $res->([404, ['Content-Type'=>'text/html'],
			 ['Not Found']]);
	}

	my $fh = $res->([200, ['Content-Type'=>'text/html; charset=UTF-8']]);
	$fh->write('<html><head><title></title></head><body>'.
			 PublicInbox::Hval::PRE);

	if ($type eq 'tree') {
		git_tree_show($fh, $git, $hex, $q, \@extra, $tslash);
	} elsif ($type eq 'blob') {
		git_blob_show($fh, $git, $hex);
	}
	$fh->write('</body></html>');
	$fh->close;
}

sub call_git_tree {
	my ($self, $req) = @_;
	sub { git_tree_stream($self, $req, @_) };
}

sub git_blob_binary {
	my ($fh) = @_;
	$fh->write("Binary file cannot be displayed\n");
}

sub git_blob_show {
	my ($fh, $git, $hex) = @_;
	# ref: buffer_is_binary in git.git
	my $to_read = 8000; # git uses this size to detect binary files
	my $text_p;
	$git->cat_file($hex, sub {
		my ($cat, $left) = @_; # $$left == $size
		my $n = 0;
		$to_read = $$left if $to_read > $$left;
		my $r = read($cat, my $buf, $to_read);
		return unless defined($r) && $r > 0;
		$$left -= $r;

		return git_blob_binary($fh) if (index($buf, "\0") >= 0);

		$text_p = 1;
		while (1) {
			my @buf = split(/\r?\n/, $buf, -1);
			$buf = pop @buf; # last line, careful...
			foreach my $l (@buf) {
				++$n;
				$fh->write("<a\nid=n$n>". utf8_html($l).
						"</a>\n");
			}
			last if ($$left == 0 || !defined $buf);

			$to_read = $$left if $to_read > $$left;
			my $off = length $buf; # last line from previous read
			$r = read($cat, $buf, $to_read, $off);
			return unless defined($r) && $r > 0;
			$$left -= $r;
		}
		0;
	});
}

sub git_tree_show {
	my ($fh, $git, $hex, $q, $extra, $tslash) = @_;

	my $ls = $git->popen(qw(ls-tree --abbrev=16 -l -z), $hex);
	local $/ = "\0";
	my $pfx = $tslash ? '.' :
		(@$extra ?  PublicInbox::Hval->new_bin($extra->[-1])->as_path :
		 'tree');
	$pfx .= '/';

	my $qs = $q->qs;
	while (defined(my $l = <$ls>)) {
		chomp $l;
		my ($m, $t, $x, $s, $path) =
			($l =~ /\A(\S+) (\S+) (\S+)( *\S+)\t(.+)\z/s);
		$m = $GIT_MODE{$m} or next;
		$path = PublicInbox::Hval->new_bin($path);
		my $ref = $path->as_path;
		$path = $path->as_html;

		if ($m eq 'g') {
			# TODO: support cross-repository gitlinks
			$fh->write('g' . (' ' x 18) . "$path @ $x\n");
			next;
		}
		elsif ($m eq 'd') { $path = "<b>$path/</b>" }
		elsif ($m eq 'x') { $path = "<b>$path</b>" }
		elsif ($m eq 'l') { $path = "<i>$path</i>" }

		$ref = $pfx.$ref.$qs;
		$fh->write("$m log raw $s <a\nhref=\"$ref\">$path</a>\n");
	}
	$fh->write('</pre>');
}

1;
