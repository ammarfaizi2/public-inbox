# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepobrowseGitTree;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::Hval qw(utf8_html);

my %GIT_MODE = (
	'100644' => ' ', # blob
	'100755' => 'x', # executable blob
	'040000' => 'd', # tree
	'120000' => 'l', # symlink
	'160000' => 'g', # commit (gitlink)
);

my $BINARY_MSG = "Binary file, save using the 'raw' link above";

sub git_tree_stream {
	my ($self, $req, $res) = @_; # res: Plack callback
	my @extra = @{$req->{extra}};
	my $git = $req->{repo_info}->{git};
	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $id = $q->{id};
	if ($id eq '') {
		chomp($id = $git->qx(qw(rev-parse --short=10 HEAD)));
		$q->{id} = $id;
	}

	my $obj = "$id:$req->{expath}";
	my ($hex, $type, $size) = $git->check($obj);

	if (!defined($type) || ($type ne 'blob' && $type ne 'tree')) {
		return $res->([404, ['Content-Type'=>'text/html'],
			 ['Not Found']]);
	}

	my $fh = $res->([200, ['Content-Type'=>'text/html; charset=UTF-8']]);
	$fh->write('<html><head>'. PublicInbox::Hval::STYLE .
		'<title></title></head><body>');

	if ($type eq 'tree') {
		git_tree_show($req, $fh, $git, $hex, $q);
	} elsif ($type eq 'blob') {
		git_blob_show($req, $fh, $git, $hex, $q);
	} else {
		# TODO
	}
	$fh->write('</body></html>');
	$fh->close;
}

sub call_git_tree {
	my ($self, $req) = @_;
	sub { git_tree_stream($self, $req, @_) };
}

sub cur_path {
	my ($req, $q) = @_;
	my $qs = $q->qs;
	my @ex = @{$req->{extra}} or return '<b>root</b>';
	my $s;

	my $rel = $req->{relcmd};
	# avoid relative paths, here, we don't want to propagate
	# trailing-slash URLs although we tolerate them
	$s = "<a\nhref=\"${rel}tree$qs\">root</a>/";
	my $cur = pop @ex;
	my @t;
	$s .= join('/', (map {
		push @t, $_;
		my $e = PublicInbox::Hval->utf8($_, join('/', @t));
		my $ep = $e->as_path;
		my $eh = $e->as_html;
		"<a\nhref=\"${rel}tree/$ep$qs\">$eh</a>";
	} @ex), '<b>'.utf8_html($cur).'</b>');
}

sub git_blob_show {
	my ($req, $fh, $git, $hex, $q) = @_;
	# ref: buffer_is_binary in git.git
	my $to_read = 8000; # git uses this size to detect binary files
	my $text_p;
	my $n = 0;

	my $rel = $req->{relcmd};
	my $plain = join('/', "${rel}plain", @{$req->{extra}});
	$plain = PublicInbox::Hval->utf8($plain)->as_path . $q->qs;
	my $t = cur_path($req, $q);
	my $h = qq{<pre>path: $t\n\nblob $hex};
	my $end = '';

	$git->cat_file($hex, sub {
		my ($cat, $left) = @_; # $$left == $size
		$h .= qq{\t$$left bytes (<a\nhref="$plain">raw</a>)};
		$to_read = $$left if $to_read > $$left;
		my $r = read($cat, my $buf, $to_read);
		return unless defined($r) && $r > 0;
		$$left -= $r;

		if (index($buf, "\0") >= 0) {
			$fh->write("$h\n$BINARY_MSG</pre>");
			return;
		}
		$fh->write($h."</pre><hr/><table\nsummary=blob><tr><td><pre>");
		$text_p = 1;

		while (1) {
			my @buf = split(/\r?\n/, $buf, -1);
			$buf = pop @buf; # last line, careful...
			foreach my $l (@buf) {
				++$n;
				$fh->write("<a\nid=n$n>". utf8_html($l).
						"</a>\n");
			}
			# no trailing newline:
			if ($$left == 0 && $buf ne '') {
				++$n;
				$buf = utf8_html($buf);
				$fh->write("<a\nid=n$n>". $buf ."</a>");
				$end = '<pre>\ No newline at end of file</pre>';
				last;
			}

			last unless defined($buf);

			$to_read = $$left if $to_read > $$left;
			my $off = length $buf; # last line from previous read
			$r = read($cat, $buf, $to_read, $off);
			return unless defined($r) && $r > 0;
			$$left -= $r;
		}
		0;
	});

	# line numbers go in a second column:
	$fh->write('</pre></td><td><pre>');
	$fh->write(qq(<a\nhref="#n$_">$_</a>\n)) foreach (1..$n);
	$fh->write("</pre></td></tr></table><hr />$end");
}

sub git_tree_show {
	my ($req, $fh, $git, $hex, $q) = @_;
	$fh->write('<pre>');
	my $ls = $git->popen(qw(ls-tree -l -z), $git->abbrev, $hex);
	my $t = cur_path($req, $q);
	my $pfx;
	$fh->write("path: $t\n\n");
	my $qs = $q->qs;

	if ($req->{tslash}) {
		$pfx = './';
	} elsif (defined(my $last = $req->{extra}->[-1])) {
		$pfx = PublicInbox::Hval->utf8($last)->as_path . '/';
	} else {
		$pfx = 'tree/';
	}

	local $/ = "\0";
	$fh->write("<b>mode\tsize\tname</b>\n");
	while (defined(my $l = <$ls>)) {
		chomp $l;
		my ($m, $t, $x, $s, $path) =
			($l =~ /\A(\S+) (\S+) (\S+)( *\S+)\t(.+)\z/s);
		$m = $GIT_MODE{$m} or next;
		$path = PublicInbox::Hval->utf8($path);
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
		$s =~ s/\s+//g;

		# 'plain' and 'log' links intentionally omitted for brevity
		# and speed
		$fh->write(qq($m\t).
			qq($s\t<a\nhref="$pfx$ref$qs">$path</a>\n));
	}
	$fh->write('</pre>');
}

1;
