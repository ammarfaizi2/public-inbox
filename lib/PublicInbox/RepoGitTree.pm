# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoGitTree;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Hval qw(utf8_html);
use PublicInbox::Qspawn;

my %GIT_MODE = (
	'100644' => ' ', # blob
	'100755' => 'x', # executable blob
	'040000' => 'd', # tree
	'120000' => 'l', # symlink
	'160000' => 'g', # commit (gitlink)
);

my $BINARY_MSG = "Binary file, save using the 'raw' link above";

sub call_git_tree {
	my ($self, $req) = @_;
	my @extra = @{$req->{extra}};
	my $repo = $req->{-repo};
	my $git = $repo->{git};
	my $tip = $repo->tip;
	my $obj = "$tip:$req->{expath}";
	my ($hex, $type, $size) = $git->check($obj);

	unless (defined($type)) {
		return [404, ['Content-Type'=>'text/plain'], ['Not Found']];
	}

	my $opts = { nofollow => 1 };
	my $title = $req->{expath};
	$title = $title eq '' ? 'tree' : utf8_html($title);
	if ($type eq 'tree') {
		$opts->{noindex} = 1;
		$req->{thtml} = $self->html_start($req, $title, $opts) . "\n";
		git_tree_show($req, $hex);
	} elsif ($type eq 'blob') {
		sub {
			my $res = $_[0];
			my $fh = $res->([200,
				['Content-Type','text/html; charset=UTF-8']]);
			$fh->write($self->html_start($req, $title, $opts) .
					"\n");
			git_blob_show($req, $fh, $git, $hex);
			$fh->write('</body></html>');
			$fh->close;
		}
	} else {
		[404, ['Content-Type', 'text/plain; charset=UTF-8'],
			 ["Unrecognized type ($type) for $obj\n"]];
	}
}

sub cur_path {
	my ($req) = @_;
	my @ex = @{$req->{extra}} or return '<b>root</b>';
	my $s;

	my $tip = $req->{-repo}->tip;
	my $rel = $req->{relcmd};
	# avoid relative paths, here, we don't want to propagate
	# trailing-slash URLs although we tolerate them
	$s = "<a\nhref=\"${rel}tree/$tip\">root</a>/";
	my $cur = pop @ex;
	my @t;
	$s .= join('/', (map {
		push @t, $_;
		my $e = PublicInbox::Hval->utf8($_, join('/', @t));
		my $ep = $e->as_path;
		my $eh = $e->as_html;
		"<a\nhref=\"${rel}tree/$tip/$ep\">$eh</a>";
	} @ex), '<b>'.utf8_html($cur).'</b>');
}

sub git_blob_show {
	my ($req, $fh, $git, $hex) = @_;
	# ref: buffer_is_binary in git.git
	my $to_read = 8000; # git uses this size to detect binary files
	my $text_p;
	my $n = 0;

	my $rel = $req->{relcmd};
	my $raw = join('/',
			"${rel}raw", $req->{-repo}->tip, @{$req->{extra}});
	$raw = PublicInbox::Hval->utf8($raw)->as_path;
	my $t = cur_path($req);
	my $s = qq{\npath: $t\n\nblob $hex};
	my $end = '';

	$git->cat_file($hex, sub {
		my ($cat, $left) = @_; # $$left == $size
		$s .= qq{\t$$left bytes (<a\nhref="$raw">raw</a>)};
		$to_read = $$left if $to_read > $$left;
		my $r = read($cat, my $buf, $to_read);
		return unless defined($r) && $r > 0;
		$$left -= $r;

		if (index($buf, "\0") >= 0) {
			$fh->write("$s\n$BINARY_MSG</pre>");
			return;
		}
		$fh->write($s."</pre><hr/><table\nsummary=blob><tr><td><pre>");
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

sub git_tree_sed ($) {
	my ($req) = @_;
	my @lines;
	my $buf = '';
	my $pfx = $req->{tpfx};
	my $end;
	sub {
		my $dst = delete $req->{thtml} || '';
		if (defined $_[0]) {
			@lines = split(/\0/, $buf .= $_[0]);
			$buf = pop @lines if @lines;
		} else {
			@lines = split(/\0/, $buf);
			$end = '</pre></body></html>';
		}
		for (@lines) {
			my ($m, $x, $s, $path) =
					(/\A(\S+) \S+ (\S+)( *\S+)\t(.+)\z/s);
			$m = $GIT_MODE{$m} or next;
			$path = PublicInbox::Hval->utf8($path);
			my $ref = $path->as_path;
			$path = $path->as_html;

			if ($m eq 'g') {
				# TODO: support cross-repository gitlinks
				$dst .= 'g' . (' ' x 15) . "$path @ $x\n";
				next;
			}
			elsif ($m eq 'd') { $path = "$path/" }
			elsif ($m eq 'x') { $path = "<b>$path</b>" }
			elsif ($m eq 'l') { $path = "<i>$path</i>" }
			$s =~ s/\s+//g;

			# 'raw' and 'log' links intentionally omitted
			# for brevity and speed
			$dst .= qq($m\t).
				qq($s\t<a\nhref="$pfx/$ref">$path</a>\n);
		}
		$dst;
	}
}

sub git_tree_show {
	my ($req, $hex) = @_;
	my $git = $req->{-repo}->{git};
	my $cmd = $git->cmd(qw(ls-tree -l -z), $git->abbrev, $hex);
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	my $t = cur_path($req);
	my $pfx;

	$req->{thtml} .= "\npath: $t\n\n<b>mode\tsize\tname</b>\n";
	if ($req->{tslash}) {
		$pfx = '../';
	} elsif (defined(my $last = $req->{extra}->[-1])) {
		$pfx = PublicInbox::Hval->utf8($last)->as_path;
	} elsif (defined $req->{h}) {
		$pfx = $req->{-repo}->tip;
	} else {
		$pfx = 'tree/' . $req->{-repo}->tip;
	}
	$req->{tpfx} = $pfx;
	my $env = $req->{env};
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		if (defined $r) {
			$env->{'qspawn.filter'} = git_tree_sed($req);
			[ 200, [ 'Content-Type', 'text/html' ] ];
		} else {
			[ 500, [ 'Content-Type', 'text/plain' ], [ $git->err ]];
		}
	});
}

1;
