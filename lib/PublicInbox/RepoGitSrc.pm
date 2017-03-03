# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoGitSrc;
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
my $MAX_ASYNC = 65536; # same as pipe size on Linux
my $BIN_DETECT = 8000; # same as git (buffer_is_binary in git.git)

sub call_git_src {
	my ($self, $req) = @_;
	my $repo = $req->{-repo};
	my $git = $repo->{git};
	my $tip = $req->{tip} || $req->{repo}->tip;
	sub {
		my ($res) = @_;
		$git->check_async($req->{env}, "$tip:$req->{expath}", sub {
			my ($info) = @_;
			my ($hex, $type, $size) = @$info;
			unless (defined $type) {
				$res->($self->rt(404, 'plain', 'Not Found'));
			}
			show_tree($self, $req, $res, $hex, $type, $size);
		});
	}
}

sub show_tree {
	my ($self, $req, $res, $hex, $type, $size) = @_;
	my $opts = { nofollow => 1 };
	my $title = "tree: ".utf8_html($req->{expath});
	$req->{thtml} = $self->html_start($req, $title, $opts) . "\n";
	if ($type eq 'tree') {
		$opts->{noindex} = 1;
		git_tree_show($self, $req, $res, $hex);
	} elsif ($type eq 'blob') {
		git_blob_show($self, $req, $res, $hex, $size);
	} else {
		$res->($self->rt(404, 'plain',
			"Unrecognized type ($type) for $hex\n"));
	}
}

sub cur_path {
	my ($req) = @_;
	my @ex = @{$req->{extra}} or return '<b>root</b>';
	my $s;
	my $tip = $req->{tip} || $req->{repo}->tip;
	my $rel = $req->{relcmd};
	# avoid relative paths, here, we don't want to propagate
	# trailing-slash URLs although we tolerate them
	$s = "<a\nhref=\"${rel}src/$tip\">root</a>/";
	my $cur = pop @ex;
	my @t;
	$s .= join('/', (map {
		push @t, $_;
		my $e = PublicInbox::Hval->utf8($_, join('/', @t));
		my $ep = $e->as_path;
		my $eh = $e->as_html;
		"<a\nhref=\"${rel}src/$tip/$ep\">$eh</a>";
	} @ex), '<b>'.utf8_html($cur).'</b>');
}

sub git_blob_sed ($$$) {
	my ($req, $hex, $size) = @_;
	my $pfx = $req->{tpfx};
	my $nl = 0;
	my $bytes = 0;
	my @lines;
	my $buf = '';
	my $rel = $req->{relcmd};
	my $tip = $req->{tip} || $req->{repo}->tip;
	my $raw = join('/', "${rel}raw", $tip, @{$req->{extra}});
	$raw = PublicInbox::Hval->utf8($raw)->as_path;
	my $t = cur_path($req);
	my $end = '';
	$req->{thtml} .= qq{\npath: $t\n\nblob $hex} .
			qq{\t$size bytes (<a\nhref="$raw">raw</a>)};
	$req->{lstart} = '</pre><hr/><pre>';
	my $s;

	sub {
		my $dst = delete $req->{thtml} || '';
		if (defined $_[0]) {
			return '' if $bytes < 0; # binary
			if ($bytes <= $BIN_DETECT) {
				if (index($_[0], "\0") >= 0) {
					$bytes = -1;
					$s = delete $req->{lstart} and
						$dst .= $s;
					$dst .= "\n";
					$dst .= $BINARY_MSG;
					return $dst .= '</pre></body></html>';
				}
			}
			$bytes += bytes::length($_[0]);
			$buf .= $_[0];
			$_[0] = ''; # save some memory
			$s = delete $req->{lstart} and $dst .= $s;
			@lines = split(/\r?\n/, $buf, -1);
			$buf = pop @lines; # last line, careful...
		} else { # EOF
			$s = delete $req->{lstart} and $dst .= $s;
			@lines = split(/\r?\n/, $buf, -1);
			$buf = pop @lines;
			$end .= '</pre></body></html>';
		}
		foreach (@lines) {
			++$nl;
			$dst .= "<a\nid=n$nl>";
			$dst .= sprintf("% 5u</a>\t", $nl);
			$dst .= utf8_html($_);
			$dst .= "\n";
		}
		@lines = ();
		if ($end && defined $buf && $buf ne '') {
			++$nl;
			$dst .= "<a\nid=n$nl>";
			$dst .= sprintf("% 5u</a>\t", $nl);
			$dst .= utf8_html($buf);
			$buf = undef;
			$dst .= "\n\\ No newline at end of file";
		}
		$dst .= $end;
	}
}

sub git_blob_show {
	my ($self, $req, $res, $hex, $size) = @_;
	my $sed = git_blob_sed($req, $hex, $size);
	my $git = $req->{-repo}->{git};
	if ($size <= $MAX_ASYNC) {
		my $buf = ''; # we slurp small files
		$git->cat_async($req->{env}, $hex, sub {
			my ($r) = @_;
			my $ref = ref($r);
			return if $ref eq 'ARRAY'; # redundant info
			if ($ref eq 'SCALAR') {
				$buf .= $$r;
				if (bytes::length($buf) == $size) {
					my $fh = $res->($self->rt(200, 'html'));
					$fh->write($sed->($buf));
					$fh->write($sed->(undef));
					$fh->close;
				}
				return;
			}
			my $cb = $res or return;
			$res = undef;
			$cb->($self->rt(500, 'plain', "Error\n"));
		});
	} else {
		$res->($self->rt(200, 'plain', "Too big\n"));
	}
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
	my ($self, $req, $res, $hex) = @_;
	my $git = $req->{-repo}->{git};
	my $cmd = $git->cmd(qw(ls-tree -l -z), $git->abbrev, $hex);
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	my $t = cur_path($req);
	my $pfx;

	$req->{thtml} .= "\npath: $t\n\n<b>mode\tsize\tname</b>\n";
	if (defined(my $last = $req->{extra}->[-1])) {
		$pfx = PublicInbox::Hval->utf8($last)->as_path;
	} elsif (defined(my $tip = $req->{tip})) {
		$pfx = $tip;
	} else {
		$pfx = 'src/' . $req->{-repo}->tip;
	}
	$req->{tpfx} = $pfx;
	my $env = $req->{env};
	$env->{'qspawn.response'} = $res;
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		if (defined $r) {
			$env->{'qspawn.filter'} = git_tree_sed($req);
			$self->rt(200, 'html');
		} else {
			$self->rt(500, 'plain', $git->err);
		}
	});
}

1;
