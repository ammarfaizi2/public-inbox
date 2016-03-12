# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /diff endpoint for git repositories for cgit compatibility
# usage: /repo.git/diff?id=COMMIT_ID&id2=COMMIT_ID2
#
# FIXME: much duplicated code between this and RepobrowseGitCommit.pm
#
# We probably will not link to this outright because it's expensive,
# but exists to preserve URL compatibility.
package PublicInbox::RepobrowseGitDiff;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::Hval qw(utf8_html to_attr);
use PublicInbox::RepobrowseGit qw(git_unquote git_commit_title);

sub call_git_diff {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $id = $q->{id};
	my $id2 = $q->{id2};

	my @cmd = (qw(diff-tree -z --numstat -p --encoding=UTF-8
			--no-notes --no-color -M -B -D -r),
			$id2, $id, '--');
	if (defined(my $expath = $req->{expath})) {
		push @cmd, $expath;
	}
	my $rpipe = $git->popen(\@cmd, undef, { 2 => $git->err_begin });
	my $env = $req->{cgi}->env;
	my $err = $env->{'psgi.errors'};
	my ($res, $vin, $fh);
	$req->{dbuf} = '';
	$req->{p} = [ $id2 ];
	$req->{h} = $id;

	my $end = sub {
		if ($fh) {
			# write out the last bit that was buffered
			my @buf = split(/\n/, delete $req->{dbuf}, -1);
			my $s = '';
			$s .= git_diff_line_i($req, $_) foreach @buf;
			$s .= '</pre></body></html>';
			$fh->write($s);

			$fh->close;
			$fh = undef;
		} elsif ($res) {
			$res->($self->r(500));
		}
		if ($rpipe) {
			$rpipe->close; # _may_ be Danga::Socket::close
			$rpipe = undef;
		}
	};
	my $fail = sub {
		if ($!{EAGAIN} || $!{EINTR}) {
			select($vin, undef, undef, undef) if defined $vin;
			# $vin is undef on async, so this is a noop on EAGAIN
			return;
		}
		my $e = $!;
		$end->();
		$err->print("git diff ($git->{git_dir}): $e\n");
	};
	my $cb = sub {
		my $off = length($req->{dbuf});
		my $n = $rpipe->sysread($req->{dbuf}, 8192, $off);
		return $fail->() unless defined $n;
		return $end->() if $n == 0;
		if ($res) {
			my $h = ['Content-Type', 'text/html; charset=UTF-8'];
			$fh = $res->([200, $h]);
			$res = undef;
			my $o = { nofollow => 1, noindex => 1 };
			$fh->write($self->html_start($req, 'diff', $o)."\n");
		}
		git_diff_to_html($req, $fh) if $fh;
	};
	if (my $async = $env->{'pi-httpd.async'}) {
		$rpipe = $async->($rpipe, $cb);
		sub { ($res) = @_ } # let Danga::Socket handle the rest.
	} else { # synchronous loop for other PSGI servers
		$vin = '';
		vec($vin, fileno($rpipe), 1) = 1;
		sub {
			($res) = @_;
			while ($rpipe) { $cb->() }
		}
	}
}

sub git_diffstat_to_html ($$$) {
	my ($req, $fh, undef) = @_;
	my @stat = split("\0", $_[2]); # avoiding copy for $_[2]
	my $nr = 0;
	my ($nadd, $ndel) = (0, 0);
	my $s = '';
	while (defined(my $l = shift @stat)) {
		$l =~ s/\n?(\S+)\t+(\S+)\t+// or next;
		my ($add, $del) = ($1, $2);
		if ($add =~ /\A\d+\z/) {
			$nadd += $add;
			$ndel += $del;
			$add = "+$add";
			$del = "-$del";
		}
		my $num = sprintf('% 6s/%-6s', $del, $add);
		if (length $l) {
			my $anchor = to_attr(git_unquote($l));
			$req->{anchors}->{$anchor} = $l;
			$l = utf8_html($l);
			$l = qq(<a\nhref="#$anchor">$l</a>);
		} else {
			my $from = shift @stat;
			my $to = shift @stat;
			$l = git_diffstat_rename($req, $from, $to);
		}
		++$nr;
		$s .= ' '.$num."\t".$l."\n";
	}
	$s .= "\n $nr ";
	$s .= $nr == 1 ? 'file changed, ' : 'files changed, ';
	$s .= $nadd;
	$s .= $nadd == 1 ? ' insertion(+), ' : ' insertions(+), ';
	$s .= $ndel;
	$s .= $ndel == 1 ? " deletion(-)\n\n" : " deletions(-)\n\n";
	$fh->write($s);
}

sub git_diff_line_i {
	my ($req, $l) = @_;
	my $cmt = '[a-f0-9]+';

	if ($l =~ m{^diff --git ("?a/.+) ("?b/.+)$}) { # regular
		$l = git_diff_ab_hdr($req, $1, $2);
	} elsif ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
		$l = git_diff_ab_index($req, $1, $2, $3);
	} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
		$l = git_diff_ab_hunk($req, $1, $2, $3);
	} elsif ($l =~ /^index ($cmt,[^\.]+)\.\.($cmt)(.*)$/o) { # --cc
		$l = git_diff_cc_index($req, $1, $2, $3);
	} else {
		$l = utf8_html($l);
	}
	$l .= "\n";
}

sub git_diff_to_html {
	my ($req, $fh) = @_;
	if (!$req->{diff_state}) {
		my ($stat, $buf) = split(/\0\0/, $req->{dbuf}, 2);
		return unless defined $buf;
		$req->{dbuf} = $buf;
		git_diffstat_to_html($req, $fh, $stat);
		$req->{diff_state} = 1;
	}
	my @buf = split(/\n/, $req->{dbuf}, -1);
	$req->{dbuf} = pop @buf; # last line, careful...
	if (@buf) {
		my $s = '';
		$s .= git_diff_line_i($req, $_) foreach @buf;
		$fh->write($s) if $s ne '';
	}
}

sub git_diffstat_rename {
	my ($req, $from, $to) = @_;
	my $anchor = to_attr(git_unquote($to));
	$req->{anchors}->{$anchor} = $to;
	my @from = split('/', $from);
	my @to = split('/', $to);
	my $orig_to = $to;
	my ($base, @base);
	while (@to && @from && $to[0] eq $from[0]) {
		push @base, shift(@to);
		shift @from;
	}

	$base = utf8_html(join('/', @base)) if @base;
	$from = utf8_html(join('/', @from));
	$to = PublicInbox::Hval->utf8(join('/', @to), $orig_to);
	my $tp = $to->as_path;
	my $th = $to->as_html;
	$to = qq(<a\nhref="#$anchor">$th</a>);
	@base ? "$base/{$from =&gt; $to}" : "$from =&gt; $to";
}

# index abcdef89..01234567
sub git_diff_ab_index {
	my ($req, $xa, $xb, $end) = @_;
	# not wasting bandwidth on links here, yet
	# links in hunk headers are far more useful with line offsets
	$end = utf8_html($end);
	"index $xa..$xb$end";
}

# diff --git a/foo.c b/bar.c
sub git_diff_ab_hdr {
	my ($req, $fa, $fb) = @_;
	my $html_a = utf8_html($fa);
	my $html_b = utf8_html($fb);
	$fa = git_unquote($fa);
	$fb = git_unquote($fb);
	$fa =~ s!\Aa/!!;
	$fb =~ s!\Ab/!!;
	my $anchor = to_attr($fb);
	delete $req->{anchors}->{$anchor};
	$fa = $req->{fa} = PublicInbox::Hval->utf8($fa);
	$fb = $req->{fb} = PublicInbox::Hval->utf8($fb);
	$req->{path_a} = $fa->as_path;
	$req->{path_b} = $fb->as_path;

	# not wasting bandwidth on links here
	# links in hunk headers are far more useful with line offsets
	qq(<a\nhref=#D\nid="$anchor">diff</a> --git $html_a $html_b);
}

# @@ -1,2 +3,4 @@ (regular diff)
sub git_diff_ab_hunk {
	my ($req, $ca, $cb, $ctx) = @_;
	my ($na) = ($ca =~ /\A-(\d+)/);
	my ($nb) = ($cb =~ /\A\+(\d+)/);

	# we add "rel=nofollow" here to reduce load on search engines, here
	my $rel = $req->{relcmd};
	my $rv = '@@ ';
	if (defined($na) && $na == 0) { # new file
		$rv .= $ca;
	} else {
		$na = defined $na ? "#n$na" : '';
		my $p = $req->{p}->[0];
		$rv .= qq(<a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$req->{path_a}?id=$p$na">);
		$rv .= "$ca</a>";
	}
	$rv .= ' ';
	if (defined($nb) && $nb == 0) { # deleted file
		$rv .= $cb;
	} else {
		my $h = $req->{h};
		$nb = defined $nb ? "#n$nb" : '';
		$rv .= qq(<a\nrel=nofollow);
		$rv .= qq(\nhref="${rel}tree/$req->{path_b}?id=$h$nb">);
		$rv .= "$cb</a>";
	}
	$rv . ' @@' . utf8_html($ctx);
}

1;
