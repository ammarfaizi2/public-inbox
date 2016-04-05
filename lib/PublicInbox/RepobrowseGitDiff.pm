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
use PublicInbox::RepobrowseGitDiffCommon qw/git_diffstat_emit
	git_diff_ab_index git_diff_ab_hdr git_diff_ab_hunk/;

sub call_git_diff {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $id = $q->{id};
	my $id2 = $q->{id2};

	my @cmd = (qw(diff-tree -z --numstat -p --encoding=UTF-8
			--no-notes --no-color -M -B -D -r),
			$id2, $id, '--');
	my $expath = $req->{expath};
	push @cmd, $expath if defined $expath;
	$req->{rpipe} = $git->popen(\@cmd, undef, { 2 => $git->err_begin });
	my $env = $req->{cgi}->env;
	my $err = $env->{'psgi.errors'};
	my ($vin);
	$req->{dbuf} = '';
	$req->{p} = [ $id2 ];
	$req->{h} = $id;
	my $end = sub {
		if (my $fh = delete $req->{fh}) {
			# write out the last bit that was buffered
			my @buf = split(/\n/, delete $req->{dbuf}, -1);
			my $s = '';
			$s .= git_diff_line_i($req, $_) foreach @buf;
			$s .= '</pre></body></html>';
			$fh->write($s);

			$fh->close;
		} elsif (my $res = delete $req->{res}) {
			$res->($self->r(500));
		}
		if (my $rpipe = delete $req->{rpipe}) {
			$rpipe->close; # _may_ be Danga::Socket::close
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
		my $n = $req->{rpipe}->sysread($req->{dbuf}, 8192, $off);
		return $fail->() unless defined $n;
		return $end->() if $n == 0;
		if (my $res = delete $req->{res}) {
			my $h = ['Content-Type', 'text/html; charset=UTF-8'];
			my $fh = $req->{fh} = $res->([200, $h]);
			my $o = { nofollow => 1, noindex => 1 };
			my $ex = defined $expath ? " $expath" : '';
			$fh->write($self->html_start($req, 'diff', $o).
					"\n\n".
					utf8_html("git diff-tree -r -M -B -D ".
						"$id2 $id --$ex"). "\n\n");
		}
		if (my $fh = $req->{fh}) {
			git_diff_to_html($req, $fh);
		}
	};
	if (my $async = $env->{'pi-httpd.async'}) {
		$req->{rpipe} = $async->($req->{rpipe}, $cb);
		sub { $req->{res} = $_[0] } # let Danga::Socket handle the rest.
	} else { # synchronous loop for other PSGI servers
		$vin = '';
		vec($vin, fileno($req->{rpipe}), 1) = 1;
		sub {
			$req->{res} = $_[0];
			while ($req->{rpipe}) { $cb->() }
		}
	}
}

sub git_diff_line_i {
	my ($req, $l) = @_;
	my $cmt = '[a-f0-9]+';

	if ($l =~ m{^diff --git ("?a/.+) ("?b/.+)$}) { # regular
		$l = git_diff_ab_hdr($req, $1, $2);
	} elsif ($l =~ /^index ($cmt)\.\.($cmt)(.*)$/o) { # regular
		$l = git_diff_ab_index($1, $2, $3);
	} elsif ($l =~ /^@@ (\S+) (\S+) @@(.*)$/) { # regular
		$l = git_diff_ab_hunk($req, $1, $2, $3);
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
		git_diffstat_emit($req, $fh, $stat);
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

1;
