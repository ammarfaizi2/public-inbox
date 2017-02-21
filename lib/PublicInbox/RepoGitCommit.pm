# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /commit/ endpoint for git repositories
#
# anchors used:
#	D - diffstat
#	P - parents
#	...and various filenames from to_attr
# The 'D' and 'P' anchors may conflict with odd filenames, but we won't
# punish the common case with extra bytes if somebody uses 'D' or 'P'
# in filenames.

package PublicInbox::RepoGitCommit;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Hval qw(utf8_html to_attr);
use PublicInbox::RepoGit qw(git_unquote git_commit_title);
use PublicInbox::RepoGitDiffCommon;
use PublicInbox::Qspawn;

use constant {
	GIT_FMT => '--pretty=format:'.join('%n',
		'%H', '%s', '%an <%ae>', '%ai', '%cn <%ce>', '%ci',
		'%t', '%p', '%D', '%b%x00'),
	CC_EMPTY => " This is a merge, and the combined diff is empty.\n",
	CC_MERGE => " This is a merge, showing combined diff:\n\n"
};

sub commit_header {
	my ($self, $req) = @_;
	my ($H, $s, $au, $ad, $cu, $cd, $t, $p, $D, $rest) =
		split("\n", $req->{dbuf}, 10);
	$s = utf8_html($s);
	$au = utf8_html($au);
	$cu = utf8_html($cu);
	my @p = split(' ', $p);

	my $rel = $req->{relcmd};
	my $x = $self->html_start($req, $s) . "\n" .
		qq(   commit $H (<a\nhref="${rel}patch/$H">patch</a>)\n) .
		qq(     tree <a\nrel=nofollow\nhref="${rel}tree/$H">$t</a>);

	my $git = $req->{-repo}->{git};
	# extra show path information, if any
	my $extra = $req->{extra};
	my $path = '';
	if (@$extra) {
		my @t;
		my $ep;
		$x .= ' -- ';
		$x .= join('/', map {
			push @t, $_;
			my $e = PublicInbox::Hval->utf8($_, join('/', @t));
			$ep = $e->as_path;
			my $eh = $e->as_html;
			$ep = "${rel}tree/$ep/$H";
			qq(<a\nrel=nofollow\nhref="$ep">$eh</a>);
		} @$extra);
		$path = "/$ep";
	}

	$x .= "\n   author $au\t$ad\ncommitter $cu\t$cd\n";
	my $np = scalar @p;
	if ($np == 1) {
		my $p = $p[0];
		$x .= git_parent_line('   parent', $p, $git, $rel);
	} elsif ($np > 1) {
		$req->{mhelp} = CC_MERGE;
		my @common = ($git, $rel);
		my @t = @p;
		my $p = shift @t;
		$x .= git_parent_line('  parents', $p, @common);
		foreach $p (@t) {
			$x .= git_parent_line('         ', $p, @common);
		}
	}
	$x .= "\n<b>";
	$x .= $s;
	$x .= "</b>\n\n";
	my $bx00;

	# FIXME: deal with excessively long commit message bodies
	($bx00, $req->{dbuf}) = split("\0", $rest, 2);
	$req->{anchors} = {};
	$req->{H} = $H;
	$req->{p} = \@p;
	$x .= utf8_html($bx00) . "<a\nid=D>---</a>\n";
}

sub git_commit_sed ($$) {
	my ($self, $req) = @_;
	git_diff_sed_init($req);
	my $dbuf = \($req->{dbuf});

	# this filters for $fh->write or $body->getline (see Qspawn)
	sub {
		my $dst = '';
		if (defined $_[0]) { # $_[0] == scalar buffer
			$$dbuf .= $_[0];
			if ($req->{dstate} == DSTATE_INIT) {
				return $dst if index($$dbuf, "\0") < 0;
				$req->{dstate} = DSTATE_STAT;
				$dst .= commit_header($self, $req);
			}
			git_diff_sed_run(\$dst, $req);
		} else { # undef means EOF from "git show", flush the last bit
			git_diff_sed_close(\$dst, $req);
			$dst .= CC_EMPTY if delete $req->{mhelp};
			show_unchanged(\$dst, $req);
			$dst .= '</pre></body></html>';
		}
		$dst;
	}
}

sub call_git_commit { # RepoBase calls this
	my ($self, $req) = @_;
	my $env = $req->{env};

	my $expath = $req->{expath};
	if ($expath ne '') {
		my $relup = join('', map { '../' } @{$req->{extra}});
		return $self->r(301, $req, "$relup#".to_attr($expath));
	}

	my $git = $req->{-repo}->{git};
	my $cmd = $git->cmd(qw(show -z --numstat -p --encoding=UTF-8
			--no-notes --no-color -c --no-abbrev),
			GIT_FMT, $req->{-repo}->tip, '--');
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	$env->{'qspawn.quiet'} = 1;
	$qsp->psgi_return($env, undef, sub { # parse header
		my ($r, $bref) = @_;
		if (!defined $r) {
			my $errmsg = $git->err;
			[ 500, [ 'Content-Type', 'text/html' ], [ $errmsg ] ];
		} elsif ($r == 0) {
			git_commit_404($req);
		} else {
			$env->{'qspawn.filter'} = git_commit_sed($self, $req);
			[ 200, [ 'Content-Type', 'text/html' ] ];
		}
	});
}

sub git_commit_404 {
	my ($req) = @_;
	my $x = 'Missing commit or path';
	my $pfx = "$req->{relcmd}commit";

	my $try = 'try';
	$x = "<html><head><title>$x</title></head><body><pre><b>$x</b>\n\n";
	$x .= "<a\nhref=\"$pfx\">$try the latest commit in HEAD</a>\n";
	$x .= '</pre></body>';

	[404, ['Content-Type', 'text/html'], [ $x ]];
}

# FIXME: horrifically expensive...
sub git_parent_line {
	my ($pfx, $p, $git, $rel) = @_;
	my $t = git_commit_title($git, $p);
	$t = defined $t ? utf8_html($t) : '';
	my $pad = ' ' x length($pfx);
	$pfx . qq( <a\nid=P\nhref="${rel}commit/$p">$p</a>\n $pad$t\n);
}

# do not break anchor links if the combined diff doesn't show changes:
sub show_unchanged {
	my ($dst, $req) = @_;

	my @unchanged = sort keys %{$req->{anchors}};
	return unless @unchanged;
	my $anchors = $req->{anchors};
	$$dst .= "\n There are uninteresting changes from this merge.\n" .
		qq( See the <a\nhref="#P">parents</a>, ) .
		"or view final state(s) below:\n\n";
	my $rel = $req->{relcmd};
	foreach my $anchor (@unchanged) {
		my $fn = $anchors->{$anchor};
		my $p = PublicInbox::Hval->utf8(git_unquote($fn));
		$p = $p->as_path;
		$fn = utf8_html($fn);
		$$dst .= qq(\t<a\nrel=nofollow);
		$$dst .= qq(\nid="$anchor"\nhref="${rel}tree/$p">);
		$$dst .= "$fn</a>\n";
	}
}

1;
