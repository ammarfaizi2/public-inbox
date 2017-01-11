# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /diff endpoint for git repositories for cgit compatibility
# usage: /repo.git/diff?id=COMMIT_ID&id2=COMMIT_ID2
#
# We probably will not link to this outright because it's expensive,
# but exists to preserve URL compatibility with cgit.
package PublicInbox::RepobrowseGitDiff;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::Hval qw(utf8_html);
use PublicInbox::RepobrowseGitDiffCommon;
use PublicInbox::Qspawn;

sub git_diff_sed ($$) {
	my ($self, $req) = @_;
	git_diff_sed_init($req);
	$req->{dstate} = DSTATE_STAT;
	# this filters for $fh->write or $body->getline (see Qspawn)
	sub {
		my $dst = delete $req->{dhtml} || '';
		if (defined $_[0]) { # $_[0] == scalar buffer
			$req->{dbuf} .= $_[0];
			git_diff_sed_run(\$dst, $req);
		} else { # undef means EOF from "git show", flush the last bit
			git_diff_sed_close(\$dst, $req);
			$dst .= '</pre></body></html>';
		}
		$dst;
	}
}

sub call_git_diff {
	my ($self, $req) = @_;
	my $env = $req->{env};
	my $q = PublicInbox::RepobrowseGitQuery->new($env);
	my $id = $q->{id};
	my $id2 = $q->{id2};

	my $git = $req->{repo_info}->{git};
	my $cmd = [ 'git', "--git-dir=$git->{git_dir}", qw(diff-tree
			-z --numstat -p --encoding=UTF-8
			--no-color -M -B -D -r),
			$id2, $id, '--' ];
	my $expath = $req->{expath};
	push @$cmd, $expath if $expath ne '';
	my $o = { nofollow => 1, noindex => 1 };
	my $ex = $expath eq '' ? '' : " $expath";
	$req->{dhtml} = $self->html_start($req, 'diff', $o). "\n\n".
				utf8_html("git diff-tree -r -M -B -D ".
				"$id2 $id --$ex"). "\n\n";
	$req->{p} = [ $id2 ];
	$req->{h} = $id;
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	# $env->{'qspawn.quiet'} = 1;
	$qsp->psgi_return($env, undef, sub { # parse header
		my ($r) = @_;
		if (!defined $r) {
			[ 500, [ 'Content-Type', 'text/html' ], [ $git->err ]];
		} elsif ($r == 0) {
			[ 200, [ 'Content-Type', 'text/html' ], [
				delete($req->{dhtml}).
				'No differences</pre></body></html>' ]
			]
		} else {
			$env->{'qspawn.filter'} = git_diff_sed($self, $req);
			[ 200, [ 'Content-Type', 'text/html' ] ];
		}
	});
}

1;
