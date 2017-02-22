# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /snapshot/ endpoint for git repositories
# Mainly for compatibility reasons with cgit, I'm unsure if
# showing this in a repository viewer is a good idea.

package PublicInbox::RepoGitSnapshot;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Git;
use PublicInbox::Qspawn;
our $SUFFIX;
BEGIN {
	# as described in git-archive(1), users may add support for
	# other compression schemes such as xz or bz2 via git-config(1):
	#	git config tar.tar.xz.command "xz -c"
	#	git config tar.tar.bz2.command "bzip2 -c"
	chomp(my @l = `git archive --list`);
	$SUFFIX = join('|', map { quotemeta $_ } @l);
}

# Not using standard mime types since the compressed tarballs are
# special or do not match my /etc/mime.types.  Choose what gitweb
# and cgit agree on for compatibility.
our %FMT_TYPES = (
	'tar' => 'application/x-tar',
	'tar.bz2' => 'application/x-bzip2',
	'tar.gz' => 'application/x-gzip',
	'tar.xz' => 'application/x-xz',
	'tgz' => 'application/x-gzip',
	'zip' => 'application/x-zip',
);

sub call_git_snapshot ($$) { # invoked by PublicInbox::RepoBase::call
	my ($self, $req) = @_;

	my $ref = $req->{tip} || $req->{-repo}->tip;
	my $orig_fn = $ref;

	# just in case git changes refname rules, don't allow wonky filenames
	# to break the Content-Disposition header, either.
	return $self->r(404) if $orig_fn =~ /["\s]/s;
	return $self->r(404) unless ($ref =~ s/\.($SUFFIX)\z//o);
	my $fmt = $1;
	my $env = $req->{env};
	my $repo = $req->{-repo};

	# support disabling certain snapshots types entirely to twart
	# URL guessing since it could burn server resources.
	return $self->r(404) if $repo->{snapshots_disabled}->{$fmt};

	# strip optional basename (may not exist)
	$ref =~ s/$repo->{snapshot_re}//;

	# don't allow option/command injection, git refs do not start with '-'
	return $self->r(404) if $ref =~ /\A-/;

	my $git = $repo->{git};
	my $tree = '';
	my $last_cb = sub {
		delete $env->{'repobrowse.tree_cb'};
		delete $env->{'qspawn.quiet'};
		my $pfx = "$repo->{snapshot_pfx}-$ref/";
		my $cmd = $git->cmd('archive',
				"--prefix=$pfx", "--format=$fmt", $tree);
		my $rdr = { 2 => $git->err_begin };
		my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
		$qsp->psgi_return($env, undef, sub {
			my $r = $_[0];
			return $self->r(500) unless $r;
			[ 200, [ 'Content-Type',
				$FMT_TYPES{$fmt} || 'application/octet-stream',
				'Content-Disposition',
					qq(inline; filename="$orig_fn"),
				'ETag', qq("$tree") ] ];
		});
	};

	my $cmd = $git->cmd(qw(rev-parse --verify --revs-only));
	# try prefixing "v" or "V" for tag names to get the tree
	my @refs = ("V$ref", "v$ref", $ref);
	$env->{'qspawn.quiet'} = 1;
	my $tree_cb = $env->{'repobrowse.tree_cb'} = sub {
		my ($ref) = @_;
		if (defined $ref) {
			$tree = $$ref;
			chomp $tree;
		}
		return $last_cb->() if $tree ne '';
		unless (scalar(@refs)) {
			my $res = delete $env->{'qspawn.response'};
			return $res->($self->r(404));
		}
		my $rdr = { 2 => $git->err_begin };
		my $r = pop @refs;
		my $qsp = PublicInbox::Qspawn->new([@$cmd, $r], undef, $rdr);
		$qsp->psgi_qx($env, undef, $env->{'repobrowse.tree_cb'});
	};
	sub {
		$env->{'qspawn.response'} = $_[0];
		# kick off the "loop" foreach @refs
		$tree_cb->(undef);
	}
}

1;
