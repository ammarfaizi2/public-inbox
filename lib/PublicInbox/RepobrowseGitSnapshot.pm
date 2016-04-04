# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /snapshot/ endpoint for git repositories
# Mainly for compatibility reasons with cgit, I'm unsure if
# showing this in a repository viewer is a good idea.

package PublicInbox::RepobrowseGitSnapshot;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::Git;
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

sub call_git_snapshot ($$) { # invoked by PublicInbox::RepobrowseBase::call
	my ($self, $req) = @_;

	my @extra = @{$req->{extra}};
	my $ref = shift @extra;
	return $self->r(404) if (!defined $ref) || scalar(@extra);
	my $orig_fn = $ref;

	# just in case git changes refname rules, don't allow wonky filenames
	# to break the Content-Disposition header, either.
	return $self->r(404) if $orig_fn =~ /["\s]/s;
	return $self->r(404) unless ($ref =~ s/\.($SUFFIX)\z//o);
	my $fmt = $1;

	my $repo_info = $req->{repo_info};

	# support disabling certain snapshots types entirely to twart
	# URL guessing since it could burn server resources.
	return $self->r(404) if $repo_info->{snapshots_disabled}->{$fmt};

	# strip optional basename (may not exist)
	$ref =~ s/$repo_info->{snapshot_re}//;

	# don't allow option/command injection, git refs do not start with '-'
	return $self->r(404) if $ref =~ /\A-/;

	my $git = $repo_info->{git};
	my $tree;

	# try prefixing "v" or "V" for tag names
	foreach my $r ($ref, "v$ref", "V$ref") {
		$tree = $git->qx([qw(rev-parse --verify --revs-only), $r],
				 undef, { 2 => $git->err_begin });
		if (defined $tree) {
			chomp $tree;
			last if $tree ne '';
		}
	}
	return $self->r(404) if (!defined $tree || $tree eq '');

	my $pfx = "$repo_info->{snapshot_pfx}-$ref/";
	my @cmd = ('archive', "--prefix=$pfx", "--format=$fmt", $tree);
	$req->{rpipe} = $git->popen(\@cmd, undef, { 2 => $git->err_begin });

	my $env = $req->{cgi}->env;
	my $vin;
	my $end = sub {
		my ($n) = @_;
		if (my $fh = delete $req->{fh}) {
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
			select($vin, undef, undef, undef) if $vin;
			# $vin is undef on async, so this is a noop
			return;
		}
		my $e = $!;
		$end->();
		my $err = $env->{'psgi.errors'};
		$err->print("git archive ($git->{git_dir}): $e\n");
	};
	my $cb = sub {
		my $n = $req->{rpipe}->sysread(my $buf, 65536);
		return $fail->() unless defined $n;
		return $end->() if $n == 0;
		if (my $res = delete $req->{res}) {
			my $h = [ 'Content-Type',
				$FMT_TYPES{$fmt} || 'application/octet-stream',
				'Content-Disposition',
				qq(inline; filename="$orig_fn"),
				'ETag', qq("$tree") ];
			$req->{fh} = $res->([200, $h]);
		}
		my $fh = $req->{fh} or return;
		$fh->write($buf);
	};
	if (my $async = $env->{'pi-httpd.async'}) {
		$req->{rpipe} = $async->($req->{rpipe}, $cb);
		sub { $req->{res} = $_[0] } # let Danga::Socket handle the rest.
	} else { # synchronous loop for other PSGI servers
		$vin = '';
		vec($vin, fileno($req->{rpipe}), 1) = 1;
		sub {
			$req->{res} = $_[0]; # Plack response callback
			while ($req->{rpipe}) { $cb->() }
		}
	}
}

1;
