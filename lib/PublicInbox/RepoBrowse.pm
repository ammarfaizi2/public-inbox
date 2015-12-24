# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Version control system (VCS) repository viewer like cgit or gitweb,
# but with optional public-inbox archive integration.
# This uses cgit-compatible PATH_INFO URLs.
# This may be expanded to support other Free Software VCSes such as
# Subversion and Mercurial, so not just git
#
# Same web design principles as PublicInbox::WWW for supporting the
# lowest common denominators (see bottom of Documentation/design_www.txt)
#
# This allows an M:N relationship between "normal" repos for project
# and public-inbox (ssoma) git repositories where N may be zero.
# In other words, RepoBrowse must work for repositories without
# any public-inbox at all; or with multiple public-inboxes.
# And the rest of public-inbox will always work without a "normal"
# code repo for the project.

package PublicInbox::RepoBrowse;
use strict;
use warnings;
use URI::Escape qw(uri_escape_utf8 uri_unescape);
use PublicInbox::RepoConfig;

my %CMD = map { lc($_) => $_ } qw(Log Commit Tree Patch);
my %VCS = (git => 'Git');
my %LOADED;

sub new {
	my ($class, $file) = @_;
	bless { rconfig => PublicInbox::RepoConfig->new($file) }, $class;
}

# simple response for errors
sub r { [ $_[0], ['Content-Type' => 'text/plain'], [ join(' ', @_, "\n") ] ] }

sub run {
	my ($self, $cgi, $method) = @_;
	return r(405, 'Method Not Allowed') if ($method !~ /\AGET|HEAD\z/);

	# URL syntax: / repo [ / cmd [ / path ] ]
	# cmd: log | commit | diff | tree | view | blob | snapshot
	# repo and path (@extra) may both contain '/'
	my $rconfig = $self->{rconfig};
	my $path_info = uri_unescape($cgi->path_info);
	my (undef, $repo_path, @extra) = split(m{/+}, $path_info, -1);

	return r404() unless $repo_path;
	my $repo_info;
	until ($repo_info = $rconfig->lookup($repo_path)) {
		my $p = shift @extra or last;
		$repo_path .= "/$p";
	}
	return r404() unless $repo_info;

	my $req = {
		repo_info => $repo_info,
		extra => \@extra, # path
		cgi => $cgi,
		rconfig => $rconfig,
	};

	my $cmd = shift @extra;
	if (defined $cmd && length $cmd) {
		my $vcs_lc = $repo_info->{vcs};
		my $vcs = $VCS{$vcs_lc} or return r404();
		my $mod = $CMD{$cmd} or return r404();
		return r404() unless defined $mod && defined $vcs;
		$mod = load_once("PublicInbox::RepoBrowse$vcs$mod");
		$vcs = load_once("PublicInbox::$vcs");
		$repo_info->{$vcs_lc} ||= $vcs->new($repo_info->{path});

		$req->{relcmd} = '../' x scalar(@extra);
		my $rv = eval { $mod->new->call($cmd, $req) };
		$rv || r404();
	} else {
		$req->{relcmd} = defined $cmd ? ''  : './';
		summary($req);
	}
}

sub summary {
	r404();
}

sub r404 { r(404, 'Not Found') }

sub load_once {
	my ($mod) = @_;

	return $mod if $LOADED{$mod};
	eval "require $mod";
	$LOADED{$mod} = 1 unless $@;
	$mod;
}

1;
