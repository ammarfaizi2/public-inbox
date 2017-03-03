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
# In other words, repobrowse must work for repositories without
# any public-inbox at all; or with multiple public-inboxes.
# And the rest of public-inbox will always work without a "normal"
# code repo for the project.

package PublicInbox::Repobrowse;
use strict;
use warnings;
use PublicInbox::RepoConfig;

my %CMD = map { lc($_) => $_ } qw(Log Commit Src Patch Raw Tag Atom
	Diff Snapshot);
my %VCS = (git => 'Git');
my %LOADED;

sub new {
	my ($class, $rconfig) = @_;
	$rconfig ||= PublicInbox::RepoConfig->new;
	bless { rconfig => $rconfig }, $class;
}

# simple response for errors
sub r { [ $_[0], ['Content-Type' => 'text/plain'], [ join(' ', @_, "\n") ] ] }

sub base_url ($) {
	my ($env) = @_;
	my $scheme = $env->{'psgi.url_scheme'} || 'http';
	my $host = $env->{HTTP_HOST};
	my $base = "$scheme://";
	if (defined $host) {
		$base .= $host;
	} else {
		$base .= $env->{SERVER_NAME};
		my $port = $env->{SERVER_PORT} || 80;
		if (($scheme eq 'http' && $port != 80) ||
				($scheme eq 'https' && $port != 443)) {
			$base.= ":$port";
		}
	}
	$base .= $env->{SCRIPT_NAME};
}

# return a PSGI response if the URL is ambiguous with
# extra slashes or has a trailing slash
sub disambiguate_uri {
	my ($env) = @_;
	my $redirect;
	my $uri = $env->{REQUEST_URI};
	my $qs = '';
	$uri =~ s!\A([^:]+://)!!;
	my $scheme = $1 || '';
	if ($uri =~ s!/(\?.+)?\z!!) { # no trailing slashes
		$qs = $1 if defined $1;
		$redirect = 1;
	}
	if ($uri =~ s!//+!/!g) { # no redundant slashes
		$redirect = 1;
	}
	return unless $redirect;
	$uri = ($scheme ? $scheme : base_url($env)) . $uri . $qs;
	[ 301,
	  [ 'Location', $uri, 'Content-Type', 'text/plain' ],
	  [ "Redirecting to $uri\n" ] ]
}

sub root_index {
	my ($self) = @_;
	my $mod = load_once('PublicInbox::RepoRoot');
	$mod->new->call($self->{rconfig}); # RepoRoot::call
}

# PSGI entry point
sub call {
	my ($self, $env) = @_;
	my $method = $env->{REQUEST_METHOD};
	return r(405, 'Method Not Allowed') if ($method !~ /\AGET|HEAD|POST\z/);
	if (my $res = disambiguate_uri($env)) {
		return $res;
	}

	# URL syntax: / repo [ / cmd [ / head [ / path ] ] ]
	# cmd: log | commit | diff | src | raw | snapshot
	# repo and path (@extra) may both contain '/'
	my $path_info = $env->{PATH_INFO};
	my (undef, $repo_path, @extra) = split(m{/+}, $path_info, -1);

	return $self->root_index($self) unless length($repo_path);

	my $rconfig = $self->{rconfig}; # RepoConfig
	my $repo;
	until ($repo = $rconfig->lookup($repo_path)) {
		my $p = shift @extra or last;
		$repo_path .= "/$p";
	}
	return r404() unless $repo;

	my $req = {
		-repo => $repo,
		extra => \@extra, # path
		rconfig => $rconfig,
		env => $env,
	};
	my $cmd = shift @extra;
	my $vcs_lc = $repo->{vcs};
	my $vcs = $VCS{$vcs_lc} or return r404();
	my $mod;
	my $tip;
	if (defined $cmd && length $cmd) {
		$mod = $CMD{$cmd};
		if ($mod) {
			$tip = shift @extra if @extra;
		} else {
			unshift @extra, $cmd;
			$mod = 'Fallback';
		}
		$req->{relcmd} = '../' x (scalar(@extra) + 1);
	} else {
		$mod = 'Summary';
		$cmd = 'summary';
		if ($path_info =~ m!/\z!) {
			$path_info =~ tr!/!!;
		} else {
			my @x = split('/', $repo_path);
			$req->{relcmd} = @x > 1 ? "./$x[-1]/" : "/$x[-1]/";
		}
	}
	while (@extra && $extra[-1] eq '') {
		pop @extra;
	}
	$req->{tip} = $tip;
	$mod = load_once("PublicInbox::Repo$vcs$mod");
	$vcs = load_once("PublicInbox::$vcs");

	# $repo->{git} ||= PublicInbox::Git->new(...)
	$repo->{$vcs_lc} ||= $vcs->new($repo->{path});

	$req->{expath} = join('/', @extra);
	my $rv = eval { $mod->new->call($cmd, $req) }; # RepoBase::call
	$rv || r404();
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
