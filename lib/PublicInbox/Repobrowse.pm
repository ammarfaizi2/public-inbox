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
use URI::Escape qw(uri_escape_utf8 uri_unescape);
use PublicInbox::RepobrowseConfig;

my %CMD = map { lc($_) => $_ } qw(Log Commit Tree Patch Blob Plain Tag);
my %VCS = (git => 'Git');
my %LOADED;

sub new {
	my ($class, $file) = @_;
	bless { rconfig => PublicInbox::RepobrowseConfig->new($file) }, $class;
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
		tslash => 0,
	};

	my $cmd = shift @extra;
	my $vcs_lc = $repo_info->{vcs};
	my $vcs = $VCS{$vcs_lc} or return r404();
	my $mod;
	if (defined $cmd && length $cmd) {
		$mod = $CMD{$cmd};
		unless ($mod) {
			unshift @extra, $cmd;
			$mod = 'Fallback';
		}
		$req->{relcmd} = '../' x scalar(@extra);
	} else {
		$mod = 'Summary';
		$cmd = 'summary';
		if ($path_info =~ m!/\z!) {
			$req->{tslash} = $path_info =~ tr!/!!;
			$req->{relcmd} = '';
		} else {
			my @repo = split('/', $repo_path);
			if (@repo > 1) {
				$req->{relcmd} = "./$repo[-1]/";
			} else {
				$req->{relcmd} = "/$repo[-1]/";
			}
		}
	}
	$mod = load_once("PublicInbox::Repobrowse$vcs$mod");
	$vcs = load_once("PublicInbox::$vcs");
	$repo_info->{$vcs_lc} ||= $vcs->new($repo_info->{path});
	while (@extra && $extra[-1] eq '') {
		pop @extra;
		++$req->{tslash};
	}
	$req->{expath} = join('/', @extra);
	my $rv = eval { $mod->new->call($cmd, $req) }; # RepobrowseBase::call
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
