# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoConfig;
use strict;
use warnings;
use PublicInbox::Config qw/try_cat/;
require PublicInbox::Hval;

sub new {
	my ($class, $file) = @_;
	$file = default_file() unless defined($file);
	my $self = bless PublicInbox::Config::git_config_dump($file), $class;
	$self->{-cache} = {};
	$self;
}

sub default_file {
	my $f = $ENV{PI_REPO_CONFIG};
	return $f if defined $f;
	PublicInbox::Config::config_dir() . '/repo_config';
}

# Returns something like:
# {
#	path => '/home/git/foo.git',
#	description => 'foo repo',
#	cloneurl => "git://example.com/foo.git\nhttp://example.com/foo.git",
#	publicinbox => '/home/pub/foo-public.git',
# }
sub lookup {
	my ($self, $repo_path) = @_; # "git.git"
	my $rv;

	$rv = $self->{-cache}->{$repo_path} and return $rv;

	my $path = $self->{"repo.$repo_path.path"};
	(defined $path && -d $path) or return;
	$rv->{path} = $path;
	$rv->{path_info} = $repo_path;

	foreach my $key (qw(description cloneurl)) {
		$rv->{$key} = try_cat("$path/$key");
	}

	$rv->{desc_html} =
		PublicInbox::Hval->new_oneline($rv->{description})->as_html;

	foreach my $key (qw(publicinbox vcs)) {
		$rv->{$key} = $self->{"repo.$repo_path.$key"};
	}

	# of course git is the default VCS
	$rv->{vcs} ||= 'git';
	$self->{-cache}->{$repo_path} = $rv;
}

1;
