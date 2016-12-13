# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepobrowseConfig;
use strict;
use warnings;
use PublicInbox::Inbox;
use PublicInbox::Config;
require PublicInbox::Hval;

sub new {
	my ($class, $file) = @_;
	$file = default_file() unless defined($file);
	my $self = bless PublicInbox::Config::git_config_dump($file), $class;
	$self->{-cache} = {};

	# hard disable these with '-' prefix by default:
	$self->{'repobrowse.snapshots'} ||= '-tar.bz2 -tar.xz';

	# for root
	$self->{-groups} = { -hidden => [], -none => [] };
	$self;
}

sub default_file {
	my $f = $ENV{REPOBROWSE_CONFIG};
	return $f if defined $f;
	PublicInbox::Config::config_dir() . '/repobrowse_config';
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
	$rv->{repo} = $repo_path;

	# snapshots:
	my $snap = (split('/', $repo_path))[-1];
	$snap =~ s/\.git\z//; # seems common for git URLs to end in ".git"
	$rv->{snapshot_re} = qr/\A\Q$snap\E[-_]/;
	$rv->{snapshot_pfx} = $snap;

	# gitweb compatibility
	foreach my $key (qw(description cloneurl)) {
		$rv->{$key} = PublicInbox::Inbox::try_cat("$path/$key");
	}

	$rv->{desc_html} =
		PublicInbox::Hval->new_oneline($rv->{description})->as_html;

	foreach my $key (qw(publicinbox vcs readme group snapshots)) {
		$rv->{$key} = $self->{"repo.$repo_path.$key"};
	}
	unless (defined $rv->{snapshots}) {
		$rv->{snapshots} = $self->{'repobrowse.snapshots'} || '';
	}

	my %disabled;
	foreach (split(/\s+/, $rv->{snapshots})) {
		s/\A-// and $disabled{$_} = 1;
	}
	$rv->{snapshots_disabled} = \%disabled;

	my $g = $rv->{group};
	defined $g or $g = '-none';
	if (ref($g) eq 'ARRAY') {
		push @{$self->{-groups}->{$_} ||= []}, $repo_path foreach @$g;
	} else {
		push @{$self->{-groups}->{$g} ||= []}, $repo_path;
	}

	# of course git is the default VCS
	$rv->{vcs} ||= 'git';
	$self->{-cache}->{$repo_path} = $rv;
}

1;
