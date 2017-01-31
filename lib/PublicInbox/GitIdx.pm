# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::GitIdx;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT = qw(git_umask_for with_umask);
use PublicInbox::Git;
use constant {
	PERM_UMASK => 0,
	OLD_PERM_GROUP => 1,
	OLD_PERM_EVERYBODY => 2,
	PERM_GROUP => 0660,
	PERM_EVERYBODY => 0664,
};

sub _git_config_perm ($) {
	my ($git) = @_;
	my @cmd = qw(config core.sharedRepository);
	$git = PublicInbox::Git->new($git) unless ref $git;
	my $perm = $git->qx(@cmd);
	chomp $perm if defined $perm;
	return PERM_GROUP if (!defined($perm) || $perm eq '');
	return PERM_UMASK if ($perm eq 'umask');
	return PERM_GROUP if ($perm eq 'group');
	if ($perm =~ /\A(?:all|world|everybody)\z/) {
		return PERM_EVERYBODY;
	}
	return PERM_GROUP if ($perm =~ /\A(?:true|yes|on|1)\z/);
	return PERM_UMASK if ($perm =~ /\A(?:false|no|off|0)\z/);

	my $i = oct($perm);
	return PERM_UMASK if ($i == PERM_UMASK);
	return PERM_GROUP if ($i == OLD_PERM_GROUP);
	return PERM_EVERYBODY if ($i == OLD_PERM_EVERYBODY);

	if (($i & 0600) != 0600) {
		die "core.sharedRepository mode invalid: ".
		    sprintf('%.3o', $i) . "\nOwner must have permissions\n";
	}
	($i & 0666);
}

sub git_umask_for ($) {
	my ($git) = @_;
	my $perm = _git_config_perm($git);
	my $rv = $perm;
	return umask if $rv == 0;

	# set +x bit if +r or +w were set
	$rv |= 0100 if ($rv & 0600);
	$rv |= 0010 if ($rv & 0060);
	$rv |= 0001 if ($rv & 0006);
	(~$rv & 0777);
}

sub with_umask ($$) {
	my ($umask, $cb) = @_;
	my $old = umask $umask;
	my $rv = eval { $cb->() };
	my $err = $@;
	umask $old;
	die $err if $@;
	$rv;
}

1;
