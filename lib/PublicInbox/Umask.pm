# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# base class to ensures Xapian||SQLite files respect core.sharedRepository
# of git repos
package PublicInbox::Umask;
use v5.12;
use PublicInbox::OnDestroy;

use constant {
	PERM_UMASK => 0,
	OLD_PERM_GROUP => 1,
	OLD_PERM_EVERYBODY => 2,
	PERM_GROUP => 0660,
	PERM_EVERYBODY => 0664,
};

sub _read_git_config_perm {
	my ($self) = @_;
	chomp(my $perm = $self->git->qx('config', 'core.sharedRepository'));
	$perm;
}

sub _git_config_perm {
	my $self = shift;
	my $perm = scalar @_ ? $_[0] : _read_git_config_perm($self);
	$perm //= '';
	return PERM_UMASK if $perm eq '' || $perm eq 'umask';
	return PERM_GROUP if $perm eq 'group';
	return PERM_EVERYBODY if $perm =~ /\A(?:all|world|everybody)\z/;
	return PERM_GROUP if ($perm =~ /\A(?:true|yes|on|1)\z/);
	return PERM_UMASK if ($perm =~ /\A(?:false|no|off|0)\z/);

	my $i = oct($perm);
	return PERM_UMASK if $i == PERM_UMASK;
	return PERM_GROUP if $i == OLD_PERM_GROUP;
	return PERM_EVERYBODY if $i == OLD_PERM_EVERYBODY;

	if (($i & 0600) != 0600) {
		die "core.sharedRepository mode invalid: ".
		    sprintf('%.3o', $i) . "\nOwner must have permissions\n";
	}
	($i & 0666);
}

sub _umask_for {
	my ($perm) = @_; # _git_config_perm return value
	my $rv = $perm;
	return umask if $rv == 0;

	# set +x bit if +r or +w were set
	$rv |= 0100 if ($rv & 0600);
	$rv |= 0010 if ($rv & 0060);
	$rv |= 0001 if ($rv & 0006);
	(~$rv & 0777);
}

sub with_umask {
	my ($self, $cb, @arg) = @_;
	my $old = umask($self->{umask} //= umask_prepare($self));
	my $restore = PublicInbox::OnDestroy->new($$, \&CORE::umask, $old);
	$cb ? $cb->(@arg) : $restore;
}

sub umask_prepare {
	my ($self) = @_;
	_umask_for(_git_config_perm($self));
}

1;
