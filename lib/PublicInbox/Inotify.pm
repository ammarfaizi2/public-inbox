# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# wrap Linux::Inotify2 XS module, support pure Perl via `syscall' someday
package PublicInbox::Inotify;
use v5.12;
our @ISA;
BEGIN {
	eval { require Linux::Inotify2 };
	if ($@) { # TODO: get rid of XS dependency
		die "W: Linux::Inotify2 missing: $@\n";
	} else {
		push @ISA, 'Linux::Inotify2';
	}
};

sub new {
	$_[0]->SUPER::new // do {
		my $msg = $!{EMFILE} ? <<EOM : "$_[0]->new: $!\n";
inotify_init/inotify_init1: $!
You may need to raise the `fs.inotify.max_user_instances' sysctl limit.
Consult your OS documentation and/or sysctl(8) + sysctl.conf(5) manpages.
EOM
		$msg =~ s/^/E: /smg;
		require Carp;
		Carp::croak($msg);
	}
}

1;
