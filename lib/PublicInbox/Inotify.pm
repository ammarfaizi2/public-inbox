# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# loads either pure Perl inotify support or wrap Linux::Inotify2 XS module
package PublicInbox::Inotify;
use v5.12;
our @ISA;
BEGIN { # prefer pure Perl since it works out-of-the-box
	my $isa;
	for my $m (qw(PublicInbox::Inotify3 Linux::Inotify2)) {
		eval "require $m";
		next if $@;
		$isa = $m;
		last;
	}
	if ($isa) {
		push @ISA, $isa;
		my $buf = '';
		for (qw(IN_MOVED_TO IN_CREATE IN_DELETE IN_DELETE_SELF
				IN_MOVE_SELF IN_MOVED_FROM IN_MODIFY)) {
			$buf .= "*$_ = \\&${isa}::$_;\n";
		}
		eval $buf;
		die $@ if $@;
	} else {
		die <<EOM;
W: inotify syscall numbers unknown on your platform and
W: Linux::Inotify2 missing: $@
W: public-inbox hackers welcome the plain-text output of ./devel/sysdefs-list
W: at meta\@public-inbox.org
EOM
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
