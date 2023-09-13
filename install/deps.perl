# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Helper script for mass installing/uninstalling with the OS package manager
eval 'exec perl -S $0 ${1+"$@"}' # no shebang
if 0; # running under some shell
use v5.12;
my $help = <<EOM;
usage: $^X $0 [-f PKG_FMT] [--allow-remove] PROFILE [PROFILE_MOD]

  -f PKG_FMT      package format (`deb', `pkg', `pkg_add', `pkgin' or `rpm')
  --allow-remove  allow removing packages (for development use only)
  --dry-run | -n  show commands that would be run

PROFILE is typically `all'.  Other profiles are subject to change.
PROFILE_MOD is only for developers checking dependencies

OS package installation typically requires administrative privileges
EOM
use Getopt::Long qw(:config gnu_getopt no_ignore_case auto_abbrev);
BEGIN { require './install/os.perl' };
my $opt = {};
GetOptions($opt, qw(pkg-fmt|f=s allow-remove dry-run|n help|h)) or die $help;
if ($opt->{help}) { print $help; exit }
my $pkg_fmt = $opt->{'pkg-fmt'} // do {
	my $fmt = pkg_fmt;
	warn "# using detected --pkg-fmt=$fmt on $ID/$VERSION_ID\n";
	$fmt;
};
@ARGV or die $help;
my @test_essential = qw(Test::Simple); # we actually use Test::More

# package profiles.  Note we specify packages at maximum granularity,
# which is typically deb for most things, but rpm seems to have the
# highest granularity for things in the Prl standard library.
my $profiles = {
	# the smallest possible profile for testing
	essential => [ qw(
		git
		perl
		Digest::SHA
		Encode
		ExtUtils::MakeMaker
		IO::Compress
		URI
		), @test_essential ],

	# everything optional for normal use
	optional => [ qw(
		Date::Parse
		BSD::Resource
		DBD::SQLite
		DBI
		Inline::C
		Mail::IMAPClient
		Net::Server
		Parse::RecDescent
		Plack
		Plack::Test
		Plack::Middleware::ReverseProxy
		Xapian
		Socket6
		highlight.pm
		xapian-tools
		) ],

	# optional developer stuff
	devtest => [ qw(
		XML::TreePP
		curl
		w3m
		Plack::Test::ExternalServer
		) ],
};

# bare minimum for v2
$profiles->{v2essential} = [ @{$profiles->{essential}}, qw(DBD::SQLite DBI) ];

# package names which can't be mapped automatically and explicit
# dependencies to prevent essential package removal:
my $non_auto = {
	git => {
		pkg => [ qw(curl p5-Socket6 p5-TimeDate git) ],
		rpm => [ qw(curl git) ],
		pkg_add => [ qw(curl p5-Socket6 p5-Time-TimeDate git) ],
	},
	perl => {
		pkg => 'perl5',
		pkgin => 'perl',
		pkg_add => [], # Perl is part of OpenBSD base
	},
	'Date::Parse' => {
		deb => 'libtimedate-perl',
		pkg => 'p5-TimeDate',
		rpm => 'perl-TimeDate',
		pkg_add => 'p5-Time-TimeDate',
	},
	'Inline::C' => {
		pkg_add => 'p5-Inline', # tested OpenBSD 7.3
		rpm => 'perl-Inline', # for CentOS 7.x, at least
	},
	'DBD::SQLite' => { deb => 'libdbd-sqlite3-perl' },
	'Plack::Test' => {
		deb => 'libplack-perl',
		pkg => 'p5-Plack',
	},
	'Xapian' => {
		deb => 'libsearch-xapian-perl',
		pkg => [qw(xapian-core p5-Xapian)],
		pkg_add => [qw(xapian-core xapian-bindings-perl)],
		pkgin => [qw(xapian p5-Xapian)],
		rpm => 'Search::Xapian', # 3rd-party repo
	},
	'highlight.pm' => {
		deb => 'libhighlight-perl',
		pkg => [],
		pkgin => 'p5-highlight',
		rpm => [],
	},

	# we call xapian-compact(1) in public-inbox-compact(1) and
	# xapian-delve(1) in public-inbox-cindex(1)
	'xapian-tools' => {
		pkg => 'xapian-core',
		pkgin => 'xapian',
		rpm => 'xapian-core', # ???
	},

	# OS-specific
	'IO::KQueue' => {
		deb => [],
		rpm => [],
	},
};

# standard library stuff that CentOS 7.x (and presumably other RPM) split out:
for (qw(Digest::SHA Encode ExtUtils::MakeMaker IO::Compress Test::Simple)) {
	$non_auto->{$_} = {
		deb => 'perl', # libperl5.XX, but the XX varies
		pkg => 'perl5',
		pkg_add => [], # perl is in the OpenBSD base system
		pkgin => 'perl',
	};
}

# NetBSD and OpenBSD package names are similar to FreeBSD in most cases
if ($pkg_fmt =~ /\A(?:pkg_add|pkgin)\z/) {
	for my $name (keys %$non_auto) {
		my $fbsd_pkg = $non_auto->{$name}->{pkg};
		$non_auto->{$name}->{$pkg_fmt} //= $fbsd_pkg if $fbsd_pkg;
	}
}

my %inst_check = ( # subs which return true if a package is intalled
	pkg => sub { system(qw(pkg info -q), $_[0]) == 0 },
	deb => sub { system("dpkg -s $_[0] >/dev/null 2>&1") == 0 },
	pkg_add => sub { system(qw(pkg_info -q -e), "$_[0]->=0") == 0 },
	pkgin => sub { system(qw(pkg_info -q -e), $_[0]) == 0 },
	rpm => sub { system("rpm -qs $_[0] >/dev/null 2>&1") == 0 },
);

our $INST_CHECK = $inst_check{$pkg_fmt} || die <<"";
don't know how to check install status for $pkg_fmt

my (@pkg_install, @pkg_remove, %all);
for my $ary (values %$profiles) {
	$all{$_} = \@pkg_remove for @$ary;
}
if ($^O =~ /\A(?:free|net|open)bsd\z/) {
	$all{'IO::KQueue'} = \@pkg_remove;
}
$profiles->{all} = [ keys %all ]; # pseudo-profile for all packages

# parse the profile list from the command-line
for my $profile (@ARGV) {
	if ($profile =~ s/-\z//) {
		# like apt-get, trailing "-" means remove
		profile2dst($profile, \@pkg_remove);
	} else {
		profile2dst($profile, \@pkg_install);
	}
}

# fill in @pkg_install and @pkg_remove:
while (my ($pkg, $dst_pkg_list) = each %all) {
	push @$dst_pkg_list, list(pkg2ospkg($pkg, $pkg_fmt));
}

my %inst = map { $_ => 1 } @pkg_install;
@pkg_remove = $opt->{'allow-remove'} ? grep { !$inst{$_} } @pkg_remove : ();
@pkg_install = grep { !$INST_CHECK->($_) } @pkg_install;

my @apt_opts =
	qw(-o APT::Install-Recommends=false -o APT::Install-Suggests=false);

# OS-specific cleanups appreciated
if ($pkg_fmt eq 'deb') {
	my @quiet = $ENV{V} ? () : ('-q');
	root('apt-get', @apt_opts, qw(install --purge -y), @quiet,
		@pkg_install,
		# apt-get lets you suffix a package with "-" to
		# remove it in an "install" sub-command:
		map { "$_-" } @pkg_remove);
	root('apt-get', @apt_opts, qw(autoremove --purge -y), @quiet);
} elsif ($pkg_fmt eq 'pkg') { # FreeBSD
	my @quiet = $ENV{V} ? () : ('-q');

	# don't remove stuff that isn't installed:
	exclude_uninstalled(\@pkg_remove);
	root(qw(pkg remove -y), @quiet, @pkg_remove) if @pkg_remove;
	root(qw(pkg install -y), @quiet, @pkg_install) if @pkg_install;
	root(qw(pkg autoremove -y), @quiet);
} elsif ($pkg_fmt eq 'pkgin') { # NetBSD
	my @quiet = $ENV{V} ? ('-'.('V'x$ENV{V})) : ();
	exclude_uninstalled(\@pkg_remove);
	root(qw(pkgin -y), @quiet, 'remove', @pkg_remove) if @pkg_remove;
	root(qw(pkgin -y), @quiet, 'install', @pkg_install) if @pkg_install;
	root(qw(pkgin -y), @quiet, 'autoremove');
# TODO: yum / rpm support
} elsif ($pkg_fmt eq 'rpm') {
	my @quiet = $ENV{V} ? () : ('-q');
	exclude_uninstalled(\@pkg_remove);
	root(qw(yum remove -y), @quiet, @pkg_remove) if @pkg_remove;
	root(qw(yum install -y), @quiet, @pkg_install) if @pkg_install;
} elsif ($pkg_fmt eq 'pkg_add') { # OpenBSD
	exclude_uninstalled(\@pkg_remove);
	my @quiet = $ENV{V} ? ('-'.('v'x$ENV{V})) : qw(-x); # -x : no progress
	if (@pkg_remove) {
		my @lifo = qw(xapian-bindings-perl);
		for my $dep (@lifo) {
			grep(/\A\Q$dep\E\z/, @pkg_remove) or next;
			root(qw(pkg_delete -I), @quiet, $dep);
			@pkg_remove = grep(!/\A\Q$dep\E\z/, @pkg_remove);
		}
		root(qw(pkg_delete -I), @quiet, @pkg_remove);
	}
	root(qw(pkg_delete -a), @quiet);
	@pkg_install = map { "$_--" } @pkg_install; # disambiguate w3m
	root(qw(pkg_add), @quiet, @pkg_install) if @pkg_install;
} else {
	die "unsupported package format: $pkg_fmt\n";
}
exit 0;


# map a generic package name to an OS package name
sub pkg2ospkg {
	my ($pkg, $fmt) = @_;

	# check explicit overrides, first:
	if (my $ospkg = $non_auto->{$pkg}->{$fmt}) {
		return $ospkg;
	}

	# check common Perl module name patterns:
	if ($pkg =~ /::/ || $pkg =~ /\A[A-Z]/) {
		if ($fmt eq 'deb') {
			$pkg =~ s/::/-/g;
			$pkg =~ tr/A-Z/a-z/;
			return "lib$pkg-perl";
		} elsif ($fmt eq 'rpm') {
			$pkg =~ s/::/-/g;
			return "perl-$pkg"
		} elsif ($fmt =~ /\Apkg(?:_add|in)?\z/) {
			$pkg =~ s/::/-/g;
			return "p5-$pkg"
		} else {
			die "unsupported package format: $fmt for $pkg\n"
		}
	}

	# use package name as-is (e.g. 'curl' or 'w3m')
	$pkg;
}

# maps a install profile to a package list (@pkg_remove or @pkg_install)
sub profile2dst {
	my ($profile, $dst_pkg_list) = @_;
	if (my $pkg_list = $profiles->{$profile}) {
		$all{$_} = $dst_pkg_list for @$pkg_list;
	} elsif ($all{$profile}) { # $profile is just a package name
		$all{$profile} = $dst_pkg_list;
	} else {
		die "unrecognized profile or package: $profile\n";
	}
}

sub exclude_uninstalled {
	my ($list) = @_;
	my (@tmp, %seen);
	for my $pkg (@$list) {
		push @tmp, $pkg if !$seen{$pkg}++ && $INST_CHECK->($pkg);
	}
	@$list = @tmp;
}

sub root {
	warn "# @_\n";
	return if $opt->{'dry-run'};
	return if system(@_) == 0;
	warn "E: command failed: @_\n";
	exit($? >> 8);
}

# ensure result can be pushed into an array:
sub list {
	my ($pkg) = @_;
	ref($pkg) eq 'ARRAY' ? @$pkg : $pkg;
}
