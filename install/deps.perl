# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Helper script for mass installing/uninstalling with the OS package manager
eval 'exec perl -S $0 ${1+"$@"}' # no shebang
if 0; # running under some shell
use v5.12;
my $help = <<EOM; # make sure this fits in 80x24 terminals
usage: $^X $0 [-f PKG_FMT] [--allow-remove] PROFILE [PROFILE_MOD]

  -f PKG_FMT      package format (`deb', `pkg', `pkg_add', `pkgin' or `rpm')
  --allow-remove  allow removing packages (DANGEROUS, non-production use only)
  --dry-run | -n  show commands that would be run
  --yes | -y      non-interactive mode / assume yes to package manager

PROFILE is typically `www-search', `lei', or `nntpd'
Some profile names are intended for developer use only and subject to change.
PROFILE_MOD is only for developers checking dependencies

OS package installation typically requires administrative privileges
EOM
use Getopt::Long qw(:config gnu_getopt no_ignore_case auto_abbrev);
BEGIN { require './install/os.perl' };
my $opt = {};
GetOptions($opt, qw(pkg-fmt|f=s allow-remove dry-run|n yes|y help|h))
	or die $help;
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
# highest granularity for things in the Perl standard library.
my $profiles = {
	# the smallest possible profile for testing
	essential => [ qw(
		autodie
		git
		perl
		Digest::SHA
		ExtUtils::MakeMaker
		IO::Compress
		Text::ParseWords
		URI
		), @test_essential ],

	# everything optional for normal use
	optional => [ qw(
		Date::Parse
		BSD::Resource
		DBD::SQLite
		Inline::C
		Mail::IMAPClient
		Net::Server
		Parse::RecDescent
		Plack
		Plack::Test
		Plack::Middleware::ReverseProxy
		Xapian
		curl
		highlight.pm
		libxapian
		pkg-config
		sqlite3
		xapian-tools
		) ],

	# optional developer stuff
	devtest => [ qw(
		XML::TreePP
		w3m
		Plack::Test::ExternalServer
		) ],
};

# only for distro-agnostic dependencies which are always true:
my $always_deps = {
	# we only load DBI explicitly
	'DBD::SQLite' => [ qw(DBI libsqlite3) ],
	'Mail::IMAPClient' => 'Parse::RecDescent',
	'Plack::Middleware::ReverseProxy' => 'Plack',
	'Xapian' => 'libxapian',
	'xapian-tools' => 'libxapian',
	'libxapian-dev' => [ qw(pkg-config libxapian) ],
	'libgit2-dev' => 'pkg-config',
};

# bare minimum for v2
$profiles->{v2essential} = [ @{$profiles->{essential}}, qw(DBD::SQLite) ];

# for old v1 installs
$profiles->{'www-v1'} = [ @{$profiles->{essential}}, qw(Plack) ];
$profiles->{'www-thread'} = [ @{$profiles->{v2essential}}, qw(Plack) ];

# common profile for PublicInbox::WWW
$profiles->{'www-search'} = [ @{$profiles->{'www-thread'}}, qw(Xapian) ];

# bare mininum for lei
$profiles->{'lei-core'} = [ @{$profiles->{v2essential}}, qw(Xapian) ];
push @{$profiles->{'lei-core'}}, 'Inline::C' if $^O ne 'linux';

# common profile for lei:
$profiles->{lei} = [ @{$profiles->{'lei-core'}}, qw(Mail::IMAPClient curl) ];

$profiles->{nntpd} = [ @{$profiles->{v2essential}} ];
$profiles->{pop3d} = [ @{$profiles->{v2essential}} ];
$profiles->{'imapd-bare'} = [ @{$profiles->{v2essential}},
				qw(Parse::RecDescent) ];
$profiles->{imapd} = [ @{$profiles->{'imapd-bare'}}, qw(Xapian) ];
$profiles->{pop3d} = [ @{$profiles->{v2essential}} ];
$profiles->{watch} = [ @{$profiles->{v2essential}}, qw(Mail::IMAPClient) ];
$profiles->{'watch-v1'} = [ @{$profiles->{essential}} ];
$profiles->{'watch-maildir'} = [ @{$profiles->{v2essential}} ];

# package names which can't be mapped automatically and explicit
# dependencies to prevent essential package removal:
my $non_auto = { # git and perl are essential
	git => {
		pkg => [ qw(curl p5-TimeDate git) ],
		rpm => [ qw(curl git) ],
		pkg_add => [ qw(curl p5-Time-TimeDate git) ],
	},
	perl => {
		pkg => 'perl5',
		pkgin => 'perl',
		pkg_add => [], # Perl is part of OpenBSD base
	},
	# optional stuff:
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
		pkg => 'p5-Xapian',
		pkg_add => 'xapian-bindings-perl',
		rpm => 'Search::Xapian', # 3rd-party repo
	},
	'highlight.pm' => {
		deb => 'libhighlight-perl',
		pkg => [],
		pkgin => 'p5-highlight',
		rpm => [],
	},

	# `libgit2' is the project name (since git has libgit)
	'libgit2-dev' => {
		pkg => 'libgit2',
		rpm => 'libgit2-devel',
	},

	# some distros have both sqlite 2 and 3, we've only ever used 3
	'libsqlite3' => {
		pkg => 'sqlite3',
		rpm => [], # `sqlite' is not removable due to yum/systemd
		deb => [], # libsqlite3-0, but no need to specify
	},

	# only one version of Xapian distros
	'libxapian' => { # avoid .so version numbers in our deps
		deb => [], # libxapian30 atm, but no need to specify
		pkg => 'xapian-core',
		pkgin => 'xapian',
		rpm => 'xapian-core',
	},
	'libxapian-dev' => {
		pkg => 'xapian-core',
		pkgin => 'xapian',
		rpm => 'xapian-core-devel',
	},
	'pkg-config' => {
		pkg_add => [], # part of the OpenBSD base system
		pkg => 'pkgconf', # pkg-config is a symlink to pkgconf
		pkgin => 'pkg-config',
	},
	'sqlite3' => { # this is just the executable binary on deb
		rpm => [], # `sqlite' is not removable due to yum/systemd
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

# standard library stuff that CentOS 7.x (and presumably other RPM)
# split out and can be removed without removing the `perl' RPM:
for (qw(autodie Digest::SHA ExtUtils::MakeMaker IO::Compress Sys::Syslog
		Test::Simple Text::ParseWords)) {
	# n.b.: Compress::Raw::Zlib is pulled in by IO::Compress
	# qw(constant Encode Getopt::Long Exporter Storable Time::HiRes)
	# don't need to be here since it's impossible to have `perl'
	# on CentOS 7.x without them.
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
	my @extra;
	for my $pkg (@$ary) {
		my $deps = $always_deps->{$pkg} // next;
		push @extra, list($deps);
	}
	push @$ary, @extra;
	$all{$_} = \@pkg_remove for @$ary;
}
if ($^O =~ /\A(?:free|net|open)bsd\z/) {
	$all{'IO::KQueue'} = \@pkg_remove;
}
$profiles->{all} = [ keys %all ]; # pseudo-profile for all packages

# parse the profile list from the command-line
my @profiles = @ARGV;
while (defined(my $profile = shift @profiles)) {
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

my (%add, %rm); # uniquify lists
@pkg_install = grep { !$add{$_}++ && !$INST_CHECK->($_) } @pkg_install;
@pkg_remove = $opt->{'allow-remove'} ? grep {
		!$add{$_} && !$rm{$_}++ && $INST_CHECK->($_)
	} @pkg_remove : ();

# OS-specific cleanups appreciated
if ($pkg_fmt eq 'deb') {
	my @apt_opt = qw(-o APT::Install-Recommends=false
			-o APT::Install-Suggests=false);
	push @apt_opt, '-y' if $opt->{yes};
	root('apt-get', @apt_opt, qw(install),
		@pkg_install,
		# apt-get lets you suffix a package with "-" to
		# remove it in an "install" sub-command:
		map { "$_-" } @pkg_remove);
	root('apt-get', @apt_opt, qw(autoremove)) if $opt->{'allow-remove'};
} elsif ($pkg_fmt eq 'pkg') { # FreeBSD
	my @pkg_opt = $opt->{yes} ? qw(-y) : ();

	# don't remove stuff that isn't installed:
	root(qw(pkg remove), @pkg_opt, @pkg_remove) if @pkg_remove;
	root(qw(pkg install), @pkg_opt, @pkg_install) if @pkg_install;
	root(qw(pkg autoremove), @pkg_opt) if $opt->{'allow-remove'};
} elsif ($pkg_fmt eq 'pkgin') { # NetBSD
	my @pkg_opt = $opt->{yes} ? qw(-y) : ();
	root(qw(pkgin), @pkg_opt, 'remove', @pkg_remove) if @pkg_remove;
	root(qw(pkgin), @pkg_opt, 'install', @pkg_install) if @pkg_install;
	root(qw(pkgin), @pkg_opt, 'autoremove') if $opt->{'allow-remove'};
# TODO: yum / rpm support
} elsif ($pkg_fmt eq 'rpm') {
	my @pkg_opt = $opt->{yes} ? qw(-y) : ();
	root(qw(yum remove), @pkg_opt, @pkg_remove) if @pkg_remove;
	root(qw(yum install), @pkg_opt, @pkg_install) if @pkg_install;
} elsif ($pkg_fmt eq 'pkg_add') { # OpenBSD
	my @pkg_opt = $opt->{yes} ? qw(-I) : (); # -I means non-interactive
	root(qw(pkg_delete -a), @pkg_opt); # autoremove unspecified
	@pkg_install = map { "$_--" } @pkg_install; # disambiguate w3m
	root(qw(pkg_add), @pkg_opt, @pkg_install) if @pkg_install;
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
