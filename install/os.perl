# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Helper library for detecting distro info and mapping to package manager.
# This should NOT be installed via `make install'.
# This is used by install/deps.perl and ci/profiles.perl
package PublicInbox::InstallOS;
use v5.12;
use parent qw(Exporter);
our ($ID, $PRETTY_NAME, $VERSION_ID); # same vars as os-release(5)
our @EXPORT = qw($ID $VERSION_ID pkg_fmt);

my ($release, $version); # from uname
if ($^O eq 'linux') { # try using os-release(5)
	for my $f (qw(/etc/os-release /usr/lib/os-release)) {
		next unless -f $f;
		my @echo = map {
			qq{echo "\$"$_" = qq[\$$_];"; }
		} qw(ID PRETTY_NAME VERSION_ID);
		# rely on sh(1) to handle interpolation and such:
		my $vars = `sh -c '. $f; @echo'`;
		die "sh \$?=$?" if $?;
		eval $vars;
		die $@ if $@;
		$VERSION_ID //= '';
		$ID //= '';
		if ($ID eq 'debian' && $VERSION_ID eq '') {
			if ($PRETTY_NAME =~ m!/sid\z!) {
				$VERSION_ID = 'sid';
			} else {
				open my $fh, '<', $f or die "open($f): $!";
				my $msg = do { local $/; <$fh> };
				die <<EOM;
ID=$ID, but no VERSION_ID
==> $f <==
$msg
EOM
			}
		}
		last if $ID ne '' && $VERSION_ID ne '';
	}
	$ID = 'linux' if $ID eq ''; # cf. os-release(5)
} elsif ($^O =~ m!\A(?:free|net|open)bsd\z! || $^O eq 'dragonfly') {
	$ID = $^O;
	require POSIX;
	(undef, undef, $release, $version) = POSIX::uname();
	$VERSION_ID = lc $release;
	$VERSION_ID =~ s/[^0-9a-z\.\_\-]//sg; # cf. os-release(5)
} else { # only support POSIX-like and Free systems:
	die "$^O unsupported";
}
$VERSION_ID //= 0; # numeric? could be 'sid', actually...
my %MIN_VER = ( # likely older versions work for many of these...
	alpine => v3.19,
	dragonfly => v6.4,
	freebsd => v11,
	netbsd => v9.3,
	openbsd => v7.3,
);

if (defined(my $min_ver = $MIN_VER{$^O})) {
	my $vid = $VERSION_ID;
	$vid =~ s/-.*\z//s; # no dashes in v-strings
	my $vstr = eval "v$vid";
	die "can't convert VERSION_ID=$VERSION_ID to v-string" if $@;
	die <<EOM if $vstr lt $min_ver;
ID=$ID VERSION_ID=$VERSION_ID release=$release ($version) too old to support
EOM
}

sub pkg_fmt () {
	if ($ID eq 'alpine') { 'apk' }
	elsif ($ID =~ /\A(?:freebsd|dragonfly)\z/) { 'pkg' }
	# *shrug*, as long as the (Net|Open)BSD names don't conflict w/ FreeBSD
	elsif ($ID eq 'netbsd') { 'pkgin' }
	elsif ($ID eq 'openbsd') { 'pkg_add' }
	elsif ($ID =~ m!\A(?:debian|ubuntu)\z!) { 'deb' }
	elsif ($ID =~ m!\A(?:centos|redhat|fedora)\z!) { 'rpm' }
	else { warn "PKG_FMT undefined for ID=$ID"; undef }
}

package main;
PublicInbox::InstallOS->import;

1;
