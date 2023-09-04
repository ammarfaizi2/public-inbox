# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Just-ahead-of-time builder for the lib/PublicInbox/xap_helper.h shim.
# I never want users to be without source code for repairs, so this
# aims to replicate the feel of a scripting language using C++.
# The resulting executable is not linked to Perl in any way.
package PublicInbox::XapHelperCxx;
use v5.12;
use PublicInbox::Spawn;
use PublicInbox::Search;
my $dir = ($ENV{PERL_INLINE_DIRECTORY} //
	die('BUG: PERL_INLINE_DIRECTORY unset')) . '/cxx';
my $bin = "$dir/xap_helper";
my ($srcpfx) = (__FILE__ =~ m!\A(.+/)[^/]+\z!);
my @srcs = map { $srcpfx.$_ } qw(xap_helper.h);
my @pm_dep = map { $srcpfx.$_ } qw(Search.pm CodeSearch.pm);
my $ldflags = '-Wl,-O1';
$ldflags .= ' -Wl,--compress-debug-sections=zlib' if $^O ne 'openbsd';
my $xflags = ($ENV{CXXFLAGS} // '-Wall -ggdb3 -O0') . ' ' .
	($ENV{LDFLAGS} // $ldflags) .
	qq{ -DTHREADID=}.PublicInbox::Search::THREADID;

sub xflags_chg () {
	open my $fh, '<', "$dir/XFLAGS" or return 1;
	chomp(my $prev = <$fh>);
	$prev ne $xflags;
}

sub build () {
	if (!-d $dir) {
		my $err;
		mkdir($dir) or $err = $!;
		die "mkdir($dir): $err" if !-d $dir;
	}
	use autodie;
	require File::Temp;
	require PublicInbox::CodeSearch;
	my ($prog) = ($bin =~ m!/([^/]+)\z!);
	my $pkg_config = $ENV{PKG_CONFIG} // 'pkg-config';
	my $tmp = File::Temp->newdir(DIR => $dir) // die "newdir: $!";
	my $src = "$tmp/$prog.cpp";
	open my $fh, '>', $src;
	for (@srcs) {
		say $fh qq(# line 1 "$_");
		open my $rfh, '<', $_;
		local $/;
		print $fh readline($rfh);
	}
	print $fh PublicInbox::Search::generate_cxx();
	print $fh PublicInbox::CodeSearch::generate_cxx();
	close $fh;

	my $cmd = "$pkg_config --libs --cflags xapian-core";
	chomp(my $fl = `$cmd`);
	die "$cmd failed: \$?=$?" if $?;

	# Using rpath seems acceptable/encouraged in the NetBSD packaging world
	# since /usr/pkg/lib isn't searched by the dynamic loader by default.
	# Not sure if other OSes need this, but rpath seems fine for JAOT
	# binaries (like this one) even if other distros discourage it for
	# distributed packages.
	$^O eq 'netbsd' and $fl =~ s/(\A|[ \t])\-L([^ \t]+)([ \t]|\z)/
				"$1-L$2 -Wl,-rpath=$2$3"/egsx;

	my $cxx = $ENV{CXX} // 'c++';
	$cmd = "$cxx $src $fl $xflags -o $tmp/$prog";
	system($cmd) and die "$cmd failed: \$?=$?";
	my $cf = "$tmp/XFLAGS";
	open $fh, '>', $cf;
	say $fh $xflags;
	close $fh;
	# not quite atomic, but close enough :P
	rename("$tmp/$_", "$dir/$_") for ($prog, 'XFLAGS');
}

sub check_build () {
	use Time::HiRes qw(stat);
	my $ctime = 0;
	my @bin = stat($bin) or return build();
	for (@srcs, @pm_dep) {
		my @st = stat($_) or die "stat $_: $!";
		if ($st[10] > $ctime) {
			$ctime = $st[10];
			return build() if $ctime > $bin[10];
		}
	}
	xflags_chg() ? build() : 0;
}

sub start (@) {
	check_build();
	my @cmd;
	if (my $v = $ENV{VALGRIND}) {
		$v = 'valgrind -v' if $v eq '1';
		@cmd = split(/\s+/, $v);
	}
	push @cmd, $bin, @_;
	my $prog = $cmd[0];
	$cmd[0] =~ s!\A.*?/([^/]+)\z!$1!;
	exec { $prog } @cmd;
}

1;
