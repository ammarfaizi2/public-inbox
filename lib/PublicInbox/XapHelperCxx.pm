# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Just-ahead-of-time builder for the lib/PublicInbox/xap_helper.h shim.
# I never want users to be without source code for repairs, so this
# aims to replicate the feel of a scripting language using C++.
# The resulting executable is not linked to Perl in any way.
package PublicInbox::XapHelperCxx;
use v5.12;
use PublicInbox::Spawn qw(popen_rd which);
use PublicInbox::Search;
use Fcntl qw(SEEK_SET);
use Config;
my $cxx = which($ENV{CXX} // 'c++');
my $dir = substr("$cxx-$Config{archname}", 1); # drop leading '/'
$dir =~ tr!/!-!;
$ENV{PERL_INLINE_DIRECTORY} // die('BUG: PERL_INLINE_DIRECTORY unset');
substr($dir, 0, 0) = "$ENV{PERL_INLINE_DIRECTORY}/";
my $bin = "$dir/xap_helper";
my ($srcpfx) = (__FILE__ =~ m!\A(.+/)[^/]+\z!);
my @srcs = map { $srcpfx.$_ } qw(xap_helper.h);
my @pm_dep = map { $srcpfx.$_ } qw(Search.pm CodeSearch.pm);
my $ldflags = '-Wl,-O1';
$ldflags .= ' -Wl,--compress-debug-sections=zlib' if $^O ne 'openbsd';
my $xflags = ($ENV{CXXFLAGS} // '-Wall -ggdb3 -O0') . ' ' .
	($ENV{LDFLAGS} // $ldflags) .
	qq{ -DTHREADID=}.PublicInbox::Search::THREADID;
my $xap_modversion;

sub xap_cfg (@) {
	open my $err, '+>', undef or die "open(undef): $!";
	my $cmd = [ $ENV{PKG_CONFIG} // 'pkg-config', @_, 'xapian-core' ];
	my $rd = popen_rd($cmd, undef, { 2 => $err });
	chomp(my $ret = do { local $/; <$rd> });
	return $ret if close($rd);
	seek($err, 0, SEEK_SET) or die "seek: $!";
	$err = do { local $/; <$err> };
	die <<EOM;
@$cmd failed: Xapian development files missing? (\$?=$?)
$err
EOM
}

sub needs_rebuild () {
	open my $fh, '<', "$dir/XFLAGS" or return 1;
	chomp(my $prev = <$fh>);
	return 1 if $prev ne $xflags;

	open $fh, '<', "$dir/xap_modversion" or return 1;
	chomp($prev = <$fh>);
	$prev or return 1;

	$xap_modversion = xap_cfg('--modversion');
	$xap_modversion ne $prev;
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

	# xap_modversion may be set by needs_rebuild
	$xap_modversion //= xap_cfg('--modversion');
	my $fl = xap_cfg(qw(--libs --cflags));

	# Using rpath seems acceptable/encouraged in the NetBSD packaging world
	# since /usr/pkg/lib isn't searched by the dynamic loader by default.
	# Not sure if other OSes need this, but rpath seems fine for JAOT
	# binaries (like this one) even if other distros discourage it for
	# distributed packages.
	$^O eq 'netbsd' and $fl =~ s/(\A|[ \t])\-L([^ \t]+)([ \t]|\z)/
				"$1-L$2 -Wl,-rpath=$2$3"/egsx;

	my $cmd = "$cxx $src $fl $xflags -o $tmp/$prog";
	system($cmd) and die "$cmd failed: \$?=$?";
	open $fh, '>', "$tmp/XFLAGS";
	say $fh $xflags;
	close $fh;

	open $fh, '>', "$tmp/xap_modversion";
	say $fh $xap_modversion;
	close $fh;
	undef $xap_modversion; # do we ever build() twice?
	# not quite atomic, but close enough :P
	rename("$tmp/$_", "$dir/$_") for ($prog, qw(XFLAGS xap_modversion));
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
	needs_rebuild() ? build() : 0;
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
