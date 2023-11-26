# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Just-ahead-of-time builder for the lib/PublicInbox/xap_helper.h shim.
# I never want users to be without source code for repairs, so this
# aims to replicate the feel of a scripting language using C++.
# The resulting executable is not linked to Perl in any way.
package PublicInbox::XapHelperCxx;
use v5.12;
use PublicInbox::Spawn qw(run_die run_qx which);
use PublicInbox::IO qw(try_cat write_file);
use PublicInbox::Search;
use Fcntl qw(SEEK_SET);
use Config;
use autodie;
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
my $xflags = ($ENV{CXXFLAGS} // '-Wall -ggdb3 -pipe') . ' ' .
	' -DTHREADID=' . PublicInbox::Search::THREADID .
	' ' . ($ENV{LDFLAGS} // $ldflags);
my $xap_modversion;

sub xap_cfg (@) {
	my $cmd = [ $ENV{PKG_CONFIG} // 'pkg-config', @_, 'xapian-core' ];
	chomp(my $ret = run_qx($cmd, undef, { 2 => \(my $err) }));
	return $ret if !$?;
	die <<EOM;
@$cmd failed: Xapian development files missing? (\$?=$?)
$err
EOM
}

sub needs_rebuild () {
	my $prev = try_cat("$dir/XFLAGS") or return 1;
	chomp $prev;
	return 1 if $prev ne $xflags;

	$prev = try_cat("$dir/xap_modversion") or return 1;
	chomp $prev;

	$xap_modversion = xap_cfg('--modversion');
	$xap_modversion ne $prev;
}

sub build () {
	if (!-d $dir && !CORE::mkdir($dir)) {
		my $err = $!;
		die "mkdir($dir): $err" if !-d $dir;
	}
	require PublicInbox::CodeSearch;
	require PublicInbox::Lock;
	require PublicInbox::OnDestroy;
	my ($prog) = ($bin =~ m!/([^/]+)\z!);
	my $lk = PublicInbox::Lock->new("$dir/$prog.lock")->lock_for_scope;
	open my $fh, '>', "$dir/$prog.cpp";
	say $fh qq(# include "$_") for @srcs;
	print $fh PublicInbox::Search::generate_cxx();
	print $fh PublicInbox::CodeSearch::generate_cxx();
	close $fh;

	opendir my $dh, '.';
	my $restore = PublicInbox::OnDestroy->new(\&chdir, $dh);
	chdir $dir;

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
	my @xflags = split(' ', "$fl $xflags"); # ' ' awk-mode eats leading WS
	my @cflags = grep(!/\A-(?:Wl|l|L)/, @xflags);
	run_die([$cxx, '-c', "$prog.cpp", '-I', $srcpfx, @cflags]);
	run_die([$cxx, '-o', "$prog.tmp", "$prog.o", @xflags]);
	unlink "$prog.cpp", "$prog.o";
	write_file '>', 'XFLAGS.tmp', $xflags, "\n";
	write_file '>', 'xap_modversion.tmp', $xap_modversion, "\n";
	undef $xap_modversion; # do we ever build() twice?
	# not quite atomic, but close enough :P
	rename("$_.tmp", $_) for ($prog, qw(XFLAGS xap_modversion));
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

# returns spawn arg
sub cmd {
	die 'PI_NO_CXX set' if $ENV{PI_NO_CXX};
	check_build();
	my @cmd;
	if (my $v = $ENV{VALGRIND}) {
		$v = 'valgrind -v' if $v eq '1';
		@cmd = split(/\s+/, $v);
	}
	push @cmd, $bin;
	\@cmd;
}

1;
