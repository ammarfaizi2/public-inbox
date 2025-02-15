# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# including backend for a git-cat-file-workalike based on libgit2,
package PublicInbox::Lg2;
use v5.12;
use PublicInbox::Spawn qw(which run_qx); # may set PERL_INLINE_DIRECTORY
use Fcntl qw(SEEK_SET);
use Time::HiRes qw(clock_gettime CLOCK_MONOTONIC);
use IO::Handle; # autoflush
use PublicInbox::Git qw($ck_unlinked_packs);
use PublicInbox::Lock;
use autodie qw(open seek truncate);

BEGIN {
	my (%CFG, $c_src);
	# PublicInbox::Spawn will set PERL_INLINE_DIRECTORY
	# to ~/.cache/public-inbox/inline-c if it exists and Inline::C works
	my $inline_dir = $ENV{PERL_INLINE_DIRECTORY} //
		die 'PERL_INLINE_DIRECTORY not defined';

	# CentOS 7.x ships Inline 0.53, 0.64+ has built-in locking
	my $lk = PublicInbox::Lock->new("$inline_dir/.public-inbox.lock");
	my $fh = $lk->lock_acquire;

	my $pc = which($ENV{PKG_CONFIG} // 'pkg-config') //
		die "pkg-config missing for libgit2";
	my ($dir) = (__FILE__ =~ m!\A(.+?)/[^/]+\z!);
	my $vals = {};
	my $rdr = { 2 => \(my $err) };
	my @switches = qw(modversion cflags libs);
	for my $k (@switches) {
		chomp(my $val = run_qx([$pc, "--$k", 'libgit2'], undef, $rdr));
		die "E: libgit2 not installed: $err\n" if $?;
		$vals->{$k} = $val;
	}
	my $f = "$dir/lg2.h";
	$c_src = PublicInbox::IO::try_cat $f or die "cat $f: $!";
	# append pkg-config results to the source to ensure Inline::C
	# can rebuild if there's changes (it doesn't seem to detect
	# $CFG{CCFLAGSEX} nor $CFG{CPPFLAGS} changes)
	$c_src .= "/* $pc --$_ libgit2 => $vals->{$_} */\n" for @switches;
	open my $oldout, '>&', \*STDOUT;
	open my $olderr, '>&', \*STDERR;
	open STDOUT, '>&', $fh;
	open STDERR, '>&', $fh;
	STDERR->autoflush(1);
	STDOUT->autoflush(1);
	$CFG{CCFLAGSEX} = $vals->{cflags};
	$CFG{LIBS} = $vals->{libs};

	# we use Capitalized and ALLCAPS for compatibility with old Inline::C
	CORE::eval <<'EOM';
use Inline C => Config => %CFG, BOOT => q[git_libgit2_init();];
use Inline C => $c_src, BUILD_NOISY => 1;
EOM
	$err = $@;
	open(STDERR, '>&', $olderr);
	open(STDOUT, '>&', $oldout);
	if ($err) {
		seek($fh, 0, SEEK_SET);
		my @msg = <$fh>;
		truncate($fh, 0);
		die "Inline::C Lg2 build failed:\n", $err, "\n", @msg;
	}
}

sub add_alt ($$) {
	my ($gcf2, $git_dir) = @_;
	my $objdir = PublicInbox::Git->new($git_dir)->git_path('objects');

	# libgit2 (tested 0.27.7+dfsg.1-0.2 and 0.28.3+dfsg.1-1~bpo10+1
	# in Debian) doesn't handle relative epochs properly when nested
	# multiple levels.  Add all the absolute paths to workaround it,
	# since $EXTINDEX_DIR/ALL.git/objects/info/alternates uses absolute
	# paths to reference $V2INBOX_DIR/all.git/objects and
	# $V2INBOX_DIR/all.git/objects/info/alternates uses relative paths
	# to refer to $V2INBOX_DIR/git/$EPOCH.git/objects
	#
	# See https://bugs.debian.org/975607
	if (my $s = PublicInbox::IO::try_cat("$objdir/info/alternates")) {
		$gcf2->add_alternate($_) for ($s =~ m!^(/[^\n]+)\n!gms);
	}
	$gcf2->add_alternate($objdir);
	1;
}

# Usage: $^X -MPublicInbox::Lg2 -e PublicInbox::Lg2::gcf2_loop [EXPIRE-TIMEOUT]
# (see lib/PublicInbox/Gcf2Client.pm)
sub gcf2_loop (;$) {
	my $exp = $_[0] || $ARGV[0] || 60; # seconds
	my $gcf2 = new();
	my (%seen, $check_at);
	STDERR->autoflush(1);
	STDOUT->autoflush(1);
	my $pid = $$;

	while (<STDIN>) {
		chomp;
		my ($oid, $git_dir) = split(/ /, $_, 2);
		$seen{$git_dir} //= add_alt($gcf2, $git_dir);
		if (!$gcf2->cat_oid(1, $oid)) {
			# retry once if missing.  We only get unabbreviated OIDs
			# from SQLite or Xapian DBs, here, so malicious clients
			# can't trigger excessive retries:
			warn "# $$ $oid missing, retrying in $git_dir\n";

			$gcf2 = new();
			%seen = ($git_dir => add_alt($gcf2, $git_dir));
			$check_at = clock_gettime(CLOCK_MONOTONIC) + $exp;

			if ($gcf2->cat_oid(1, $oid)) {
				warn "# $$ $oid found after retry\n";
			} else {
				warn "W: $$ $oid missing after retry\n";
				print "$oid missing\n"; # mimic git-cat-file
			}
		} else { # check expiry to deal with deleted pack files
			my $now = clock_gettime(CLOCK_MONOTONIC);
			$check_at //= $now + $exp;
			if ($now > $check_at) {
				undef $check_at;
				if (!$ck_unlinked_packs ||
						$ck_unlinked_packs->($pid)) {
					$gcf2 = new();
					%seen = ();
				}
			}
		}
	}
}

1;
