#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
my $tmpdir = tmpdir;
use_ok 'PublicInbox::IO';
use PublicInbox::Spawn qw(which run_qx);

# test failures:
SKIP: {
my $strace = strace_inject;
my $env = { PERL5LIB => join(':', @INC) };
my $opt = { 1 => \my $out, 2 => \my $err };
my $dst = "$tmpdir/dst";
my $tr = "$tmpdir/tr";
my $cmd = [ $strace, "-o$tr", "-P$dst",
		'-e', 'inject=writev,write:error=EIO',
		$^X, qw(-w -MPublicInbox::IO=write_file -e),
		q[write_file '>', $ARGV[0], 'hello world'], $dst ];
xsys($cmd, $env, $opt);
isnt($?, 0, 'write failed');
like($err, qr/\bclose\b/, 'close error noted');
is(-s $dst, 0, 'file created and empty after EIO');
} # /SKIP

PublicInbox::IO::write_file '>:unix', "$tmpdir/f", "HI\n";
is(-s "$tmpdir/f", 3, 'write_file works w/ IO layer');
PublicInbox::IO::write_file '>>', "$tmpdir/f", "HI\n";
is(-s "$tmpdir/f", 6, 'write_file can append');

is PublicInbox::IO::try_cat("$tmpdir/non-existent"), '',
	"try_cat on non-existent file returns `'";

done_testing;
