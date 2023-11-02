#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
my $tmpdir = tmpdir;
use_ok 'PublicInbox::IO';
use PublicInbox::Spawn qw(which run_qx);

# only test failures
SKIP: {
skip 'linux only test' if $^O ne 'linux';
my $strace = which('strace') or skip 'strace missing for test';
my $v = run_qx([$strace, '--version']);
$v =~ m!version\s+([1-9]+\.[0-9]+)! or xbail "no strace --version: $v";
$v = eval("v$1");
$v ge v4.16 or skip "$strace too old for syscall injection (".
		sprintf('v%vd', $v). ' < v4.16)';
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

done_testing;
