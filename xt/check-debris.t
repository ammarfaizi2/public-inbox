#!perl -w
use v5.12;
use autodie qw(open);
use PublicInbox::TestCommon;
use File::Spec;
my $tmpdir = File::Spec->tmpdir;

diag "note: writes to `$tmpdir' by others results in false-positives";

my %cur = map { $_ => 1 } glob("$tmpdir/*");
for my $t (@ARGV ? @ARGV : glob('t/*.t')) {
	open my $fh, '-|', $^X, '-w', $t;
	my @out;
	while (<$fh>) {
		chomp;
		push @out, $_;
		next if /^ok / || /\A[0-9]+\.\.[0-9]+\z/;
		diag $_;
	}
	ok(close($fh), $t) or diag(explain(\@out));

	no_coredump($tmpdir);

	my @remain = grep { !$cur{$_}++ } glob("$tmpdir/*");
	next if !@remain;
	is_deeply(\@remain, [], "$t has no leftovers") or
		diag "$t added: ",explain(\@remain);
}

done_testing;
