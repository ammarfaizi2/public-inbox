#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use Benchmark qw(:all :hireswallclock);
use PublicInbox::Inbox;
use PublicInbox::ViewDiff;
use PublicInbox::MsgIter qw(msg_part_text);
my $nr = $ENV{NR} // 5;
my $inboxdir = $ENV{GIANT_INBOX_DIR};
plan skip_all => "GIANT_INBOX_DIR not defined for $0" unless $inboxdir;

my @cat = qw(cat-file --buffer --batch-check --batch-all-objects);
if (require_git(v2.19, 1)) {
	push @cat, '--unordered';
} else {
	warn
"git <2.19, cat-file lacks --unordered, locality suffers\n";
}
my $ibx = PublicInbox::Inbox->new({ inboxdir => $inboxdir, name => 'name' });
my $git = $ibx->git;
my ($eml, $res, $oid, $type, $n, $m);
my ($part, $s, $err, @top);
sub text_part {
	$part = $_[0]->[0];
	($s, $err) = msg_part_text($part, $part->content_type || 'text/plain');
	$s // return;
	$s =~ s/\r+\n/\n/sg;
}

my %extract_cb = (
	var => sub { # callback for Eml->each_part
		text_part(@_) // return;
		my @top = split($PublicInbox::ViewDiff::EXTRACT_DIFFS, $s);
	},
	slash => sub { # callback for Eml->each_part
		text_part(@_) // return;
		my @top = split(/$PublicInbox::ViewDiff::EXTRACT_DIFFS/, $s);
	},
	slash_o => sub { # callback for Eml->each_part
		text_part(@_) // return;
		my @top = split(/$PublicInbox::ViewDiff::EXTRACT_DIFFS/o, $s);
	},
);

my $oid_cb = sub {
	my ($bref, undef, undef, undef, $cb) = @_;
	++$m;
	$eml = PublicInbox::Eml->new($bref);
	$eml->each_part($cb);
};

# ensure all --batch-check processes are ready
my @cats = map {
	my $fh = $git->popen(@cat);
	vec(my $vec = '', fileno($fh), 1) = 1;
	select($vec, undef, undef, 60) or
		xbail 'timed out waiting for --batch-check';
	$fh
} (1..((scalar keys %extract_cb) * $nr));

my $time;
while (my ($name, $eml_cb) = each %extract_cb) {
	$time->{$name} = sub {
		my $fh = shift @cats // xbail "no --batch-check for $name";
		$n = $m = 0;
		while (<$fh>) {
			($oid, $type) = split / /;
			next if $type ne 'blob';
			++$n;
			$git->cat_async($oid, $oid_cb, $eml_cb);
		}
		$git->async_wait_all;
		is $n, $m, "$n of $m messages scanned ($name)";
	};
}

timethese($nr, $time);
done_testing;
