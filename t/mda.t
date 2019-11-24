# Copyright (C) 2014-2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use Email::MIME;
use Cwd qw(getcwd);
use PublicInbox::MID qw(mid2path);
use PublicInbox::Git;
require './t/common.perl';
my ($tmpdir, $for_destroy) = tmpdir();
my $home = "$tmpdir/pi-home";
my $pi_home = "$home/.public-inbox";
my $pi_config = "$pi_home/config";
my $maindir = "$tmpdir/main.git";
my $main_bin = getcwd()."/t/main-bin";
my $main_path = "$main_bin:$ENV{PATH}"; # for spamc ham mock
my $fail_bin = getcwd()."/t/fail-bin";
my $fail_path = "$fail_bin:$ENV{PATH}"; # for spamc spam mock
my $addr = 'test-public@example.com';
my $cfgpfx = "publicinbox.test";
my $faildir = "$home/faildir/";
my $mime;
my $git = PublicInbox::Git->new($maindir);

{
	ok(-x "$main_bin/spamc",
		"spamc ham mock found (run in top of source tree");
	ok(-x "$fail_bin/spamc",
		"spamc mock found (run in top of source tree");
	is(1, mkdir($home, 0755), "setup ~/ for testing");
	is(1, mkdir($pi_home, 0755), "setup ~/.public-inbox");
	is(0, system(qw(git init -q --bare), $maindir), "git init (main)");

	my %cfg = (
		"$cfgpfx.address" => $addr,
		"$cfgpfx.inboxdir" => $maindir,
	);
	while (my ($k,$v) = each %cfg) {
		is(0, system(qw(git config --file), $pi_config, $k, $v),
			"setup $k");
	}
}

local $ENV{GIT_COMMITTER_NAME} = eval {
	use PublicInbox::MDA;
	use PublicInbox::Address;
	use Encode qw/encode/;
	my $mbox = 't/utf8.mbox';
	open(my $fh, '<', $mbox) or die "failed to open mbox: $mbox\n";
	my $str = eval { local $/; <$fh> };
	close $fh;
	my $msg = Email::MIME->new($str);

	my $from = $msg->header('From');
	my ($author) = PublicInbox::Address::names($from);
	my ($email) = PublicInbox::Address::emails($from);
	my $date = $msg->header('Date');

	is('El&#233;anor',
		encode('us-ascii', my $tmp = $author, Encode::HTMLCREF),
		'HTML conversion is correct');
	is($email, 'e@example.com', 'email parsed correctly');
	is($date, 'Thu, 01 Jan 1970 00:00:00 +0000',
		'message date parsed correctly');
	$author;
};
die $@ if $@;

{
	my $good_rev;
	local $ENV{PI_EMERGENCY} = $faildir;
	local $ENV{HOME} = $home;
	local $ENV{ORIGINAL_RECIPIENT} = $addr;
	my $simple = Email::Simple->new(<<EOF);
From: Me <me\@example.com>
To: You <you\@example.com>
Cc: $addr
Message-Id: <blah\@example.com>
Subject: hihi
Date: Thu, 01 Jan 1970 00:00:00 +0000

EOF
	my $in = $simple->as_string;

	# ensure successful message delivery
	{
		local $ENV{PATH} = $main_path;
		ok(run_script(['-mda'], undef, { 0 => \$in }));
		my $rev = $git->qx(qw(rev-list HEAD));
		like($rev, qr/\A[a-f0-9]{40}/, "good revision committed");
		chomp $rev;
		my $cmt = $git->cat_file($rev);
		like($$cmt, qr/^author Me <me\@example\.com> 0 \+0000\n/m,
			"author info set correctly");
		like($$cmt, qr/^committer test <test-public\@example\.com>/m,
			"committer info set correctly");
		$good_rev = $rev;
	}

	# ensure failures work, fail with bad spamc
	{
		my @prev = <$faildir/new/*>;
		is(scalar @prev, 0 , "nothing in PI_EMERGENCY before");
		local $ENV{PATH} = $fail_path;
		ok(run_script(['-mda'], undef, { 0 => \$in }));
		my @revs = $git->qx(qw(rev-list HEAD));
		is(scalar @revs, 1, "bad revision not committed");
		my @new = <$faildir/new/*>;
		is(scalar @new, 1, "PI_EMERGENCY is written to");
	}

	fail_bad_header($good_rev, "bad recipient", <<"");
From: Me <me\@example.com>
To: You <you\@example.com>
Message-Id: <bad-recipient\@example.com>
Subject: hihi
Date: Thu, 01 Jan 1970 00:00:00 +0000

	my $fail = fail_bad_header($good_rev, "duplicate Message-ID", <<"");
From: Me <me\@example.com>
To: You <you\@example.com>
Cc: $addr
Message-ID: <blah\@example.com>
Subject: hihi
Date: Thu, 01 Jan 1970 00:00:00 +0000

	like($fail->[2], qr/CONFLICT/, "duplicate Message-ID message");

	fail_bad_header($good_rev, "missing From:", <<"");
To: $addr
Message-ID: <missing-from\@example.com>
Subject: hihi
Date: Thu, 01 Jan 1970 00:00:00 +0000

	fail_bad_header($good_rev, "short subject:", <<"");
To: $addr
From: cat\@example.com
Message-ID: <short-subject\@example.com>
Subject: a
Date: Thu, 01 Jan 1970 00:00:00 +0000

	fail_bad_header($good_rev, "no date", <<"");
To: $addr
From: u\@example.com
Message-ID: <no-date\@example.com>
Subject: hihi

	fail_bad_header($good_rev, "bad date", <<"");
To: $addr
From: u\@example.com
Message-ID: <bad-date\@example.com>
Subject: hihi
Date: deadbeef

}

# spam training
{
	local $ENV{PI_EMERGENCY} = $faildir;
	local $ENV{HOME} = $home;
	local $ENV{ORIGINAL_RECIPIENT} = $addr;
	local $ENV{PATH} = $main_path;
	my $mid = 'spam-train@example.com';
	my $simple = Email::Simple->new(<<EOF);
From: Spammer <spammer\@example.com>
To: You <you\@example.com>
Cc: $addr
Message-ID: <$mid>
Subject: this message will be trained as spam
Date: Thu, 01 Jan 1970 00:00:00 +0000

EOF
	my $in = $simple->as_string;

	{
		# deliver the spam message, first
		ok(run_script(['-mda'], undef, { 0 => \$in }));
		my $path = mid2path($mid);
		my $msg = $git->cat_file("HEAD:$path");
		like($$msg, qr/\Q$mid\E/, "message delivered");

		# now train it
		local $ENV{GIT_AUTHOR_EMAIL} = 'trainer@example.com';
		local $ENV{GIT_COMMITTER_EMAIL} = 'trainer@example.com';
		local $ENV{GIT_COMMITTER_NAME};
		delete $ENV{GIT_COMMITTER_NAME};
		ok(run_script(['-learn', 'spam'], undef, { 0 => $msg }),
			"no failure from learning spam");
		ok(run_script(['-learn', 'spam'], undef, { 0 => $msg }),
			"no failure from learning spam idempotently");
	}
}

# train ham message
{
	local $ENV{PI_EMERGENCY} = $faildir;
	local $ENV{HOME} = $home;
	local $ENV{ORIGINAL_RECIPIENT} = $addr;
	local $ENV{PATH} = $main_path;
	my $mid = 'ham-train@example.com';
	my $simple = Email::Simple->new(<<EOF);
From: False-positive <hammer\@example.com>
To: You <you\@example.com>
Cc: $addr
Message-ID: <$mid>
Subject: this message will be trained as spam
Date: Thu, 01 Jan 1970 00:00:00 +0000

EOF
	my $in = $simple->as_string;

	# now train it
	# these should be overridden
	local $ENV{GIT_AUTHOR_EMAIL} = 'trainer@example.com';
	local $ENV{GIT_COMMITTER_EMAIL} = 'trainer@example.com';

	ok(run_script(['-learn', 'ham'], undef, { 0 => \$in }),
		"learned ham without failure");
	my $path = mid2path($mid);
	my $msg = $git->cat_file("HEAD:$path");
	like($$msg, qr/\Q$mid\E/, "ham message delivered");
	ok(run_script(['-learn', 'ham'], undef, { 0 => \$in }),
		"learned ham idempotently ");

	# ensure trained email is filtered, too
	my $html_body = "<html><body>hi</body></html>";
	my $parts = [
		Email::MIME->create(
			attributes => {
				content_type => 'text/html; charset=UTF-8',
				encoding => 'base64',
			},
			body => $html_body,
		),
		Email::MIME->create(
			attributes => {
				content_type => 'text/plain',
				encoding => 'quoted-printable',
			},
			body => 'hi = "bye"',
		)
	];
	$mid = 'multipart-html-sucks@11';
	$mime = Email::MIME->create(
		header_str => [
		  From => 'a@example.com',
		  Subject => 'blah',
		  Cc => $addr,
		  'Message-ID' => "<$mid>",
		  'Content-Type' => 'multipart/alternative',
		],
		parts => $parts,
	);

	{
		$in = $mime->as_string;
		ok(run_script(['-learn', 'ham'], undef, { 0 => \$in }),
			"learned ham without failure");
		my $path = mid2path($mid);
		$msg = $git->cat_file("HEAD:$path");
		like($$msg, qr/<\Q$mid\E>/, "ham message delivered");
		unlike($$msg, qr/<html>/i, '<html> filtered');
	}
}

# List-ID based delivery
{
	local $ENV{PI_EMERGENCY} = $faildir;
	local $ENV{HOME} = $home;
	local $ENV{ORIGINAL_RECIPIENT} = undef;
	delete $ENV{ORIGINAL_RECIPIENT};
	local $ENV{PATH} = $main_path;
	my $list_id = 'foo.example.com';
	my $mid = 'list-id-delivery@example.com';
	my $simple = Email::Simple->new(<<EOF);
From: user <user\@example.com>
To: You <you\@example.com>
Cc: $addr
Message-ID: <$mid>
List-Id: <$list_id>
Subject: this message will be trained as spam
Date: Thu, 01 Jan 1970 00:00:00 +0000

EOF
	system(qw(git config --file), $pi_config, "$cfgpfx.listid", $list_id);
	$? == 0 or die "failed to set listid $?";
	my $in = $simple->as_string;
	ok(run_script(['-mda'], undef, { 0 => \$in }),
		'mda OK with List-Id match');
	my $path = mid2path($mid);
	my $msg = $git->cat_file("HEAD:$path");
	like($$msg, qr/\Q$list_id\E/, 'delivered message w/ List-ID matches');

	# try a message w/o precheck
	$simple = Email::Simple->new(<<EOF);
To: You <you\@example.com>
List-Id: <$list_id>

this message would not be accepted without --no-precheck
EOF
	$in = $simple->as_string;
	my ($out, $err) = ('', '');
	my $rdr = { 0 => \$in, 1 => \$out, 2 => \$err };
	ok(run_script(['-mda', '--no-precheck'], undef, $rdr),
		'mda OK with List-Id match and --no-precheck');
	my $cur = $git->qx(qw(diff HEAD~1..HEAD));
	like($cur, qr/this message would not be accepted without --no-precheck/,
		'--no-precheck delivered message anyways');

	# try a message with multiple List-ID headers
	$in = <<EOF;
List-ID: <foo.bar>
List-ID: <$list_id>
Message-ID: <2lids\@example>
Subject: two List-IDs
From: user <user\@example.com>
To: $addr
Date: Fri, 02 Oct 1993 00:00:00 +0000

EOF
	($out, $err) = ('', '');
	ok(run_script(['-mda'], undef, $rdr),
		'mda OK with multiple List-Id matches');
	$cur = $git->qx(qw(diff HEAD~1..HEAD));
	like($cur, qr/Message-ID: <2lids\@example>/,
		'multi List-ID match delivered');
	like($err, qr/multiple List-ID/, 'warned about multiple List-ID');
}

done_testing();

sub fail_bad_header {
	my ($good_rev, $msg, $in) = @_;
	my @f = glob("$faildir/*/*");
	unlink @f if @f;
	my ($out, $err) = ("", "");
	my $opt = { 0 => \$in, 1 => \$out, 2 => \$err };
	local $ENV{PATH} = $main_path;
	ok(run_script(['-mda'], undef, $opt),
		"no error on undeliverable ($msg)");
	my $rev = $git->qx(qw(rev-list HEAD));
	chomp $rev;
	is($rev, $good_rev, "bad revision not commited ($msg)");
	@f = glob("$faildir/*/*");
	is(scalar @f, 1, "faildir written to");
	[ $in, $out, $err ];
}
