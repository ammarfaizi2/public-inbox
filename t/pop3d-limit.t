#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
require_mods(qw(DBD::SQLite Net::POP3));
$^O =~ /\A(?:linux|(?:free|net|open)bsd)\z/ or
	require_mods(qw(File::FcntlLock));
use autodie;
my ($tmpdir, $for_destroy) = tmpdir();
mkdir("$tmpdir/p3state");
use PublicInbox::Eml;
my $group = 'test.pop3d.limit';
my $addr = 'pop3d-limit@example.com';

my $add_msg = sub {
	my ($im, $n) = @_;
	$im->add(PublicInbox::Eml->new(<<EOM)) or die 'add dup';
From: $n\@example.com
Subject: msg $n
To: $addr
Message-ID: <mid-$n\@example.com>
Date: Sat, 02 Oct 2010 00:00:00 +0000

body $n
EOM
};

my $ibx = create_inbox 'pop3d-limit', version => 2, -primary_address => $addr,
			indexlevel => 'basic', tmpdir => "$tmpdir/ibx", sub {
	my ($im, $ibx) = @_;
	$add_msg->($im, $_) for (1..3);
	$im->done;
	diag 'done';
}; # /create_inbox

my $pi_config = "$tmpdir/pi_config";
{
	open my $fh, '>', $pi_config;
	print $fh <<EOF;
[publicinbox]
	pop3state = $tmpdir/p3state
[publicinbox "pop3"]
	inboxdir = $ibx->{inboxdir}
	address = $addr
	indexlevel = basic
	newsgroup = $group
EOF
	close $fh;
}

my $plain = tcp_server();
my $plain_addr = tcp_host_port($plain);
my $env = { PI_CONFIG => $pi_config };
my $p3d = start_script([qw(-pop3d -W0),
	"--stdout=$tmpdir/out.log", "--stderr=$tmpdir/err.log" ],
	$env, { 3 => $plain });
my @np3args = ($plain->sockhost, Port => $plain->sockport);
my $fetch_delete = sub {
	my ($np3) = @_;
	map {
		my $msg = $np3->get($_);
		$np3->delete($_);
		PublicInbox::Eml->new(join('', @$msg));
	} sort { $a <=> $b } keys %{$np3->list};
};

my $login_a = ('a'x32)."\@$group?initial_limit=2&limit=1";
my $login_a0 = ('a'x32)."\@$group.0?initial_limit=2&limit=1";
my $login_b = ('b'x32)."\@$group?limit=1";
my $login_b0 = ('b'x32)."\@$group.0?limit=1";
my $login_c = ('c'x32)."\@$group?limit=10";
my $login_c0 = ('c'x32)."\@$group.0?limit=10";
my $login_d = ('d'x32)."\@$group?limit=100000";
my $login_d0 = ('d'x32)."\@$group.0?limit=100000";

for my $login ($login_a, $login_a0) {
	my $np3 = Net::POP3->new(@np3args) or xbail "Net::POP3 $!";
	$np3->login($login, 'anonymous') or xbail "login $login ($!)";
	my @msg = $fetch_delete->($np3);
	$np3->quit;
	is_deeply([ map { $_->header('Message-ID') } @msg ],
		[ qw(<mid-2@example.com> <mid-3@example.com>) ],
		"initial_limit ($login)") or diag explain(\@msg);
}

for my $login ($login_b, $login_b0) {
	my $np3 = Net::POP3->new(@np3args);
	$np3->login($login, 'anonymous') or xbail "login $login ($!)";
	my @msg = $fetch_delete->($np3);
	$np3->quit;
	is_deeply([ map { $_->header('Message-ID') } @msg ],
		[ qw(<mid-3@example.com>) ],
		"limit-only ($login)") or diag explain(\@msg);
}

for my $login ($login_c, $login_c0, $login_d, $login_d0) {
	my $np3 = Net::POP3->new(@np3args);
	$np3->login($login, 'anonymous') or xbail "login $login ($!)";
	my @msg = $fetch_delete->($np3);
	$np3->quit;
	is_deeply([ map { $_->header('Message-ID') } @msg ],
		[ qw(<mid-1@example.com> <mid-2@example.com>
			<mid-3@example.com>) ],
		"excessive limit ($login)") or diag explain(\@msg);
}

{ # add some new messages
	my $im = $ibx->importer(0);
	$add_msg->($im, $_) for (4..5);
	$im->done;
}

for my $login ($login_a, $login_a0) {
	my $np3 = Net::POP3->new(@np3args);
	$np3->login($login, 'anonymous') or xbail "login $login ($!)";
	my @msg = $fetch_delete->($np3);
	$np3->quit;
	is_deeply([ map { $_->header('Message-ID') } @msg ],
		[ qw(<mid-5@example.com>) ],
		"limit used (initial_limit ignored, $login)") or
			diag explain(\@msg);
}

for my $login ($login_b, $login_b0) {
	my $np3 = Net::POP3->new(@np3args);
	$np3->login($login, 'anonymous') or xbail "login $login ($!)";
	my @msg = $fetch_delete->($np3);
	$np3->quit;
	is_deeply([ map { $_->header('Message-ID') } @msg ],
		[ qw(<mid-5@example.com>) ],
		"limit-only after new messages ($login)") or
		diag explain(\@msg);
}

for my $login ($login_c, $login_c0, $login_d, $login_d0) {
	my $np3 = Net::POP3->new(@np3args);
	$np3->login($login, 'anonymous') or xbail "login $login ($!)";
	my @msg = $fetch_delete->($np3);
	$np3->quit;
	is_deeply([ map { $_->header('Message-ID') } @msg ],
		[ qw(<mid-4@example.com> <mid-5@example.com>) ],
		"excessive limit ($login)") or diag explain(\@msg);
}

done_testing;
