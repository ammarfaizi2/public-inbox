# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use autodie;
use PublicInbox::TestCommon;
use PublicInbox::Eml;
use PublicInbox::Emergency;
use PublicInbox::IO qw(write_file);
use PublicInbox::InboxIdle;
use PublicInbox::Inbox;
use PublicInbox::DS;
use PublicInbox::Config;
require_mods(qw(DBD::SQLite Xapian));
my $tmpdir = tmpdir;
my $config = "$tmpdir/pi_config";
local $ENV{PI_CONFIG} = $config;
delete local $ENV{PI_DIR};
my @V = (1);
my @creat_opt = (indexlevel => 'medium', sub {});
my $v1 = create_inbox 'v1', tmpdir => "$tmpdir/v1", @creat_opt;
my $fh = write_file '>', $config, <<EOM;
[publicinbox "v1"]
	inboxdir = $v1->{inboxdir}
	address = v1\@example.com
	watch = maildir:$tmpdir/v1-md
	indexheader = boolean_term:xarchiveshash:X-Archives-Hash
EOM

SKIP: {
	require_git(v2.6, 1);
	push @V, 2;
	my $v2 = create_inbox 'v2', tmpdir => "$tmpdir/v2", @creat_opt;
	print $fh <<EOM;
[publicinbox "v2"]
	inboxdir = $tmpdir/v2
	address = v2\@example.com
	watch = maildir:$tmpdir/v2-md
	indexheader = boolean_term:xarchiveshash:X-Archives-Hash
EOM
}
close $fh;
my $cfg = PublicInbox::Config->new;
for my $v (@V) { for ('', qw(cur new tmp)) { mkdir "$tmpdir/v$v-md/$_" } }
my $wm = start_script([qw(-watch)]);
my $h1 = 'deadbeef' x 4;
my @em = map {
	my $v = $_;
	my $em = PublicInbox::Emergency->new("$tmpdir/v$v-md");
	$em->prepare(\(PublicInbox::Eml->new(<<EOM)->as_string));
From: x\@example.com
Message-ID: <i-1$v\@example.com>
To: <v$v\@example.com>
Date: Sat, 02 Oct 2010 00:00:00 +0000
X-Archives-Hash: $h1

EOM
	$em;
} @V;

my $delivered = 0;
my $cb = sub {
	diag "message delivered to `$_[0]->{name}'";
	++$delivered;
};
PublicInbox::DS->Reset;
my $ii = PublicInbox::InboxIdle->new($cfg);
my $obj = bless \$cb, 'PublicInbox::TestCommon::InboxWakeup';
$cfg->each_inbox(sub { $_[0]->subscribe_unlock('ident', $obj) });
local @PublicInbox::DS::post_loop_do = (sub { $delivered != @V });
$_->commit for @em;
diag 'waiting for -watch to import new message(s)';
PublicInbox::DS::event_loop();
$wm->join('TERM');
$ii->close;

$cfg->each_inbox(sub {
	my ($ibx) = @_;
	my $srch = $ibx->search;
	my $mset = $srch->mset('xarchiveshash:miss');
	is($mset->size, 0, 'got xarchiveshash:miss non-result');
	$mset = $srch->mset("xarchiveshash:$h1");
	is($mset->size, 1, 'got xarchiveshash: hit result') or return;
	my $num = $srch->mset_to_artnums($mset);
	my $eml = $ibx->smsg_eml($ibx->over->get_art($num->[0]));
	is($eml->header_raw('X-Archives-Hash'), $h1,
		'stored message with X-Archives-Hash');
	my @opt = $srch->xh_args;
	is $opt[-2], '-Q', 'xap_helper -Q switch';
	is $opt[-1], 'xarchiveshash=XXARCHIVESHASH', 'xap_helper -Q arg';
});

done_testing;
