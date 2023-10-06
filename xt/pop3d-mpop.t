#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# ensure mpop compatibility
use v5.12;
use File::Path qw(make_path);
use PublicInbox::TestCommon;
use PublicInbox::Spawn qw(spawn);
my $inboxdir = $ENV{GIANT_INBOX_DIR};
(defined($inboxdir) && -d $inboxdir) or
	plan skip_all => "GIANT_INBOX_DIR not defined for $0";
plan skip_all => "bad characters in $inboxdir" if $inboxdir =~ m![^\w\.\-/]!;
my $uuidgen = require_cmd('uuidgen');
my $mpop = require_cmd('mpop');
require_mods(qw(DBD::SQLite :fcntl_lock));
require_git(v2.6); # for v2

my ($tmpdir, $for_destroy) = tmpdir();
my $cfg = "$tmpdir/cfg";
my $newsgroup = 'inbox.test';
my %pids;
{
	open my $fh, '>', $cfg or xbail "open: $!";
	print $fh <<EOF or xbail "print: $!";
[publicinbox]
	pop3state = $tmpdir/p3s
[publicinbox "test"]
	newsgroup = $newsgroup
	address = mpop-test\@example.com
	inboxdir = $inboxdir
EOF
	close $fh or xbail "close: $!";
}
my ($out, $err) = ("$tmpdir/stdout.log", "$tmpdir/stderr.log");
my $sock = tcp_server();
my $cmd = [ '-pop3d', '-W0', "--stdout=$out", "--stderr=$err" ];
my $env = { PI_CONFIG => $cfg };
my $td = start_script($cmd, $env, { 3 => $sock }) or xbail "-xbail $?";
chomp(my $uuid = xqx([$uuidgen]));

make_path("$tmpdir/home/.config/mpop",
	map { "$tmpdir/md/$_" } qw(new cur tmp));

{
	open my $fh, '>', "$tmpdir/home/.config/mpop/config"
		or xbail "open $!";
	chmod 0600, $fh;
	print $fh <<EOM or xbail "print $!";
defaults
tls off
delivery maildir $tmpdir/md
account default
host ${\$sock->sockhost}
port ${\$sock->sockport}
user $uuid\@$newsgroup?limit=10000
auth user
password anonymous
received_header off
EOM
	close $fh or xbail "close $!";
	delete local $ENV{XDG_CONFIG_HOME}; # mpop uses this
	local $ENV{HOME} = "$tmpdir/home";
	my $cmd = [ $mpop, '-q' ];
	my $pid = spawn($cmd, undef, { 1 => 2 });
	$pids{$pid} = $cmd;
}
diag "mpop is writing to $tmpdir/md ...";
while (scalar keys %pids) {
	my $pid = waitpid(-1, 0) or next;
	my $cmd = delete $pids{$pid} or next;
	is($?, 0, join(' ', @$cmd, 'done'));
}
$td->kill;
$td->join;
is($?, 0, 'no error on -pop3d exit');
done_testing;
