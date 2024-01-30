#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::Eml;
use PublicInbox::TestCommon;
use PublicInbox::Import;
use PublicInbox::IO qw(write_file);
use POSIX qw(mkfifo);
use File::Copy qw(cp);
use autodie qw(rename mkdir);

my $tmpdir = tmpdir;
my $git_dir = "$tmpdir/test.git";
my $mh = "$tmpdir/mh";
my $spamdir = "$tmpdir/mh-spam";
mkdir $_ for ($mh, $spamdir);
use_ok 'PublicInbox::Watch';
my $addr = 'test-public@example.com';
my $default_branch = PublicInbox::Import::default_branch;
PublicInbox::Import::init_bare($git_dir);
my $msg = <<EOF;
From: user\@example.com
To: $addr
Subject: spam
Message-ID: <a\@b.com>
Date: Sat, 18 Jun 2016 00:00:00 +0000

something
EOF

cp 't/plack-qp.eml', "$mh/1";
mkfifo("$mh/5", 0777) or xbail "mkfifo: $!"; # FIFO to ensure no stuckage
my $cfg = cfg_new $tmpdir, <<EOF;
[publicinbox "test"]
	address = $addr
	inboxdir = $git_dir
	watch = mh:$mh
[publicinboxlearn]
	watchspam = mh:$spamdir
EOF
PublicInbox::Watch->new($cfg)->scan('full');
my $git = PublicInbox::Git->new($git_dir);
{
	my @list = $git->qx('rev-list', $default_branch);
	is(scalar @list, 1, 'one revision in rev-list');
	$git->cleanup;
}

# end-to-end test which actually uses inotify/kevent
{
	my $env = { PI_CONFIG => $cfg->{-f} };
	# n.b. --no-scan is only intended for testing atm
	my $wm = start_script([qw(-watch --no-scan)], $env);
	no_pollerfd($wm->{pid});

	my $eml = eml_load 't/data/binary.patch';
	$eml->header_set('Cc', $addr);
	write_file '>', "$mh/2.tmp", $eml->as_string;

	use_ok 'PublicInbox::InboxIdle';
	use_ok 'PublicInbox::DS';
	my $delivered = 0;
	my $cb = sub {
		my ($ibx) = @_;
		diag "message delivered to `$ibx->{name}'";
		$delivered++;
	};
	PublicInbox::DS->Reset;
	my $ii = PublicInbox::InboxIdle->new($cfg);
	my $obj = bless \$cb, 'PublicInbox::TestCommon::InboxWakeup';
	$cfg->each_inbox(sub { $_[0]->subscribe_unlock('ident', $obj) });
	local @PublicInbox::DS::post_loop_do = (sub { $delivered == 0 });

	# wait for -watch to setup inotify watches
	my $sleep = 1;
	if (eval { require PublicInbox::Inotify } && -d "/proc/$wm->{pid}/fd") {
		my $end = time + 2;
		my (@ino, @ino_info);
		do {
			@ino = grep {
				(readlink($_)//'') =~ /\binotify\b/
			} glob("/proc/$wm->{pid}/fd/*");
		} until (@ino || time > $end || !tick);
		if (scalar(@ino) == 1) {
			my $ino_fd = (split(m'/', $ino[0]))[-1];
			my $ino_fdinfo = "/proc/$wm->{pid}/fdinfo/$ino_fd";
			while (time < $end && open(my $fh, '<', $ino_fdinfo)) {
				@ino_info = grep(/^inotify wd:/, <$fh>);
				last if @ino_info >= 2;
				tick;
			}
			$sleep = undef if @ino_info >= 2;
		}
	}
	if ($sleep) {
		diag "waiting ${sleep}s for -watch to start up";
		sleep $sleep;
	}
	rename "$mh/2.tmp", "$mh/2";
	diag 'waiting for -watch to import new message';
	PublicInbox::DS::event_loop();

	my $subj = $eml->header_raw('Subject');
	my $head = $git->qx(qw(cat-file commit HEAD));
	like $head, qr/^\Q$subj\E/sm, 'new commit made';

	$wm->kill;
	$wm->join;
	$ii->close;
	PublicInbox::DS->Reset;
}

my $is_mh = sub { PublicInbox::Watch::is_mh(my $val = shift) };

is $is_mh->('mh:/hello//world'), '/hello/world', 'extra slash gone';
is $is_mh->('MH:/hello/world/'), '/hello/world', 'trailing slash gone';
is $is_mh->('maildir:/hello/world/'), undef, 'non-MH rejected';

done_testing;
