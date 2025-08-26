#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
require_git 2.6;
require_mods(qw(DBD::SQLite));
require PublicInbox::SearchIdx;
use_ok 'PublicInbox::InboxIdle';
my ($tmpdir, $for_destroy) = tmpdir();

# for non-inotify|kqueue systems w/ low-res FS timestamps
# This only makes the test work, but either high-res FS timestamps
# or inotify or kqueue support needs to be added to your system.
my $poll_delay = 1;

for my $V (1, 2) {
	my $inboxdir = "$tmpdir/$V";
	my $ibx = create_inbox "idle$V", tmpdir => $inboxdir, version => $V,
				indexlevel => 'basic', -no_gc => 1, sub {
		my ($im, $ibx) = @_; # capture
		$im->done;
		$ibx->init_inbox(0);
		$_[0] = undef;
		return if $V != 1;
		my $sidx = PublicInbox::SearchIdx->new($ibx, { wal => 1 });
		$sidx->idx_acquire;
		$sidx->set_metadata_once;
		$sidx->idx_release; # allow watching on lockfile
	};
	my $obj = InboxIdleTestObj->new;
	my $pi_cfg = cfg_new $tmpdir, <<EOF;
[publicinbox "inbox-idle"]
	inboxdir = $inboxdir
	indexlevel = basic
	address = $ibx->{-primary_address}
EOF
	my $ident = 'whatever';
	$pi_cfg->each_inbox(sub { shift->subscribe_unlock($ident, $obj) });
	my $ii = PublicInbox::InboxIdle->new($pi_cfg);
	ok($ii, 'InboxIdle created');
	SKIP: {
		$ii->{sock} or skip
'inotify or kqueue missing, expect real-world breakage on low-res FSes', 1;
		ok(fileno($ii->{sock}) >= 0, 'fileno() gave valid FD');
		$poll_delay = 0;
	}
	my $im = $ibx->importer(0);
	ok($im->add(eml_load('t/utf8.eml')), "$V added");
	tick $poll_delay if $poll_delay;
	$im->done;
	$ii->event_step;
	is(scalar @{$obj->{called}}, 1, 'called on unlock') or
		diag explain($obj);
	$pi_cfg->each_inbox(sub { shift->unsubscribe_unlock($ident) });
	ok($im->add(eml_load('t/data/0001.patch')), "$V added #2");
	tick $poll_delay if $poll_delay;
	$im->done;
	$ii->event_step;
	is(scalar @{$obj->{called}}, 1, 'not called when unsubbed');
	$ii->close;
}

done_testing;

package InboxIdleTestObj;
use strict;

sub new { bless {}, shift }

sub on_inbox_unlock {
	my ($self, $ibx) = @_;
	push @{$self->{called}}, $ibx;
}
