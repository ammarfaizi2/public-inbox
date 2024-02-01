# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
require_mods(qw(DBD::SQLite Net::POP3 :fcntl_lock));
use autodie;
my $tmpdir = tmpdir;
require_ok 'PublicInbox::POP3D';
my $pop3d = bless {}, 'PublicInbox::POP3D';
open $pop3d->{txn_fh}, '+>>', "$tmpdir/txn.lock";
use Fcntl qw(F_SETLK F_UNLCK F_WRLCK);

ok $pop3d->_setlk(l_type => F_WRLCK, l_start => 9, l_len => 1),
	'locked file (check with ktrace/strace)';

done_testing;
