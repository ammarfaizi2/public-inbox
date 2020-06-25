# Copyright (C) 2018-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# for XOVER, OVER in NNTP, and feeds/homepage/threads in PSGI
# Unlike Msgmap, this is an _UNSTABLE_ database which can be
# tweaked/updated over time and rebuilt.
package PublicInbox::Over;
use strict;
use warnings;
use DBI;
use DBD::SQLite;
use PublicInbox::Smsg;
use Compress::Zlib qw(uncompress);
use constant DEFAULT_LIMIT => 1000;

sub dbh_new {
	my ($self, $rw) = @_;
	my $f = $self->{filename};
	if ($rw && !-f $f) { # SQLite defaults mode to 0644, we want 0666
		open my $fh, '+>>', $f or die "failed to open $f: $!";
	}
	my (@st, $st, $dbh);
	my $tries = 0;
	do {
		@st = stat($f) or die "failed to stat $f: $!";
		$st = pack('dd', $st[0], $st[1]); # 0: dev, 1: inode
		$dbh = DBI->connect("dbi:SQLite:dbname=$f",'','', {
			AutoCommit => 1,
			RaiseError => 1,
			PrintError => 0,
			ReadOnly => !$rw,
			sqlite_use_immediate_transaction => 1,
		});
		$self->{st} = $st;
		@st = stat($f) or die "failed to stat $f: $!";
		$st = pack('dd', $st[0], $st[1]);
	} while ($st ne $self->{st} && $tries++ < 3);
	warn "W: $f: .st_dev, .st_ino unstable\n" if $st ne $self->{st};
	$dbh->{sqlite_unicode} = 1;
	$dbh;
}

sub new {
	my ($class, $f) = @_;
	bless { filename => $f }, $class;
}

sub disconnect { $_[0]->{dbh} = undef }

sub connect { $_[0]->{dbh} ||= $_[0]->dbh_new }

sub load_from_row ($;$) {
	my ($smsg, $cull) = @_;
	bless $smsg, 'PublicInbox::Smsg';
	if (defined(my $data = delete $smsg->{ddd})) {
		$data = uncompress($data);
		utf8::decode($data);
		PublicInbox::Smsg::load_from_data($smsg, $data);

		# saves over 600K for 1000+ message threads
		PublicInbox::Smsg::psgi_cull($smsg) if $cull;
	}
	$smsg
}

sub do_get {
	my ($self, $sql, $opts, @args) = @_;
	my $dbh = $self->connect;
	my $lim = (($opts->{limit} || 0) + 0) || DEFAULT_LIMIT;
	$sql .= "LIMIT $lim";
	my $msgs = $dbh->selectall_arrayref($sql, { Slice => {} }, @args);
	my $cull = $opts->{cull};
	load_from_row($_, $cull) for @$msgs;
	$msgs
}

sub query_xover {
	my ($self, $beg, $end) = @_;
	do_get($self, <<'', {}, $beg, $end);
SELECT num,ts,ds,ddd FROM over WHERE num >= ? AND num <= ?
ORDER BY num ASC

}

sub query_ts {
	my ($self, $ts, $prev) = @_;
	do_get($self, <<'', {}, $ts, $prev);
SELECT num,ddd FROM over WHERE ts >= ? AND num > ?
ORDER BY num ASC

}

sub nothing () { wantarray ? (0, []) : [] };

sub get_thread {
	my ($self, $mid, $prev) = @_;
	my $dbh = $self->connect;
	my $opts = { cull => 1 };

	my $id = $dbh->selectrow_array(<<'', undef, $mid);
SELECT id FROM msgid WHERE mid = ? LIMIT 1

	defined $id or return nothing;

	my $num = $dbh->selectrow_array(<<'', undef, $id);
SELECT num FROM id2num WHERE id = ? AND num > 0
ORDER BY num ASC LIMIT 1

	defined $num or return nothing;

	my ($tid, $sid) = $dbh->selectrow_array(<<'', undef, $num);
SELECT tid,sid FROM over WHERE num = ? LIMIT 1

	defined $tid or return nothing; # $sid may be undef

	my $cond_all = '(tid = ? OR sid = ?) AND num > ?';
	my $sort_col = 'ds';
	$num = 0;
	if ($prev) { # mboxrd stream, only
		$num = $prev->{num} || 0;
		$sort_col = 'num';
	}

	my $cols = 'num,ts,ds,ddd';
	unless (wantarray) {
		return do_get($self, <<"", $opts, $tid, $sid, $num);
SELECT $cols FROM over WHERE $cond_all
ORDER BY $sort_col ASC

	}

	# HTML view always wants an array and never uses $prev,
	# but the mbox stream never wants an array and always has $prev
	die '$prev not supported with wantarray' if $prev;
	my $nr = $dbh->selectrow_array(<<"", undef, $tid, $sid, $num);
SELECT COUNT(num) FROM over WHERE $cond_all

	# giant thread, prioritize strict (tid) matches and throw
	# in the loose (sid) matches at the end
	my $msgs = do_get($self, <<"", $opts, $tid, $num);
SELECT $cols FROM over WHERE tid = ? AND num > ?
ORDER BY $sort_col ASC

	# do we have room for loose matches? get the most recent ones, first:
	my $lim = DEFAULT_LIMIT - scalar(@$msgs);
	if ($lim > 0) {
		$opts->{limit} = $lim;
		my $loose = do_get($self, <<"", $opts, $tid, $sid, $num);
SELECT $cols FROM over WHERE tid != ? AND sid = ? AND num > ?
ORDER BY $sort_col DESC

		# TODO separate strict and loose matches here once --reindex
		# is fixed to preserve `tid' properly
		push @$msgs, @$loose;
	}
	($nr, $msgs);
}

sub recent {
	my ($self, $opts, $after, $before) = @_;
	my ($s, @v);
	if (defined($before)) {
		if (defined($after)) {
			$s = '+num > 0 AND ts >= ? AND ts <= ? ORDER BY ts DESC';
			@v = ($after, $before);
		} else {
			$s = '+num > 0 AND ts <= ? ORDER BY ts DESC';
			@v = ($before);
		}
	} else {
		if (defined($after)) {
			$s = '+num > 0 AND ts >= ? ORDER BY ts ASC';
			@v = ($after);
		} else {
			$s = '+num > 0 ORDER BY ts DESC';
		}
	}
	my $msgs = do_get($self, <<"", $opts, @v);
SELECT ts,ds,ddd FROM over WHERE $s

	return $msgs unless wantarray;

	my $nr = $self->{dbh}->selectrow_array(<<'');
SELECT COUNT(num) FROM over WHERE num > 0

	($nr, $msgs);
}

sub get_art {
	my ($self, $num) = @_;
	my $dbh = $self->connect;
	my $sth = $dbh->prepare_cached(<<'', undef, 1);
SELECT num,ds,ts,ddd FROM over WHERE num = ? LIMIT 1

	$sth->execute($num);
	my $smsg = $sth->fetchrow_hashref;
	$smsg ? load_from_row($smsg) : undef;
}

sub next_by_mid {
	my ($self, $mid, $id, $prev) = @_;
	my $dbh = $self->connect;

	unless (defined $$id) {
		my $sth = $dbh->prepare_cached(<<'', undef, 1);
	SELECT id FROM msgid WHERE mid = ? LIMIT 1

		$sth->execute($mid);
		$$id = $sth->fetchrow_array;
		defined $$id or return;
	}
	my $sth = $dbh->prepare_cached(<<"", undef, 1);
SELECT num FROM id2num WHERE id = ? AND num > ?
ORDER BY num ASC LIMIT 1

	$$prev ||= 0;
	$sth->execute($$id, $$prev);
	my $num = $sth->fetchrow_array or return;
	$$prev = $num;

	$sth = $dbh->prepare_cached(<<"", undef, 1);
SELECT num,ts,ds,ddd FROM over WHERE num = ? LIMIT 1

	$sth->execute($num);
	my $smsg = $sth->fetchrow_hashref or return;
	load_from_row($smsg);
}

# IMAP search, this is limited by callers to UID_SLICE size (50K)
sub uid_range {
	my ($self, $beg, $end, $sql) = @_;
	my $dbh = $self->connect;
	my $q = 'SELECT num FROM over WHERE num >= ? AND num <= ?';

	# This is read-only, anyways; but caller should verify it's
	# only sending \A[0-9]+\z for ds and ts column ranges
	$q .= $$sql if $sql;
	$q .= ' ORDER BY num ASC';
	$dbh->selectcol_arrayref($q, undef, $beg, $end);
}

sub max {
	my ($self) = @_;
	my $sth = $self->connect->prepare_cached(<<'', undef, 1);
SELECT MAX(num) FROM over WHERE num > 0

	$sth->execute;
	$sth->fetchrow_array // 0;
}

sub imap_exists {
	my ($self, $uid_base, $uid_end) = @_;
	my $sth = $self->connect->prepare_cached(<<'', undef, 1);
SELECT COUNT(num) FROM over WHERE num > ? AND num <= ?

	$sth->execute($uid_base, $uid_end);
	$sth->fetchrow_array;
}

sub check_inodes {
	my ($self) = @_;
	if (my @st = stat($self->{filename})) { # did st_dev, st_ino change?
		my $st = pack('dd', $st[0], $st[1]);

		# don't actually reopen, just let {dbh} be recreated later
		delete($self->{dbh}) if ($st ne ($self->{st} // $st));
	} else {
		warn "W: stat $self->{filename}: $!\n";
	}
}

1;
