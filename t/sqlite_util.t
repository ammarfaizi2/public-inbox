#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
require_mods 'DBD::SQLite';
use_ok 'PublicInbox::SQLiteUtil';
require DBI;
DBI->import(':sql_types');

my $dbh = DBI->connect('dbi:SQLite:dbname=:memory:', '', '', {
	AutoCommit => 1,
	RaiseError => 1,
	PrintError => 0,
	sqlite_use_immediate_transaction => 1,
});

$dbh->do('CREATE TABLE test (key BLOB NOT NULL, UNIQUE (key))');

my $ins = $dbh->prepare('INSERT INTO test (key) VALUES (?)');
my $sel = $dbh->prepare('SELECT key FROM test WHERE key GLOB ?');
my $non_utf8 = "h\x{e5}llo[wor]ld!";
my $us_ascii = 'h*llo[wor]ld?';

$dbh->begin_work;
my @SQL_BLOB = (SQL_BLOB());
@SQL_BLOB = (); # FIXME: can't get GLOB to work w/ SQL_BLOB
for my $k ($us_ascii, $non_utf8) {
	$ins->bind_param(1, $k, @SQL_BLOB);
	$ins->execute;
}
$dbh->commit;

$sel->bind_param(1, '*', @SQL_BLOB);
$sel->execute;
my $rows = $sel->fetchall_arrayref;
is scalar(@$rows), 2, q[`*' got everything];

$sel->bind_param(1, PublicInbox::SQLiteUtil::escape_glob($us_ascii), @SQL_BLOB);
$sel->execute;
$rows = $sel->fetchall_arrayref;
is_deeply $rows, [ [ $us_ascii ] ], 'US-ASCII exact match';

$sel->bind_param(1, PublicInbox::SQLiteUtil::escape_glob($non_utf8), @SQL_BLOB);
$sel->execute;
$rows = $sel->fetchall_arrayref;
is_deeply $rows, [ [ $non_utf8 ] ], 'ISO-8859-1 exact match';

done_testing;
