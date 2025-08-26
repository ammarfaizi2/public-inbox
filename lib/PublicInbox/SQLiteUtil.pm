# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# common bits for SQLite users in our codebase
package PublicInbox::SQLiteUtil;
use v5.12;
use autodie qw(open);

my %SQLITE_GLOB_MAP = (
	'[' => '[[]',
	']' => '[]]',
	'*' => '[*]',
	'?' => '[?]'
);

# n.b. GLOB doesn't seem to work on data inserted w/ SQL_BLOB
sub escape_glob ($) {
	my ($s) = @_;
	$s =~ s/([\[\]\*\?])/$SQLITE_GLOB_MAP{$1}/sge;
	$s;
}

# DBD::SQLite maps REGEXP to use perlre, and that works on SQL_BLOB
# whereas GLOB and LIKE don't seem to...
sub mk_sqlite_re ($$) {
	my ($pfx, $anywhere) = @_;
	ref($pfx) ? $pfx # assume qr// Regexp
		: ($anywhere ? '.*' : '^')."\Q$pfx\E.*";
}

sub create_db ($;$) {
	my ($f, $opt) = @_;
	my ($dir) = ($f =~ m!(.+)/[^/]+\z!);
	unless ($opt->{cow}) {
		require PublicInbox::Syscall;
		PublicInbox::Syscall::nodatacow_dir($dir); # for journal/shm/wal
	}
	# SQLite defaults mode to 0644, we want 0666 to respect umask
	open my $fh, '+>>', $f;
}

1;
