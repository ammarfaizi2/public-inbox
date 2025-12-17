# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Quick-n-dirty leak finder to detect leaks, since we've
# had to workaround anon sub leaks in Perl twice, now...
# This is NOT a cycle detector, since short-lived, carefully
# managed cycles are fine.  This exists mainly to find leaks
# due to bugs in Perl itself.
#
# Place the result of noleak() into any (array|hash)ref you don't
# want leaked.  When tests/tasks are complete, see if any
# $TMPDIR/pi-leak-* files remain, those files will contain
# backtraces to leaky codepaths
package PublicInbox::Leak;
use v5.12;
use parent qw(Exporter);
use File::Temp ();
use Carp ();
our @EXPORT = qw(noleak);

sub noleak () {
	my $ft = File::Temp->new(TEMPLATE => 'pi-leak-XXXXXX', TMPDIR => 1);
	say $ft Carp::longmess();
	$ft->flush or die "flush: $!";
	$ft;
}

1;
