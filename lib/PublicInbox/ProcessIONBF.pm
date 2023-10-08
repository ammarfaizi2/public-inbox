# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# used to support unbuffered partial reads
package PublicInbox::ProcessIONBF;
use v5.12;
use parent qw(PublicInbox::ProcessIO);
use IO::Handle; # ->blocking

sub new {
	my ($cls, $pid, $fh, @cb_arg) = @_;
	$fh->blocking(0) // die "$fh->blocking(0): $!";
	my $io = $cls->SUPER::maybe_new($pid, $fh, @cb_arg);
}

sub replace {
	my ($cls, $orig) = @_;
	my $pio = tied *$orig; # ProcessIO
	$pio->{fh}->blocking(0) // die "$pio->{fh}->blocking(0): $!";
	bless $pio, $cls;
}

sub READ { sysread($_[0]->{fh}, $_[1], $_[2], $_[3] // 0) }

1;
