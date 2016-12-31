# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# internal class used by PublicInbox::Git + Danga::Socket
# This parses the output pipe of "git cat-file --batch/--batch-check"
package PublicInbox::GitAsyncRd;
use strict;
use warnings;
use base qw(Danga::Socket);
use fields qw(jobq rbuf wr check);
use PublicInbox::GitAsyncWr;
our $MAX = 65536; # Import may bump this in the future

sub new {
	my ($class, $rd, $wr, $check) = @_;
	my $self = fields::new($class);
	IO::Handle::blocking($rd, 0);
	$self->SUPER::new($rd);
	$self->{jobq} = []; # [ [ $obj, $cb, $state ], ... ]
	my $buf = '';
	$self->{rbuf} = \$buf;
	$self->{wr} = PublicInbox::GitAsyncWr->new($wr);
	$self->{check} = $check;
	$self->watch_read(1);
	$self;
}

sub cat_file_async {
	my ($self, $obj, $cb) = @_;
	# order matters
	push @{$self->{jobq}}, [ $obj, $cb ];
	$self->{wr}->write($obj."\n");
}

# Returns: an array ref of the info line for --batch-check and --batch,
# which may be: [ $obj, 'missing']
# Returns undef on error
sub read_info ($) {
	my ($self) = @_;
	my $rbuf = $self->{rbuf};
	my $rd = $self->{sock};

	while (1) {
		$$rbuf =~ s/\A([^\n]+)\n//s and return [ split(/ /, $1) ];

		my $r = sysread($rd, $$rbuf, 110, length($$rbuf));
		next if $r;
		return $r;
	}
}

sub event_read {
	my ($self) = @_;
	my $jobq = $self->{jobq};
	my ($cur, $obj, $cb, $info, $left);
	my $check = $self->{check};
	my ($rbuf, $rlen, $need, $buf);
take_job:
	$cur = shift @$jobq or die 'BUG: empty job queue in '.__PACKAGE__;
	($obj, $cb, $info, $left) = @$cur;
	if (!$info) {
		$info = read_info($self);
		if (!defined $info && ($!{EAGAIN} || $!{EINTR})) {
			return unshift(@$jobq, $cur)
		}
		$cb->($info); # $info may 0 (EOF, or undef, $cb will see $!)
		return $self->close unless $info;
		if ($check || (scalar(@$info) != 3)) {
			# do not monopolize the event loop if we're drained:
			return if ${$self->{rbuf}} eq '';
			goto take_job;
		}
		$cur->[2] = $info;
		my $len = $info->[2];
		$left = \$len;
		$cur->[3] = $left; # onto reading body...
	}
	ref($left) or die 'BUG: $left not ref in '.__PACKAGE__;

	$rbuf = $self->{rbuf};
	$rlen = length($$rbuf);
	$need = $$left + 1; # +1 for trailing LF
	$buf = '';

	if ($rlen == $need) {
final_hunk:
		$self->{rbuf} = \$buf;
		$$left = undef;
		my $lf = chop $$rbuf;
		$lf eq "\n" or die "BUG: missing LF (got $lf)";
		$cb->($rbuf);

		return if $buf eq '';
		goto take_job;
	} elsif ($rlen < $need) {
		my $all = $need - $rlen;
		my $n = $all > $MAX ? $MAX : $all;
		my $r = sysread($self->{sock}, $$rbuf, $n, $rlen);
		if ($r) {
			goto final_hunk if $r == $all;

			# more to read later...
			$$left -= $r;
			$self->{rbuf} = \$buf;
			$cb->($rbuf);

			# don't monopolize the event loop
			return unshift(@$jobq, $cur);
		} elsif (!defined $r) {
			return unshift(@$jobq, $cur) if $!{EAGAIN} || $!{EINTR};
		}
		$cb->($r); # $cb should handle 0 and undef (and see $!)
		$self->close; # FAIL...
	} else { # too much data in rbuf
		$buf = substr($$rbuf, $need, $rlen - $need);
		$$rbuf = substr($$rbuf, 0, $need);
		goto final_hunk;
	}
}

sub close {
	my $self = shift;
	my $jobq = $self->{jobq};
	$self->{jobq} = [];
	$_->[1]->(0) for @$jobq;
	$self->{wr}->close;
	$self->SUPER::close(@_);
}

sub event_hup { $_[0]->close }
sub event_err { $_[0]->close }

1;
