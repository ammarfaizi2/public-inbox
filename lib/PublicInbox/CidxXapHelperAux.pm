# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Intended for PublicInbox::DS::event_loop for -cindex --associate,
# this reports auxilliary status while dumping
package PublicInbox::CidxXapHelperAux;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN);

# rpipe connects to req->fp[1] in xap_helper.h
sub new {
	my ($cls, $rpipe, $cidx, $pfx) = @_;
	my $self = bless { cidx => $cidx, pfx => $pfx }, $cls;
	$rpipe->blocking(0);
	$self->SUPER::new($rpipe, EPOLLIN);
}

sub event_step {
	my ($self) = @_; # xap_helper.h is line-buffered
	my $buf = delete($self->{buf}) // '';
	my $n = sysread($self->{sock}, $buf, 65536, length($buf));
	if (!defined($n)) {
		return if $!{EAGAIN};
		die "sysread: $!";
	}
	my $pfx = $self->{pfx};
	if ($n == 0) {
		warn "BUG? $pfx buf=$buf" if $buf ne '';
		if (delete $self->{cidx}->{PENDING}->{$pfx}) {
			warn "BUG? $pfx did not get mset.size";
			$self->{cidx}->index_next;
		}
		return $self->close;
	}
	my @lines = split(/^/m, $buf);
	$self->{buf} = pop @lines if substr($lines[-1], -1) ne "\n";
	for my $l (@lines) {
		if ($l =~ /\Amset\.size=[0-9]+ nr_out=[0-9]+\n\z/) {
			delete $self->{cidx}->{PENDING}->{$pfx};
			$self->{cidx}->index_next;
		}
		chomp $l;
		$self->{cidx}->progress("$pfx $l");
	}
}

1;
