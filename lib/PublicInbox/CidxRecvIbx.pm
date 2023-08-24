# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# dumps all per-inbox info for -cindex --associate
# integrated into the event loop for signalfd SIGINT handling
package PublicInbox::CidxRecvIbx;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Syscall qw(EPOLLIN EPOLLEXCLUSIVE);
use Socket qw(MSG_EOR);
use PublicInbox::CidxDumpIbx;

sub new {
	my ($cls, $cidx, $qry_str) = @_;
	my ($op_p, $r_ibx, $sort_w) = delete @$cidx{0..2};
	$op_p // die 'BUG: no $op_p';
	$r_ibx // die 'BUG: no $r_ibx';
	$sort_w // die 'BUG: no $sort_w';
	my $self = bless {}, $cls;
	$self->SUPER::new($r_ibx, EPOLLIN|EPOLLEXCLUSIVE);
	$self->{cidx} = $cidx;
	$self->{sort_w} = $sort_w;
	$self->{op_p} = $op_p; # PublicInbox::CidxDumpIbx uses this
	$self->{qry_str} = $qry_str;
	# writes to this pipe are never longer than POSIX PIPE_BUF,
	# so rely on POSIX atomicity guarantees
	$sort_w->autoflush(1);
	$self;
}

sub event_step {
	my ($self) = @_;
	recv($self->{sock}, my $ibx_id, 25, 0) // die "recv: $!";
	return $self->close if $ibx_id eq '' || $self->{cidx}->do_quit;
	PublicInbox::CidxDumpIbx::start($self, $ibx_id);
}

sub close {
	my ($self) = @_;
	$self->{cidx}->do_quit or
		send($self->{op_p},
			"recv_ibx_done $self->{cidx}->{shard}", MSG_EOR);
	$self->SUPER::close; # PublicInbox::DS::close
}

1;
