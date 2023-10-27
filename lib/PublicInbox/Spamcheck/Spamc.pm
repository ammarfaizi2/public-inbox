# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Default spam filter class for wrapping spamc(1)
package PublicInbox::Spamcheck::Spamc;
use v5.12;
use PublicInbox::Spawn qw(run_qx run_wait);
use IO::Handle;
use Fcntl qw(SEEK_SET);

sub new {
	my ($class) = @_;
	bless {
		checkcmd => [qw(spamc -E --headers)],
		hamcmd => [qw(spamc -L ham)],
		spamcmd => [qw(spamc -L spam)],
	}, $class;
}

sub spamcheck {
	my ($self, $msg, $out) = @_;

	$out = \(my $buf = '') unless ref($out);
	my $rdr = { 0 => _msg_to_fh($self, $msg) };
	$$out = run_qx($self->{checkcmd}, undef, $rdr);
	($? || $$out eq '') ? 0 : 1;
}

sub hamlearn {
	my ($self, $msg, $rdr) = @_;
	_learn($self, $msg, $rdr, 'hamcmd');
}

sub spamlearn {
	my ($self, $msg, $rdr) = @_;
	_learn($self, $msg, $rdr, 'spamcmd');
}

sub _learn {
	my ($self, $msg, $rdr, $field) = @_;
	$rdr ||= {};
	$rdr->{0} = _msg_to_fh($self, $msg);
	$rdr->{1} ||= $self->_devnull;
	$rdr->{2} ||= $self->_devnull;
	0 == run_wait($self->{$field}, undef, $rdr);
}

sub _devnull {
	my ($self) = @_;
	$self->{-devnull} //= do {
		open my $fh, '+>', '/dev/null' or
				die "failed to open /dev/null: $!";
		$fh
	}
}

sub _msg_to_fh {
	my ($self, $msg) = @_;
	if (my $ref = ref($msg)) {
		my $fd = eval { fileno($msg) };
		return $msg if defined($fd) && $fd >= 0;

		open(my $tmpfh, '+>', undef) or die "failed to open: $!";
		$tmpfh->autoflush(1);
		$msg = \($msg->as_string) if $ref ne 'SCALAR';
		print $tmpfh $$msg or die "failed to print: $!";
		sysseek($tmpfh, 0, SEEK_SET) or
			die "sysseek(fh) failed: $!";

		return $tmpfh;
	}
	$msg;
}

1;
