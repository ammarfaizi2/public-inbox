# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# supports reaping of children tied to a pipe or socket
package PublicInbox::IO;
use v5.12;
use parent qw(IO::Handle Exporter);
use PublicInbox::DS qw(awaitpid);
our @EXPORT_OK = qw(poll_in read_all try_cat write_file);
use Carp qw(croak);
use IO::Poll qw(POLLIN);

sub waitcb { # awaitpid callback
	my ($pid, $errref, $cb, @args) = @_;
	$$errref = $?; # sets .cerr for _close
	$cb->($pid, @args) if $cb;
}

sub attach_pid ($$;@) {
	my ($io, $pid, @cb_arg) = @_;
	bless $io, __PACKAGE__;
	# we share $err (and not $self) with awaitpid to avoid a ref cycle
	${*$io}{pi_io_reap} = [ $$, $pid, \(my $err) ];
	awaitpid($pid, \&waitcb, \$err, @cb_arg);
	$io;
}

sub attached_pid {
	my ($io) = @_;
	${${*$io}{pi_io_reap} // []}[1];
}

# caller cares about error result if they call close explicitly
# reap->[2] may be set before this is called via waitcb
sub close {
	my ($io) = @_;
	my $ret = $io->SUPER::close;
	my $reap = delete ${*$io}{pi_io_reap};
	return $ret unless $reap && $reap->[0] == $$;
	${$reap->[2]} // (my $w = awaitpid($reap->[1])); # sets [2]
	($? = ${$reap->[2]}) ? '' : $ret;
}

sub DESTROY {
	my ($io) = @_;
	my $reap = delete ${*$io}{pi_io_reap};
	if ($reap && $reap->[0] == $$) {
		$io->SUPER::close;
		awaitpid($reap->[1]);
	}
	$io->SUPER::DESTROY;
}

sub write_file ($$@) { # mode, filename, LIST (for print)
	use autodie qw(open close);
	open(my $fh, shift, shift);
	print $fh @_;
	defined(wantarray) && !wantarray ? $fh : close $fh;
}

sub poll_in ($;$) {
	IO::Poll::_poll($_[1] // -1, fileno($_[0]), my $ev = POLLIN);
}

sub read_all ($;$$) {
	use autodie qw(read);
	my ($io, $len, $bref) = @_;
	$bref //= \(my $buf);
	my $r = read($io, $$bref, $len //= -s $io);
	croak("read($io) ($r != $len)") if $len != $r;
	$$bref;
}

sub try_cat ($) {
	my ($path) = @_;
	open(my $fh, '<', $path) or return '';
	read_all $fh;
}

1;
