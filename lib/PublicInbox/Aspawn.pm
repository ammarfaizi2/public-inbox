# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# async system()/qx() which takes callback
package PublicInbox::Aspawn;
use v5.12;
use parent qw(Exporter);
use PublicInbox::DS qw(awaitpid);
use PublicInbox::Spawn qw(spawn);
our @EXPORT_OK = qw(run_await);

sub _await_cb { # awaitpid cb
	my ($pid, $cmd, $env, $opt, $cb, @args) = @_;
	PublicInbox::Spawn::read_out_err($opt);
	if ($? && !$opt->{quiet}) {
		my ($status, $sig) = ($? >> 8, $? & 127);
		my $msg = '';
		$msg .= " (-C=$opt->{-C})" if defined $opt->{-C};
		$msg .= " status=$status" if $status;
		$msg .= " signal=$sig" if $sig;
		warn "E: @$cmd", $msg, "\n";
	}
	$cb->($pid, $cmd, $env, $opt, @args) if $cb;
}

sub run_await {
	my ($cmd, $env, $opt, $cb, @args) = @_;
	$opt->{1} //= \(my $out);
	my $pid = spawn($cmd, $env, $opt);
	awaitpid($pid, \&_await_cb, $cmd, $env, $opt, $cb, @args);
	awaitpid($pid); # synchronous for non-$in_loop
}

1;
