# Copyright (C) 2014-2015 all contributors <meta@public-inbox.org>
# License: GPLv2 or later (https://www.gnu.org/licenses/gpl-2.0.txt)
#
# Used to read files from a git repository without excessive forking.
# Used in our web interfaces as well as our -nntpd server.
# This is based on code in Git.pm which is GPLv2+, but modified to avoid
# dependence on environment variables for compatibility with mod_perl.
# There are also API changes to simplify our usage and data set.
package PublicInbox::Git;
use strict;
use warnings;
use POSIX qw(dup2);
require IO::Handle;
use PublicInbox::Spawn qw(spawn popen_rd);
use Fcntl qw(:seek);
my $have_async = eval {
	require PublicInbox::EvCleanup;
	require PublicInbox::GitAsyncRd;
};

# Documentation/SubmittingPatches recommends 12 (Linux v4.4)
my $abbrev = `git config core.abbrev` || 12;

sub abbrev { "--abbrev=$abbrev" }

sub new {
	my ($class, $git_dir) = @_;
	bless { git_dir => $git_dir }, $class
}

sub err_begin ($) {
	my $err = $_[0]->{err};
	unless ($err) {
		open($err, '+>', undef);
		$_[0]->{err} = $err;
	}
	sysseek($err, 0, SEEK_SET) or die "sysseek failed: $!";
	truncate($err, 0) or die "truncate failed: $!";
	my $ret = fileno($err);
	defined $ret or die "fileno failed: $!";
	$ret;
}

sub err ($) {
	my $err = $_[0]->{err} or return '';
	sysseek($err, 0, SEEK_SET) or die "sysseek failed: $!";
	defined(sysread($err, my $buf, -s $err)) or die "sysread failed: $!";
	sysseek($err, 0, SEEK_SET) or die "sysseek failed: $!";
	truncate($err, 0) or die "truncate failed: $!";
	$buf;
}

sub _bidi_pipe {
	my ($self, $batch, $in, $out, $pid) = @_;
	return if $self->{$pid};
	my ($in_r, $in_w, $out_r, $out_w);

	pipe($in_r, $in_w) or fail($self, "pipe failed: $!");
	pipe($out_r, $out_w) or fail($self, "pipe failed: $!");

	my @cmd = ('git', "--git-dir=$self->{git_dir}", qw(cat-file), $batch);
	my $redir = { 0 => fileno($out_r), 1 => fileno($in_w) };
	my $p = spawn(\@cmd, undef, $redir);
	defined $p or fail($self, "spawn failed: $!");
	$self->{$pid} = $p;
	$out_w->autoflush(1);
	$self->{$out} = $out_w;
	$self->{$in} = $in_r;
}

# legacy synchronous API
sub cat_file_begin {
	my ($self, $obj) = @_;
	$self->_bidi_pipe(qw(--batch in out pid));
	$self->{out}->print($obj, "\n") or fail($self, "write error: $!");

	my $in = $self->{in};
	local $/ = "\n";
	my $head = $in->getline;
	$head =~ / missing$/ and return undef;
	$head =~ /^([0-9a-f]{40}) (\S+) (\d+)$/ or
		fail($self, "Unexpected result from git cat-file: $head");

	($in, $1, $2, $3);
}

# legacy synchronous API
sub cat_file_finish {
	my ($self, $left) = @_;
	my $max = 8192;
	my $in = $self->{in};
	my $buf;
	while ($left > 0) {
		my $r = read($in, $buf, $left > $max ? $max : $left);
		defined($r) or fail($self, "read failed: $!");
		$r == 0 and fail($self, 'exited unexpectedly');
		$left -= $r;
	}

	my $r = read($in, $buf, 1);
	defined($r) or fail($self, "read failed: $!");
	fail($self, 'newline missing after blob') if ($r != 1 || $buf ne "\n");
}

# legacy synchronous API
sub cat_file {
	my ($self, $obj, $ref) = @_;

	my ($in, $hex, $type, $size) = $self->cat_file_begin($obj);
	return unless $in;
	my $ref_type = $ref ? ref($ref) : '';

	my $rv;
	my $left = $size;
	$$ref = $size if ($ref_type eq 'SCALAR');
	my $cb_err;

	if ($ref_type eq 'CODE') {
		$rv = eval { $ref->($in, \$left, $type, $hex) };
		$cb_err = $@;
	} else {
		my $offset = 0;
		my $buf = '';
		while ($left > 0) {
			my $r = read($in, $buf, $left, $offset);
			defined($r) or fail($self, "read failed: $!");
			$r == 0 and fail($self, 'exited unexpectedly');
			$left -= $r;
			$offset += $r;
		}
		$rv = \$buf;
	}
	$self->cat_file_finish($left);
	die $cb_err if $cb_err;

	$rv;
}

sub batch_prepare ($) { _bidi_pipe($_[0], qw(--batch in out pid)) }

# legacy synchronous API
sub check {
	my ($self, $obj) = @_;
	$self->_bidi_pipe(qw(--batch-check in_c out_c pid_c));
	$self->{out_c}->print($obj, "\n") or fail($self, "write error: $!");
	local $/ = "\n";
	chomp(my $line = $self->{in_c}->getline);
	my ($hex, $type, $size) = split(' ', $line);
	return if $type eq 'missing';
	($hex, $type, $size);
}

sub _destroy {
	my ($self, $in, $out, $pid) = @_;
	my $p = delete $self->{$pid} or return;
	foreach my $f ($in, $out) {
		delete $self->{$f};
	}
	waitpid $p, 0;
}

sub fail {
	my ($self, $msg) = @_;
	cleanup($self);
	die $msg;
}

sub popen {
	my ($self, @cmd) = @_;
	my $cmd = [ 'git', "--git-dir=$self->{git_dir}" ];
	my ($env, $opt);
	if (ref $cmd[0]) {
		push @$cmd, @{$cmd[0]};
		$env = $cmd[1];
		$opt = $cmd[2];
	} else {
		push @$cmd, @cmd;
	}
	popen_rd($cmd, $env, $opt);
}

sub qx {
	my ($self, @cmd) = @_;
	my $fh = $self->popen(@cmd);
	defined $fh or return;
	local $/ = "\n";
	return <$fh> if wantarray;
	local $/;
	<$fh>
}

sub cleanup {
	my ($self) = @_;
	_destroy($self, qw(in out pid));
	_destroy($self, qw(in_c out_c pid_c));

	if ($have_async) {
		my %h = %$self; # yup, copy ourselves
		%$self = ();
		my $ds_closed;

		# schedule closing with Danga::Socket::close:
		foreach (qw(async async_c)) {
			my $ds = delete $h{$_} or next;
			$ds->close;
			$ds_closed = 1;
		}

		# can't do waitpid in _destroy() until next tick,
		# since D::S defers closing until end of current event loop
		$ds_closed and PublicInbox::EvCleanup::next_tick(sub {
			_destroy(\%h, qw(in_a out_a pid_a));
			_destroy(\%h, qw(in_ac out_ac pid_ac));
		});
	}
}

sub DESTROY { cleanup(@_) }

# modern async API
sub check_async_ds ($$$) {
	my ($self, $obj, $cb) = @_;
	($self->{async_c} ||= do {
		_bidi_pipe($self, qw(--batch-check in_ac out_ac pid_ac));
		PublicInbox::GitAsyncRd->new($self->{in_ac}, $self->{out_ac}, 1)
	})->cat_file_async($obj, $cb);
}

sub cat_async_ds ($$$) {
	my ($self, $obj, $cb) = @_;
	($self->{async} ||= do {
		_bidi_pipe($self, qw(--batch in_a out_a pid_a));
		PublicInbox::GitAsyncRd->new($self->{in_a}, $self->{out_a});
	})->cat_file_async($obj, $cb);
}

sub async_info_compat ($) {
	local $/ = "\n";
	chomp(my $line = $_[0]->getline);
	[ split(/ /, $line) ];
}

sub check_async_compat ($$$) {
	my ($self, $obj, $cb) = @_;
	$self->_bidi_pipe(qw(--batch-check in_c out_c pid_c));
	$self->{out_c}->print($obj."\n") or fail($self, "write error: $!");
	my $info = async_info_compat($self->{in_c});
	$cb->($info);
}

sub cat_async_compat ($$$) {
	my ($self, $obj, $cb) = @_;
	$self->_bidi_pipe(qw(--batch in out pid));
	$self->{out}->print($obj."\n") or fail($self, "write error: $!");
	my $in = $self->{in};
	my $info = async_info_compat($in);
	$cb->($info);
	return if scalar(@$info) != 3; # missing
	my $max = 8192;
	my $left = $info->[2];
	my ($buf, $r);
	while ($left > 0) {
		$r = read($in, $buf, $left > $max ? $max : $left);
		return $cb->($r) unless $r; # undef or 0
		$left -= $r;
		$cb->(\$buf);
	}
	$r = read($in, $buf, 1);
	defined($r) or fail($self, "read failed: $!");
	fail($self, 'newline missing after blob') if ($r != 1 || $buf ne "\n");
}

if ($have_async) {
	*check_async = *check_async_ds;
	*cat_async = *cat_async_ds;
} else {
	*check_async = *check_async_compat;
	*cat_async = *cat_async_compat;
}

1;
__END__
=pod

=head1 NAME

PublicInbox::Git - git wrapper

=head1 VERSION

version 1.0

=head1 SYNOPSIS

	use PublicInbox::Git;
	chomp(my $git_dir = `git rev-parse --git-dir`);
	$git_dir or die "GIT_DIR= must be specified\n";
	my $git = PublicInbox::Git->new($git_dir);

=head1 DESCRIPTION

Unstable API outside of the L</new> method.
It requires L<git(1)> to be installed.

=head1 METHODS

=cut

=head2 new

	my $git = PublicInbox::Git->new($git_dir);

Initialize a new PublicInbox::Git object for use with L<PublicInbox::Import>
This is the only public API method we support.  Everything else
in this module is subject to change.

=head1 SEE ALSO

L<Git>, L<PublicInbox::Import>

=head1 CONTACT

All feedback welcome via plain-text mail to L<mailto:meta@public-inbox.org>

The mail archives are hosted at L<https://public-inbox.org/meta/>

=head1 COPYRIGHT

Copyright (C) 2016 all contributors L<mailto:meta@public-inbox.org>

License: AGPL-3.0+ L<http://www.gnu.org/licenses/agpl-3.0.txt>

=cut
