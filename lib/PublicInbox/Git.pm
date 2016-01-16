# Copyright (C) 2014-2015 all contributors <meta@public-inbox.org>
# License: GPLv2 or later (https://www.gnu.org/licenses/gpl-2.0.txt)
#
# Used to read files from a git repository without excessive forking.
# Used in our web interfaces as well as our -nntpd server.
# This is based on code in Git.pm which is GPLv2, but modified to avoid
# dependence on environment variables for compatibility with mod_perl.
# There are also API changes to simplify our usage and data set.
package PublicInbox::Git;
use strict;
use warnings;
use POSIX qw(dup2);
require IO::Handle;
use PublicInbox::Spawn qw(spawn popen_rd);

# Documentation/SubmittingPatches recommends 12 (Linux v4.4)
my $abbrev = `git config core.abbrev` || 12;

sub abbrev { "--abbrev=$abbrev" }

sub new {
	my ($class, $git_dir) = @_;
	bless { git_dir => $git_dir }, $class
}

sub _bidi_pipe {
	my ($self, $batch, $in, $out, $pid) = @_;
	return if $self->{$pid};
	my ($in_r, $in_w, $out_r, $out_w);

	pipe($in_r, $in_w) or fail($self, "pipe failed: $!");
	pipe($out_r, $out_w) or fail($self, "pipe failed: $!");

	my @cmd = ('git', "--git-dir=$self->{git_dir}", qw(cat-file), $batch);
	my $redir = { 0 => fileno($out_r), 1 => fileno($in_w) };
	$self->{$pid} = spawn(\@cmd, undef, $redir);
	$out_w->autoflush(1);
	$self->{$out} = $out_w;
	$self->{$in} = $in_r;
}

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
	@cmd = ('git', "--git-dir=$self->{git_dir}", @cmd);
	popen_rd(\@cmd);
}

sub qx {
	my ($self, @cmd) = @_;
	my $fh = $self->popen(@cmd);
	return <$fh> if wantarray;
	local $/;
	<$fh>
}

sub cleanup {
	my ($self) = @_;
	_destroy($self, qw(in out pid));
	_destroy($self, qw(in_c out_c pid_c));
}

sub DESTROY { cleanup(@_) }

1;
