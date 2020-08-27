# Copyright (C) 2014-2020 all contributors <meta@public-inbox.org>
# License: GPLv2 or later <https://www.gnu.org/licenses/gpl-2.0.txt>
#
# Used to read files from a git repository without excessive forking.
# Used in our web interfaces as well as our -nntpd server.
# This is based on code in Git.pm which is GPLv2+, but modified to avoid
# dependence on environment variables for compatibility with mod_perl.
# There are also API changes to simplify our usage and data set.
package PublicInbox::Git;
use strict;
use v5.10.1;
use parent qw(Exporter);
use POSIX ();
use IO::Handle; # ->autoflush
use Errno qw(EINTR);
use File::Glob qw(bsd_glob GLOB_NOSORT);
use Time::HiRes qw(stat);
use PublicInbox::Spawn qw(popen_rd);
use PublicInbox::Tmpfile;
use Carp qw(croak);
our @EXPORT_OK = qw(git_unquote git_quote);
our $PIPE_BUFSIZ = 65536; # Linux default
our $in_cleanup;

use constant MAX_INFLIGHT =>
	(($^O eq 'linux' ? 4096 : POSIX::_POSIX_PIPE_BUF()) * 3)
	/
	65; # SHA-256 hex size + "\n" in preparation for git using non-SHA1

my %GIT_ESC = (
	a => "\a",
	b => "\b",
	f => "\f",
	n => "\n",
	r => "\r",
	t => "\t",
	v => "\013",
	'"' => '"',
	'\\' => '\\',
);
my %ESC_GIT = map { $GIT_ESC{$_} => $_ } keys %GIT_ESC;


# unquote pathnames used by git, see quote.c::unquote_c_style.c in git.git
sub git_unquote ($) {
	return $_[0] unless ($_[0] =~ /\A"(.*)"\z/);
	$_[0] = $1;
	$_[0] =~ s/\\([\\"abfnrtv])/$GIT_ESC{$1}/g;
	$_[0] =~ s/\\([0-7]{1,3})/chr(oct($1))/ge;
	$_[0];
}

sub git_quote ($) {
	if ($_[0] =~ s/([\\"\a\b\f\n\r\t\013]|[^[:print:]])/
		      '\\'.($ESC_GIT{$1}||sprintf("%0o",ord($1)))/egs) {
		return qq{"$_[0]"};
	}
	$_[0];
}

sub new {
	my ($class, $git_dir) = @_;
	# may contain {-tmp} field for File::Temp::Dir
	bless { git_dir => $git_dir, alt_st => '', -git_path => {} }, $class
}

sub git_path ($$) {
	my ($self, $path) = @_;
	$self->{-git_path}->{$path} ||= do {
		local $/ = "\n";
		chomp(my $str = $self->qx(qw(rev-parse --git-path), $path));

		# git prior to 2.5.0 did not understand --git-path
		if ($str eq "--git-path\n$path") {
			$str = "$self->{git_dir}/$path";
		}
		$str;
	};
}

sub alternates_changed {
	my ($self) = @_;
	my $alt = git_path($self, 'objects/info/alternates');
	my @st = stat($alt) or return 0;

	# can't rely on 'q' on some 32-bit builds, but `d' works
	my $st = pack('dd', $st[10], $st[7]); # 10: ctime, 7: size
	return 0 if $self->{alt_st} eq $st;
	$self->{alt_st} = $st; # always a true value
}

sub last_check_err {
	my ($self) = @_;
	my $fh = $self->{err_c} or return;
	sysseek($fh, 0, 0) or fail($self, "sysseek failed: $!");
	defined(sysread($fh, my $buf, -s $fh)) or
			fail($self, "sysread failed: $!");
	$buf;
}

sub _bidi_pipe {
	my ($self, $batch, $in, $out, $pid, $err) = @_;
	if ($self->{$pid}) {
		if (defined $err) { # "err_c"
			my $fh = $self->{$err};
			sysseek($fh, 0, 0) or fail($self, "sysseek failed: $!");
			truncate($fh, 0) or fail($self, "truncate failed: $!");
		}
		return;
	}
	my ($out_r, $out_w);
	pipe($out_r, $out_w) or fail($self, "pipe failed: $!");
	my @cmd = (qw(git), "--git-dir=$self->{git_dir}",
			qw(-c core.abbrev=40 cat-file), $batch);
	my $redir = { 0 => $out_r };
	if ($err) {
		my $id = "git.$self->{git_dir}$batch.err";
		my $fh = tmpfile($id) or fail($self, "tmpfile($id): $!");
		$self->{$err} = $fh;
		$redir->{2} = $fh;
	}
	my ($in_r, $p) = popen_rd(\@cmd, undef, $redir);
	$self->{$pid} = $p;
	$out_w->autoflush(1);
	if ($^O eq 'linux') { # 1031: F_SETPIPE_SZ
		fcntl($out_w, 1031, 4096);
		fcntl($in_r, 1031, 4096) if $batch eq '--batch-check';
	}
	$self->{$out} = $out_w;
	$self->{$in} = $in_r;
}

sub my_read ($$$) {
	my ($fh, $rbuf, $len) = @_;
	my $left = $len - length($$rbuf);
	my $r;
	while ($left > 0) {
		$r = sysread($fh, $$rbuf, $PIPE_BUFSIZ, length($$rbuf));
		if ($r) {
			$left -= $r;
		} else {
			next if (!defined($r) && $! == EINTR);
			return $r;
		}
	}
	\substr($$rbuf, 0, $len, '');
}

sub my_readline ($$) {
	my ($fh, $rbuf) = @_;
	while (1) {
		if ((my $n = index($$rbuf, "\n")) >= 0) {
			return substr($$rbuf, 0, $n + 1, '');
		}
		my $r = sysread($fh, $$rbuf, $PIPE_BUFSIZ, length($$rbuf));
		next if $r || (!defined($r) && $! == EINTR);
		return defined($r) ? '' : undef; # EOF or error
	}
}

sub cat_async_retry ($$$$$) {
	my ($self, $inflight, $req, $cb, $arg) = @_;

	# {inflight} may be non-existent, but if it isn't we delete it
	# here to prevent cleanup() from waiting:
	delete $self->{inflight};
	cleanup($self);

	$self->{inflight} = $inflight;
	batch_prepare($self);
	my $buf = "$req\n";
	for (my $i = 0; $i < @$inflight; $i += 3) {
		$buf .= "$inflight->[$i]\n";
	}
	print { $self->{out} } $buf or fail($self, "write error: $!");
	unshift(@$inflight, \$req, $cb, $arg); # \$ref to indicate retried

	cat_async_step($self, $inflight); # take one step
}

sub cat_async_step ($$) {
	my ($self, $inflight) = @_;
	die 'BUG: inflight empty or odd' if scalar(@$inflight) < 3;
	my ($req, $cb, $arg) = splice(@$inflight, 0, 3);
	my $rbuf = delete($self->{cat_rbuf}) // \(my $new = '');
	my ($bref, $oid, $type, $size);
	my $head = my_readline($self->{in}, $rbuf);
	if ($head =~ /^([0-9a-f]{40}) (\S+) ([0-9]+)$/) {
		($oid, $type, $size) = ($1, $2, $3 + 0);
		$bref = my_read($self->{in}, $rbuf, $size + 1) or
			fail($self, defined($bref) ? 'read EOF' : "read: $!");
		chop($$bref) eq "\n" or fail($self, 'LF missing after blob');
	} elsif ($head =~ / missing$/) {
		# ref($req) indicates it's already been retried
		if (!ref($req) && !$in_cleanup && alternates_changed($self)) {
			return cat_async_retry($self, $inflight,
						$req, $cb, $arg);
		}
		$type = 'missing';
		$oid = ref($req) ? $$req : $req;
	} else {
		fail($self, "Unexpected result from async git cat-file: $head");
	}
	eval { $cb->($bref, $oid, $type, $size, $arg) };
	$self->{cat_rbuf} = $rbuf if $$rbuf ne '';
	warn "E: $oid: $@\n" if $@;
}

sub cat_async_wait ($) {
	my ($self) = @_;
	my $inflight = delete $self->{inflight} or return;
	while (scalar(@$inflight)) {
		cat_async_step($self, $inflight);
	}
}

sub batch_prepare ($) {
	_bidi_pipe($_[0], qw(--batch in out pid));
}

sub _cat_file_cb {
	my ($bref, undef, undef, $size, $result) = @_;
	@$result = ($bref, $size);
}

sub cat_file {
	my ($self, $oid, $sizeref) = @_;
	my $result = [];
	cat_async($self, $oid, \&_cat_file_cb, $result);
	cat_async_wait($self);
	$$sizeref = $result->[1] if $sizeref;
	$result->[0];
}

sub check_async_step ($$) {
	my ($self, $inflight_c) = @_;
	die 'BUG: inflight empty or odd' if scalar(@$inflight_c) < 3;
	my ($req, $cb, $arg) = splice(@$inflight_c, 0, 3);
	my $rbuf = delete($self->{rbuf_c}) // \(my $new = '');
	chomp(my $line = my_readline($self->{in_c}, $rbuf));
	my ($hex, $type, $size) = split(/ /, $line);

	# Future versions of git.git may have type=ambiguous, but for now,
	# we must handle 'dangling' below (and maybe some other oddball
	# stuff):
	# https://public-inbox.org/git/20190118033845.s2vlrb3wd3m2jfzu@dcvr/T/
	if ($hex eq 'dangling' || $hex eq 'notdir' || $hex eq 'loop') {
		my $ret = my_read($self->{in_c}, $rbuf, $type + 1);
		fail($self, defined($ret) ? 'read EOF' : "read: $!") if !$ret;
	}
	eval { $cb->($hex, $type, $size, $arg, $self) };
	warn "E: check($req) $@\n" if $@;
	$self->{rbuf_c} = $rbuf if $$rbuf ne '';
}

sub check_async_wait ($) {
	my ($self) = @_;
	my $inflight_c = delete $self->{inflight_c} or return;
	while (scalar(@$inflight_c)) {
		check_async_step($self, $inflight_c);
	}
}

sub check_async_begin ($) {
	my ($self) = @_;
	cleanup($self) if alternates_changed($self);
	_bidi_pipe($self, qw(--batch-check in_c out_c pid_c err_c));
	die 'BUG: already in async check' if $self->{inflight_c};
	$self->{inflight_c} = [];
}

sub check_async ($$$$) {
	my ($self, $oid, $cb, $arg) = @_;
	my $inflight_c = $self->{inflight_c} // check_async_begin($self);
	if (scalar(@$inflight_c) >= MAX_INFLIGHT) {
		check_async_step($self, $inflight_c);
	}
	print { $self->{out_c} } $oid, "\n" or fail($self, "write error: $!");
	push(@$inflight_c, $oid, $cb, $arg);
}

sub _check_cb { # check_async callback
	my ($hex, $type, $size, $result) = @_;
	@$result = ($hex, $type, $size);
}

sub check {
	my ($self, $oid) = @_;
	my $result = [];
	check_async($self, $oid, \&_check_cb, $result);
	check_async_wait($self);
	my ($hex, $type, $size) = @$result;

	# Future versions of git.git may show 'ambiguous', but for now,
	# we must handle 'dangling' below (and maybe some other oddball
	# stuff):
	# https://public-inbox.org/git/20190118033845.s2vlrb3wd3m2jfzu@dcvr/T/
	return if $type eq 'missing' || $type eq 'ambiguous';
	return if $hex eq 'dangling' || $hex eq 'notdir' || $hex eq 'loop';
	($hex, $type, $size);
}

sub _destroy {
	my ($self, $rbuf, $in, $out, $pid, $err) = @_;
	my $p = delete $self->{$pid} or return;
	delete @$self{($rbuf, $in, $out)};
	delete $self->{$err} if $err; # `err_c'

	# PublicInbox::DS may not be loaded
	eval { PublicInbox::DS::dwaitpid($p, undef, undef) };
	waitpid($p, 0) if $@; # wait synchronously if not in event loop
}

sub cat_async_abort ($) {
	my ($self) = @_;
	my $inflight = delete $self->{inflight} or die 'BUG: not in async';
	cleanup($self);
}

sub fail {
	my ($self, $msg) = @_;
	$self->{inflight} ? cat_async_abort($self) : cleanup($self);
	croak("git $self->{git_dir}: $msg");
}

sub popen {
	my ($self, @cmd) = @_;
	@cmd = ('git', "--git-dir=$self->{git_dir}", @cmd);
	popen_rd(\@cmd);
}

sub qx {
	my ($self, @cmd) = @_;
	my $fh = $self->popen(@cmd);
	local $/ = "\n";
	return <$fh> if wantarray;
	local $/;
	<$fh>
}

# returns true if there are pending "git cat-file" processes
sub cleanup {
	my ($self) = @_;
	local $in_cleanup = 1;
	delete $self->{async_cat};
	check_async_wait($self);
	cat_async_wait($self);
	_destroy($self, qw(cat_rbuf in out pid));
	_destroy($self, qw(chk_rbuf in_c out_c pid_c err_c));
	!!($self->{pid} || $self->{pid_c});
}

# assuming a well-maintained repo, this should be a somewhat
# accurate estimation of its size
# TODO: show this in the WWW UI as a hint to potential cloners
sub packed_bytes {
	my ($self) = @_;
	my $n = 0;
	my $pack_dir = git_path($self, 'objects/pack');
	foreach my $p (bsd_glob("$pack_dir/*.pack", GLOB_NOSORT)) {
		$n += -s $p;
	}
	$n
}

sub DESTROY { cleanup(@_) }

sub local_nick ($) {
	my ($self) = @_;
	my $ret = '???';
	# don't show full FS path, basename should be OK:
	if ($self->{git_dir} =~ m!/([^/]+)(?:/\.git)?\z!) {
		$ret = "/path/to/$1";
	}
	wantarray ? ($ret) : $ret;
}

sub host_prefix_url ($$) {
	my ($env, $url) = @_;
	return $url if index($url, '//') >= 0;
	my $scheme = $env->{'psgi.url_scheme'};
	my $host_port = $env->{HTTP_HOST} //
		"$env->{SERVER_NAME}:$env->{SERVER_PORT}";
	"$scheme://$host_port". ($env->{SCRIPT_NAME} || '/') . $url;
}

sub pub_urls {
	my ($self, $env) = @_;
	if (my $urls = $self->{cgit_url}) {
		return map { host_prefix_url($env, $_) } @$urls;
	}
	local_nick($self);
}

sub cat_async_begin {
	my ($self) = @_;
	cleanup($self) if alternates_changed($self);
	batch_prepare($self);
	die 'BUG: already in async' if $self->{inflight};
	$self->{inflight} = [];
}

sub cat_async ($$$;$) {
	my ($self, $oid, $cb, $arg) = @_;
	my $inflight = $self->{inflight} // cat_async_begin($self);
	if (scalar(@$inflight) >= MAX_INFLIGHT) {
		cat_async_step($self, $inflight);
	}

	print { $self->{out} } $oid, "\n" or fail($self, "write error: $!");
	push(@$inflight, $oid, $cb, $arg);
}

# this is safe to call inside $cb, but not guaranteed to enqueue
# returns true if successful, undef if not.
sub async_prefetch {
	my ($self, $oid, $cb, $arg) = @_;
	if (defined($self->{async_cat}) && (my $inflight = $self->{inflight})) {
		# we could use MAX_INFLIGHT here w/o the halving,
		# but lets not allow one client to monopolize a git process
		if (scalar(@$inflight) < int(MAX_INFLIGHT/2)) {
			print { $self->{out} } $oid, "\n" or
						fail($self, "write error: $!");
			return push(@$inflight, $oid, $cb, $arg);
		}
	}
	undef;
}

sub extract_cmt_time {
	my ($bref, undef, undef, undef, $modified) = @_;

	if ($$bref =~ /^committer .*?> ([0-9]+) [\+\-]?[0-9]+/sm) {
		my $cmt_time = $1 + 0;
		$$modified = $cmt_time if $cmt_time > $$modified;
	}
}

# returns the modified time of a git repo, same as the "modified" field
# of a grokmirror manifest
sub modified ($) {
	my ($self) = @_;
	my $modified = 0;
	my $fh = popen($self, qw(rev-parse --branches));
	local $/ = "\n";
	while (my $oid = <$fh>) {
		chomp $oid;
		cat_async($self, $oid, \&extract_cmt_time, \$modified);
	}
	cat_async_wait($self);
	$modified || time;
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
