# Copyright (C) all contributors <meta@public-inbox.org>
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
use parent qw(Exporter PublicInbox::DS);
use POSIX ();
use IO::Handle; # ->blocking
use Socket qw(AF_UNIX SOCK_STREAM);
use PublicInbox::Syscall qw(EPOLLIN EPOLLET);
use Errno qw(EINTR EAGAIN);
use File::Glob qw(bsd_glob GLOB_NOSORT);
use File::Spec ();
use Time::HiRes qw(stat);
use PublicInbox::Spawn qw(spawn popen_rd which);
use PublicInbox::Tmpfile;
use IO::Poll qw(POLLIN);
use Carp qw(croak carp);
use PublicInbox::SHA ();
our %HEXLEN2SHA = (40 => 1, 64 => 256);
our %OFMT2HEXLEN = (sha1 => 40, sha256 => 64);
our @EXPORT_OK = qw(git_unquote git_quote %HEXLEN2SHA %OFMT2HEXLEN);
our $in_cleanup;
our $RDTIMEO = 60_000; # milliseconds
our $async_warn; # true in read-only daemons

# committerdate:unix is git 2.9.4+ (2017-05-05), so using raw instead
my @MODIFIED_DATE = qw[for-each-ref --sort=-committerdate
			--format=%(committerdate:raw) --count=1];

use constant {
	MAX_INFLIGHT => 18, # arbitrary, formerly based on PIPE_BUF
	BATCH_CMD_VER => v2.36.0, # git 2.36+
};

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

my $EXE_ST = ''; # pack('dd', st_ctime, st_size);
my ($GIT_EXE, $GIT_VER);

sub check_git_exe () {
	$GIT_EXE = which('git') // die "git not found in $ENV{PATH}";
	my @st = stat($GIT_EXE) or die "stat: $!";
	my $st = pack('dd', $st[10], $st[7]);
	if ($st ne $EXE_ST) {
		my $rd = popen_rd([ $GIT_EXE, '--version' ]);
		my $v = readline($rd);
		CORE::close($rd) or die "$GIT_EXE --version: $?";
		$v =~ /\b([0-9]+(?:\.[0-9]+){2})/ or die
			"$GIT_EXE --version output: $v # unparseable";
		$GIT_VER = eval("v$1") // die "BUG: bad vstring: $1 ($v)";
		$EXE_ST = $st;
	}
}

sub git_version {
	check_git_exe();
	$GIT_VER;
}

# unquote pathnames used by git, see quote.c::unquote_c_style.c in git.git
sub git_unquote ($) {
	return $_[0] unless ($_[0] =~ /\A"(.*)"\z/);
	$_[0] = $1;
	$_[0] =~ s!\\([\\"abfnrtv]|[0-3][0-7]{2})!$GIT_ESC{$1}//chr(oct($1))!ge;
	$_[0];
}

sub git_quote ($) {
	if ($_[0] =~ s/([\\"\a\b\f\n\r\t\013]|[^[:print:]])/
		      '\\'.($ESC_GIT{$1}||sprintf("%03o",ord($1)))/egs) {
		return qq{"$_[0]"};
	}
	$_[0];
}

sub new {
	my ($class, $git_dir) = @_;
	$git_dir =~ tr!/!/!s;
	$git_dir =~ s!/*\z!!s;
	# may contain {-tmp} field for File::Temp::Dir
	bless { git_dir => $git_dir }, $class
}

sub git_path ($$) {
	my ($self, $path) = @_;
	$self->{-git_path}->{$path} //= do {
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
	return 0 if ($self->{alt_st} // '') eq $st;
	$self->{alt_st} = $st; # always a true value
}

sub object_format {
	$_[0]->{object_format} //= do {
		my $fmt = $_[0]->qx(qw(config extensions.objectformat));
		$fmt eq "sha256\n" ? \'sha256' : \undef;
	}
}

sub last_check_err {
	my ($self) = @_;
	my $fh = $self->{err_c} or return '';
	sysseek($fh, 0, 0) or $self->fail("sysseek: $!");
	my $size = -s $fh or return '';
	sysread($fh, my $buf, $size) // $self->fail("sysread: $!");
	truncate($fh, 0) or $self->fail("truncate: $!");
	$buf;
}

sub _sock_cmd {
	my ($self, $batch, $err_c) = @_;
	$self->{sock} and Carp::confess('BUG: {sock} exists');
	my ($s1, $s2);
	socketpair($s1, $s2, AF_UNIX, SOCK_STREAM, 0) or die "socketpair $!";
	$s1->blocking(0);
	my $opt = { pgid => 0, 0 => $s2, 1 => $s2 };
	my $gd = $self->{git_dir};
	if ($gd =~ s!/([^/]+/[^/]+)\z!/!) {
		$opt->{-C} = $gd;
		$gd = $1;
	}

	# git 2.31.0+ supports -c core.abbrev=no, don't bother with
	# core.abbrev=64 since not many releases had SHA-256 prior to 2.31
	my $abbr = $GIT_VER lt v2.31.0 ? 40 : 'no';
	my @cmd = ($GIT_EXE, "--git-dir=$gd", '-c', "core.abbrev=$abbr",
			'cat-file', "--$batch");
	if ($err_c) {
		my $id = "git.$self->{git_dir}.$batch.err";
		$self->{err_c} = $opt->{2} = tmpfile($id, undef, 1) or
						$self->fail("tmpfile($id): $!");
	}
	my $pid = spawn(\@cmd, undef, $opt);
	$self->{sock} = PublicInbox::ProcessPipe->maybe_new($pid, $s1);
}

sub poll_in ($) { IO::Poll::_poll($RDTIMEO, fileno($_[0]), my $ev = POLLIN) }

sub my_read ($$$) {
	my ($fh, $rbuf, $len) = @_;
	my $left = $len - length($$rbuf);
	my $r;
	while ($left > 0) {
		$r = sysread($fh, $$rbuf, $left, length($$rbuf));
		if ($r) {
			$left -= $r;
		} elsif (defined($r)) { # EOF
			return 0;
		} else {
			next if ($! == EAGAIN and poll_in($fh));
			next if $! == EINTR; # may be set by sysread or poll_in
			return; # unrecoverable error
		}
	}
	my $no_pad = substr($$rbuf, 0, $len, '');
	\$no_pad;
}

sub my_readline ($$) {
	my ($fh, $rbuf) = @_;
	while (1) {
		if ((my $n = index($$rbuf, "\n")) >= 0) {
			return substr($$rbuf, 0, $n + 1, '');
		}
		my $r = sysread($fh, $$rbuf, 65536, length($$rbuf)) and next;

		# return whatever's left on EOF
		return substr($$rbuf, 0, length($$rbuf)+1, '') if defined($r);

		next if ($! == EAGAIN and poll_in($fh));
		next if $! == EINTR; # may be set by sysread or poll_in
		return; # unrecoverable error
	}
}

sub cat_async_retry ($$) {
	my ($self, $old_inflight) = @_;

	# {inflight} may be non-existent, but if it isn't we delete it
	# here to prevent cleanup() from waiting:
	delete $self->{inflight};
	cleanup($self);
	my $new_inflight = batch_prepare($self);

	while (my ($oid, $cb, $arg) = splice(@$old_inflight, 0, 3)) {
		write_all($self, $oid."\n", \&cat_async_step, $new_inflight);
		$oid = \$oid if !@$new_inflight; # to indicate oid retried
		push @$new_inflight, $oid, $cb, $arg;
	}
	cat_async_step($self, $new_inflight); # take one step
}

# returns true if prefetch is successful
sub async_prefetch {
	my ($self, $oid, $cb, $arg) = @_;
	my $inflight = $self->{inflight} or return;
	return if @$inflight;
	substr($oid, 0, 0) = 'contents ' if $self->{-bc};
	write_all($self, "$oid\n", \&cat_async_step, $inflight);
	push(@$inflight, $oid, $cb, $arg);
}

sub cat_async_step ($$) {
	my ($self, $inflight) = @_;
	die 'BUG: inflight empty or odd' if scalar(@$inflight) < 3;
	my ($req, $cb, $arg) = @$inflight[0, 1, 2];
	my $rbuf = delete($self->{rbuf}) // \(my $new = '');
	my ($bref, $oid, $type, $size);
	my $head = my_readline($self->{sock}, $rbuf);
	my $cmd = ref($req) ? $$req : $req;
	# ->fail may be called via Gcf2Client.pm
	my $info = $self->{-bc} && substr($cmd, 0, 5) eq 'info ';
	if ($head =~ /^([0-9a-f]{40,}) (\S+) ([0-9]+)$/) {
		($oid, $type, $size) = ($1, $2, $3 + 0);
		unless ($info) { # --batch-command
			$bref = my_read($self->{sock}, $rbuf, $size + 1) or
				$self->fail(defined($bref) ?
						'read EOF' : "read: $!");
			chop($$bref) eq "\n" or
					$self->fail('LF missing after blob');
		}
	} elsif ($info && $head =~ / (missing|ambiguous)\n/) {
		$type = $1;
		$oid = substr($cmd, 5); # remove 'info '
	} elsif ($head =~ s/ missing\n//s) {
		$oid = $head;
		# ref($req) indicates it's already been retried
		# -gcf2 retries internally, so it never hits this path:
		if (!ref($req) && !$in_cleanup && $self->alternates_changed) {
			return cat_async_retry($self, $inflight);
		}
		$type = 'missing';
		if ($oid eq '') {
			$oid = $cmd;
			$oid =~ s/\A(?:contents|info) // if $self->{-bc};
		}
	} else {
		my $err = $! ? " ($!)" : '';
		$self->fail("bad result from async cat-file: $head$err");
	}
	$self->{rbuf} = $rbuf if $$rbuf ne '';
	splice(@$inflight, 0, 3); # don't retry $cb on ->fail
	eval { $cb->($bref, $oid, $type, $size, $arg) };
	async_err($self, $req, $oid, $@, $info ? 'check' : 'cat') if $@;
}

sub cat_async_wait ($) {
	my ($self) = @_;
	my $inflight = $self->{inflight} or return;
	while (scalar(@$inflight)) {
		cat_async_step($self, $inflight);
	}
}

sub batch_prepare ($) {
	my ($self) = @_;
	check_git_exe();
	if ($GIT_VER ge BATCH_CMD_VER) {
		$self->{-bc} = 1;
		_sock_cmd($self, 'batch-command', 1);
	} else {
		_sock_cmd($self, 'batch');
	}
	$self->{inflight} = [];
}

sub _cat_file_cb {
	my ($bref, $oid, $type, $size, $result) = @_;
	@$result = ($bref, $oid, $type, $size);
}

sub cat_file {
	my ($self, $oid) = @_;
	my $result = [];
	cat_async($self, $oid, \&_cat_file_cb, $result);
	cat_async_wait($self);
	wantarray ? @$result : $result->[0];
}

sub check_async_step ($$) {
	my ($ck, $inflight) = @_;
	die 'BUG: inflight empty or odd' if scalar(@$inflight) < 3;
	my ($req, $cb, $arg) = @$inflight[0, 1, 2];
	my $rbuf = delete($ck->{rbuf}) // \(my $new = '');
	chomp(my $line = my_readline($ck->{sock}, $rbuf));
	my ($hex, $type, $size) = split(/ /, $line);

	# git <2.21 would show `dangling' (2.21+ shows `ambiguous')
	# https://public-inbox.org/git/20190118033845.s2vlrb3wd3m2jfzu@dcvr/T/
	if ($hex eq 'dangling') {
		my $ret = my_read($ck->{sock}, $rbuf, $type + 1);
		$ck->fail(defined($ret) ? 'read EOF' : "read: $!") if !$ret;
	}
	$ck->{rbuf} = $rbuf if $$rbuf ne '';
	splice(@$inflight, 0, 3); # don't retry $cb on ->fail
	eval { $cb->(undef, $hex, $type, $size, $arg) };
	async_err($ck, $req, $hex, $@, 'check') if $@;
}

sub check_async_wait ($) {
	my ($self) = @_;
	return cat_async_wait($self) if $self->{-bc};
	my $ck = $self->{ck} or return;
	my $inflight = $ck->{inflight} or return;
	check_async_step($ck, $inflight) while (scalar(@$inflight));
}

# git <2.36
sub ck {
	$_[0]->{ck} //= bless { git_dir => $_[0]->{git_dir} },
				'PublicInbox::GitCheck';
}

sub check_async_begin ($) {
	my ($self) = @_;
	cleanup($self) if alternates_changed($self);
	check_git_exe();
	if ($GIT_VER ge BATCH_CMD_VER) {
		$self->{-bc} = 1;
		_sock_cmd($self, 'batch-command', 1);
	} else {
		_sock_cmd($self = ck($self), 'batch-check', 1);
	}
	$self->{inflight} = [];
}

sub write_all {
	my ($self, $buf, $read_step, $inflight) = @_;
	$self->{sock} // Carp::confess 'BUG: no {sock}';
	Carp::confess('BUG: not an array') if ref($inflight) ne 'ARRAY';
	$read_step->($self, $inflight) while @$inflight >= MAX_INFLIGHT;
	do {
		my $w = syswrite($self->{sock}, $buf);
		if (defined $w) {
			return if $w == length($buf);
			substr($buf, 0, $w, ''); # sv_chop
		} elsif ($! != EAGAIN) {
			$self->fail("write: $!");
		}
		$read_step->($self, $inflight);
	} while (1);
}

sub check_async ($$$$) {
	my ($self, $oid, $cb, $arg) = @_;
	my $inflight;
	if ($self->{-bc}) { # likely as time goes on
batch_command:
		$inflight = $self->{inflight} // cat_async_begin($self);
		substr($oid, 0, 0) = 'info ';
		write_all($self, "$oid\n", \&cat_async_step, $inflight);
	} else { # accounts for git upgrades while we're running:
		my $ck = $self->{ck}; # undef OK, maybe set in check_async_begin
		$inflight = $ck->{inflight} // check_async_begin($self);
		goto batch_command if $self->{-bc};
		write_all($self->{ck}, "$oid\n", \&check_async_step, $inflight);
	}
	push(@$inflight, $oid, $cb, $arg);
}

sub _check_cb { # check_async callback
	my (undef, $hex, $type, $size, $result) = @_;
	@$result = ($hex, $type, $size);
}

sub check {
	my ($self, $oid) = @_;
	my $result = [];
	check_async($self, $oid, \&_check_cb, $result);
	check_async_wait($self);
	my ($hex, $type, $size) = @$result;

	# git <2.21 would show `dangling' (2.21+ shows `ambiguous')
	# https://public-inbox.org/git/20190118033845.s2vlrb3wd3m2jfzu@dcvr/T/
	return if $type =~ /\A(?:missing|ambiguous)\z/ || $hex eq 'dangling';
	($hex, $type, $size);
}

sub fail {
	my ($self, $msg) = @_;
	$self->close;
	croak(ref($self) . ' ' . ($self->{git_dir} // '') . ": $msg");
}

sub async_err ($$$$$) {
	my ($self, $req, $oid, $err, $action) = @_;
	$req = $$req if ref($req); # retried
	my $msg = "E: $action $req ($oid): $err";
	$async_warn ? carp($msg) : $self->fail($msg);
}

# $git->popen(qw(show f00)); # or
# $git->popen(qw(show f00), { GIT_CONFIG => ... }, { 2 => ... });
sub popen {
	my ($self, $cmd) = splice(@_, 0, 2);
	$cmd = [ 'git', "--git-dir=$self->{git_dir}",
		ref($cmd) ? @$cmd : ($cmd, grep { defined && !ref } @_) ];
	popen_rd($cmd, grep { !defined || ref } @_); # env and opt
}

# same args as popen above
sub qx {
	my $fh = popen(@_);
	if (wantarray) {
		my @ret = <$fh>;
		CORE::close $fh; # caller should check $?
		@ret;
	} else {
		local $/;
		my $ret = <$fh>;
		CORE::close $fh; # caller should check $?
		$ret;
	}
}

sub date_parse {
	my $self = shift;
	map {
		substr($_, length('--max-age='), -1)
	} $self->qx('rev-parse', map { "--since=$_" } @_);
}

sub _active ($) {
	scalar(@{$_[0]->{inflight} // []}) ||
		($_[0]->{ck} && scalar(@{$_[0]->{ck}->{inflight} // []}))
}

# check_async and cat_async may trigger the other, so ensure they're
# both completely done by using this:
sub async_wait_all ($) {
	my ($self) = @_;
	while (_active($self)) {
		check_async_wait($self);
		cat_async_wait($self);
	}
}

# returns true if there are pending "git cat-file" processes
sub cleanup {
	my ($self, $lazy) = @_;
	return 1 if $lazy && _active($self);
	local $in_cleanup = 1;
	async_wait_all($self);
	$_->close for ($self, (delete($self->{ck}) // ()));
	undef;
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

sub DESTROY { cleanup($_[0]) }

sub local_nick ($) {
	# don't show full FS path, basename should be OK:
	$_[0]->{nick} // ($_[0]->{git_dir} =~ m!/([^/]+?)(?:/*\.git/*)?\z! ?
			"$1.git" : undef);
}

sub host_prefix_url ($$) {
	my ($env, $url) = @_;
	return $url if index($url, '//') >= 0;
	my $host_port = $env->{HTTP_HOST} //
		"$env->{SERVER_NAME}:$env->{SERVER_PORT}";
	my $sn = $env->{SCRIPT_NAME} // '';
	"$env->{'psgi.url_scheme'}://$host_port$sn/$url";
}

sub base_url { # for coderepos, PSGI-only
	my ($self, $env) = @_; # env - PSGI env
	my $nick = $self->{nick} // return undef;
	my $url = host_prefix_url($env, '');
	# for mount in Plack::Builder
	$url .= '/' if substr($url, -1, 1) ne '/';
	$url . $nick . '/';
}

sub isrch {} # TODO

sub pub_urls {
	my ($self, $env) = @_;
	if (my $urls = $self->{cgit_url}) {
		map { host_prefix_url($env, $_) } @$urls;
	} else {
		(base_url($self, $env) // '???');
	}
}

sub cat_async_begin {
	my ($self) = @_;
	cleanup($self) if $self->alternates_changed;
	die 'BUG: already in async' if $self->{inflight};
	batch_prepare($self);
}

sub cat_async ($$$;$) {
	my ($self, $oid, $cb, $arg) = @_;
	my $inflight = $self->{inflight} // cat_async_begin($self);
	substr($oid, 0, 0) = 'contents ' if $self->{-bc};
	write_all($self, $oid."\n", \&cat_async_step, $inflight);
	push(@$inflight, $oid, $cb, $arg);
}

# returns the modified time of a git repo, same as the "modified" field
# of a grokmirror manifest
sub modified ($;$) {
	my $fh = $_[1] // popen($_[0], @MODIFIED_DATE);
	(split(/ /, <$fh> // time))[0] + 0; # integerize for JSON
}

sub try_cat {
	my ($path) = @_;
	open(my $fh, '<', $path) or return '';
	local $/;
	<$fh> // '';
}

sub cat_desc ($) {
	my $desc = try_cat($_[0]);
	chomp $desc;
	utf8::decode($desc);
	$desc =~ s/\s+/ /smg;
	$desc eq '' ? undef : $desc;
}

sub description {
	cat_desc("$_[0]->{git_dir}/description") // 'Unnamed repository';
}

sub cloneurl {
	my ($self, $env) = @_;
	$self->{cloneurl} // do {
		my @urls = split(/\s+/s, try_cat("$self->{git_dir}/cloneurl"));
		scalar(@urls) ? ($self->{cloneurl} = \@urls) : undef;
	} // [ substr(base_url($self, $env), 0, -1) ];
}

# for grokmirror, which doesn't read gitweb.description
# templates/hooks--update.sample and git-multimail in git.git
# only match "Unnamed repository", not the full contents of
# templates/this--description in git.git
sub manifest_entry {
	my ($self, $epoch, $default_desc) = @_;
	check_git_exe();
	my $gd = $self->{git_dir};
	my @git = ($GIT_EXE, "--git-dir=$gd");
	my $sr = popen_rd([@git, 'show-ref']);
	my $own = popen_rd([@git, qw(config gitweb.owner)]);
	my $mod = popen_rd([@git, @MODIFIED_DATE]);
	my $buf = description($self);
	if (defined $epoch && index($buf, 'Unnamed repository') == 0) {
		$buf = "$default_desc [epoch $epoch]";
	}
	my $ent = { description => $buf, reference => undef };
	if (open(my $alt, '<', "$gd/objects/info/alternates")) {
		# n.b.: GitPython doesn't seem to handle comments or C-quoted
		# strings like native git does; and we don't for now, either.
		local $/ = "\n";
		chomp(my @alt = <$alt>);

		# grokmirror only supports 1 alternate for "reference",
		if (scalar(@alt) == 1) {
			$buf = File::Spec->rel2abs($alt[0], "$gd/objects");
			$buf =~ s!/[^/]+/?\z!!; # basename
			$ent->{reference} = $buf;
		}
	}
	my $dig = PublicInbox::SHA->new(1);
	while (read($sr, $buf, 65536)) { $dig->add($buf) }
	CORE::close $sr or return; # empty, uninitialized git repo
	$ent->{fingerprint} = $dig->hexdigest;
	$ent->{modified} = modified(undef, $mod);
	chomp($buf = <$own> // '');
	utf8::decode($buf);
	$ent->{owner} = $buf eq '' ? undef : $buf;
	$ent;
}

# returns true if there are pending cat-file processes
sub cleanup_if_unlinked {
	my ($self) = @_;
	return cleanup($self, 1) if $^O ne 'linux';
	# Linux-specific /proc/$PID/maps access
	# TODO: support this inside git.git
	my $ret = 0;
	for my $obj ($self, ($self->{ck} // ())) {
		my $sock = $obj->{sock} // next;
		my PublicInbox::ProcessPipe $pp = tied *$sock; # ProcessPipe
		my $pid = $pp->{pid} // next;
		open my $fh, '<', "/proc/$pid/maps" or return cleanup($self, 1);
		while (<$fh>) {
			# n.b. we do not restart for unlinked multi-pack-index
			# since it's not too huge, and the startup cost may
			# be higher.
			/\.(?:idx|pack) \(deleted\)$/ and
				return cleanup($self, 1);
		}
		++$ret;
	}
	$ret;
}

sub event_step {
	my ($self) = @_;
	$self->close if !$self->{sock}; # process died while requeued
	my $inflight = $self->{inflight};
	if ($inflight && @$inflight) {
		$self->cat_async_step($inflight);
		return $self->close unless $self->{sock};
		# don't loop here to keep things fair, but we must requeue
		# if there's already-read data in rbuf
		$self->requeue if exists($self->{rbuf});
	}
}

# idempotently registers with DS epoll/kqueue/select/poll
sub watch_async ($) {
	$_[0]->{epwatch} //= do {
		$_[0]->SUPER::new($_[0]->{sock}, EPOLLIN);
		\undef;
	}
}

sub close {
	my ($self) = @_;
	if (my $q = $self->{inflight}) { # abort inflight requests
		while (@$q) {
			my ($req, $cb, $arg) = splice(@$q, 0, 3);
			$req = $$req if ref($req);
			$self->{-bc} and $req =~ s/\A(?:contents|info) //;
			$req =~ s/ .*//; # drop git_dir for Gcf2Client
			eval { $cb->(undef, $req, undef, undef, $arg) };
			warn "E: (in abort) $req: $@" if $@;
		}
	}
	delete @$self{qw(-bc err_c inflight rbuf)};
	delete($self->{epwatch}) ? $self->SUPER::close : delete($self->{sock});
}

package PublicInbox::GitCheck; # only for git <2.36
use v5.12;
our @ISA = qw(PublicInbox::Git);
no warnings 'once';

# for event_step
*cat_async_step = \&PublicInbox::Git::check_async_step;

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
