# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# common stuff for administrative command-line tools
# Unstable internal API
package PublicInbox::Admin;
use v5.12;
use parent qw(Exporter);
use autodie qw(chdir open);
our @EXPORT_OK = qw(setup_signals fmt_localtime);
use PublicInbox::Config;
use PublicInbox::Inbox;
use PublicInbox::Spawn qw(run_qx);
use PublicInbox::Eml;
use PublicInbox::Git qw(git_exe);
*rel2abs_collapsed = \&PublicInbox::Config::rel2abs_collapsed;

sub setup_signals {
	my ($cb, $arg) = @_; # optional
	require POSIX;

	# we call exit() here instead of _exit() so DESTROY methods
	# get called (e.g. File::Temp::Dir and PublicInbox::Msgmap)
	$SIG{INT} = $SIG{HUP} = $SIG{PIPE} = $SIG{TERM} = sub {
		my ($sig) = @_;
		# https://www.tldp.org/LDP/abs/html/exitcodes.html
		eval { $cb->($sig, $arg) } if $cb;
		$sig = 'SIG'.$sig;
		exit(128 + POSIX->$sig);
	};
}

sub resolve_any_idxdir ($$) {
	my ($cd, $lock_bn) = @_;
	my $try = $cd // '.';
	my $root_dev_ino;
	while (1) {
		if (-f "$try/$lock_bn") { # inbox.lock, ei.lock, cidx.lock
			return rel2abs_collapsed($try);
		} elsif (-d $try) {
			my @try = stat _;
			$root_dev_ino //= do {
				my @root = stat('/') or die "stat /: $!\n";
				"$root[0]\0$root[1]";
			};
			return undef if "$try[0]\0$try[1]" eq $root_dev_ino;
			$try .= '/..'; # continue, cd up
		} else {
			die "`$try' is not a directory\n";
		}
	}
}

sub resolve_eidxdir ($) { resolve_any_idxdir($_[0], 'ei.lock') }
sub resolve_cidxdir ($) { resolve_any_idxdir($_[0], 'cidx.lock') }

sub resolve_inboxdir {
	my ($cd, $ver) = @_;
	my $dir;
	if (defined($dir = resolve_any_idxdir($cd, 'inbox.lock'))) { # try v2
		$$ver = 2 if $ver;
	} elsif (defined($dir = resolve_git_dir($cd))) { # try v1
		$$ver = 1 if $ver;
	} # else: not an inbox at all
	$dir;
}

sub valid_pwd {
	my $pwd = $ENV{PWD} // return;
	my @st_pwd = stat $pwd or return;
	my @st_cwd = stat '.' or die "stat(.): $!";
	"@st_pwd[1,0]" eq "@st_cwd[1,0]" ? $pwd : undef;
}

sub resolve_git_dir {
	my ($cd) = @_; # cd may be `undef' for cwd
	# try v1 bare git dirs
	my $pwd = valid_pwd();
	my $env;
	defined($pwd) && substr($cd // '/', 0, 1) ne '/' and
		$env->{PWD} = "$pwd/$cd";
	my $cmd = [ git_exe, qw(rev-parse --git-dir) ];
	my $dir = run_qx($cmd, $env, { -C => $cd });
	die "error in @$cmd (cwd:${\($cd // '.')}): $?\n" if $?;
	chomp $dir;
	# --absolute-git-dir requires git v2.13.0+, and we want to
	# respect symlinks when $ENV{PWD} if $ENV{PWD} ne abs_path('.')
	# since we store absolute GIT_DIR paths in cindex.
	if (substr($dir, 0, 1) ne '/') {
		substr($cd // '/', 0, 1) eq '/' or
			$cd = File::Spec->rel2abs($cd, $pwd);
		$dir = rel2abs_collapsed($dir, $cd);
	}
	$dir;
}

sub unconfigured_ibx ($$) {
	my ($dir, $i) = @_;
	my $name = "unconfigured-$i";
	PublicInbox::Inbox->new({
		name => $name,
		address => [ "$name\@example.com" ],
		inboxdir => $dir,
		# consumers (-convert) warn on this:
		-unconfigured => 1,
	});
}

sub resolve_inboxes ($;$$) {
	my ($argv, $opt, $cfg) = @_;
	$opt ||= {};

	$cfg //= PublicInbox::Config->new;
	if ($opt->{all}) {
		my $cfgfile = PublicInbox::Config::default_file();
		$cfg or die "--all specified, but $cfgfile not readable\n";
		@$argv and die "--all specified, but directories specified\n";
	}
	my (@old, @ibxs, @eidx, @cidx);
	if ($opt->{-cidx_ok}) {
		require PublicInbox::CodeSearchIdx;
		@$argv = grep {
			if (defined(my $d = resolve_cidxdir($_))) {
				push @cidx, PublicInbox::CodeSearchIdx->new(
							$d, $opt);
				undef;
			} else {
				1;
			}
		} @$argv;
	}
	if ($opt->{-eidx_ok}) {
		require PublicInbox::ExtSearchIdx;
		@$argv = grep {
			if (defined(my $ei = resolve_eidxdir($_))) {
				$ei = PublicInbox::ExtSearchIdx->new($ei, $opt);
				push @eidx, $ei;
				undef;
			} else {
				1;
			}
		} @$argv;
	}
	my $min_ver = $opt->{-min_inbox_version} || 0;
	# lookup inboxes by st_dev + st_ino instead of {inboxdir} pathnames,
	# pathnames are not unique due to symlinks and bind mounts
	if ($opt->{all}) {
		$cfg->each_inbox(sub {
			my ($ibx) = @_;
			if (-e $ibx->{inboxdir}) {
				push(@ibxs, $ibx) if $ibx->version >= $min_ver;
			} else {
				warn "W: $ibx->{name} $ibx->{inboxdir}: $!\n";
			}
		});
		# TODO: no way to configure cindex in config file, yet
	} else { # directories specified on the command-line
		my @dirs = @$argv;
		push @dirs, '.' if !@dirs && $opt->{-use_cwd};
		my %s2i; # "st_dev\0st_ino" => array index
		for (my $i = 0; $i <= $#dirs; $i++) {
			my $dir = $dirs[$i];
			my @st = stat($dir) or die "stat($dir): $!\n";
			$dir = $dirs[$i] = resolve_inboxdir($dir, \(my $ver));
			if ($ver >= $min_ver) {
				$s2i{"$st[0]\0$st[1]"} //= $i;
			} else {
				push @old, $dir;
			}
		}
		my $done = \'done';
		eval {
			$cfg->each_inbox(sub {
				my ($ibx) = @_;
				return if $ibx->version < $min_ver;
				my $dir = $ibx->{inboxdir};
				if (my @s = stat $dir) {
					my $i = delete($s2i{"$s[0]\0$s[1]"})
						// return;
					$ibxs[$i] = $ibx;
					die $done if !keys(%s2i);
				} else {
					warn "W: $ibx->{name} $dir: $!\n";
				}
			});
		};
		die $@ if $@ && $@ ne $done;
		for my $i (sort { $a <=> $b } values %s2i) {
			$ibxs[$i] = unconfigured_ibx($dirs[$i], $i);
		}
		@ibxs = grep { defined } @ibxs; # duplicates are undef
	}
	if (@old) {
		die "-V$min_ver inboxes not supported by $0\n\t",
		    join("\n\t", @old), "\n";
	}
	($opt->{-eidx_ok} || $opt->{-cidx_ok}) ? (\@ibxs, \@eidx, \@cidx)
						: @ibxs;
}

my @base_mod = ();
my @over_mod = qw(DBD::SQLite DBI);
my %mod_groups = (
	-index => [ @base_mod, @over_mod ],
	-base => \@base_mod,
	-search => [ @base_mod, @over_mod, 'Xapian' ],
);

sub scan_ibx_modules ($$) {
	my ($mods, $ibx) = @_;
	if (!$ibx->{indexlevel} || $ibx->{indexlevel} ne 'basic') {
		$mods->{'Xapian'} = 1;
	} else {
		$mods->{$_} = 1 foreach @over_mod;
	}
}

sub check_require {
	my (@mods) = @_;
	my $err = {};
	while (my $mod = shift @mods) {
		if (my $groups = $mod_groups{$mod}) {
			push @mods, @$groups;
		} elsif ($mod eq 'Xapian') {
			require PublicInbox::Search;
			PublicInbox::Search::load_xapian() or
				$err->{'Xapian || Search::Xapian'} = $@;
		} else {
			eval "require $mod";
			$err->{$mod} = $@ if $@;
		}
	}
	scalar keys %$err ? $err : undef;
}

sub missing_mod_msg {
	my ($err) = @_;
	my @mods = map { "`$_'" } sort keys %$err;
	my $last = pop @mods;
	@mods ? (join(', ', @mods)."' and $last") : $last
}

sub require_or_die {
	my $err = check_require(@_) or return;
	die missing_mod_msg($err)." required for $0\n";
}

sub indexlevel_ok_or_die ($) {
	my ($indexlevel) = @_;
	my $req;
	if ($indexlevel eq 'basic') {
		$req = '-index';
	} elsif ($indexlevel =~ /\A(?:medium|full)\z/) {
		$req = '-search';
	} else {
		die <<"";
invalid indexlevel=$indexlevel (must be `basic', `medium', or `full')

	}
	my $err = check_require($req) or return;
	die missing_mod_msg($err) ." required for indexlevel=$indexlevel\n";
}

sub index_terminate {
	my (undef, $ibx) = @_; # $_[0] = signal name
	$ibx->git->cleanup;
}

sub warn_cb ($) {
	my ($self) = @_;
	my $cb = $SIG{__WARN__} || \&CORE::warn;
	sub {
		return if PublicInbox::Eml::warn_ignore(@_);
		my @m = @_;
		$self->{current_info} ne '' && @m && $m[0] =~ s/\A# // and
			unshift @m, '# ', $self->{current_info}, ': ';
		$cb->(@m);
	}
}

sub index_inbox {
	my ($ibx, $im, $opt) = @_;
	require PublicInbox::InboxWritable;
	my $jobs = delete $opt->{jobs} if $opt;
	if (my $pr = $opt->{-progress}) {
		$pr->("indexing $ibx->{inboxdir} ...\n");
	}
	local @SIG{keys %SIG} = values %SIG;
	setup_signals(\&index_terminate, $ibx);
	my $idx = { current_info => $ibx->{inboxdir} };
	local $SIG{__WARN__} = warn_cb $idx;
	if ($ibx->version == 2) {
		eval { require PublicInbox::V2Writable };
		die "v2 requirements not met: $@\n" if $@;
		$ibx->{-creat_opt}->{nproc} = $jobs;
		my $v2w = $im // $ibx->importer($opt->{reindex} // $jobs);
		if (defined $jobs) {
			if ($jobs == 0) {
				$v2w->{parallel} = 0;
			} else {
				my $n = $v2w->{shards};
				if ($jobs < ($n + 1) && !$opt->{reshard}) {
					warn <<EOM;
Unable to respect --jobs=$jobs on index, inbox was created with $n shards
EOM
				}
			}
		}
		$idx = $v2w;
	} else {
		require PublicInbox::SearchIdx;
		$idx = PublicInbox::SearchIdx->new($ibx, 1);
	}
	$idx->index_sync($opt);
	$idx->{nidx} // 0; # returns number processed
}

sub progress_prepare ($;$) {
	my ($opt, $dst) = @_;

	# public-inbox-index defaults to quiet, -xcpdb and -compact do not
	if (defined($opt->{quiet}) && $opt->{quiet} < 0) {
		$opt->{quiet} = !$opt->{verbose};
	}
	if ($opt->{quiet}) {
		open $opt->{1}, '>', '/dev/null'; # suitable for spawn() redirect
	} else {
		$opt->{verbose} ||= 1;
		$dst //= \*STDERR;
		$opt->{-progress} = sub { print $dst '# ', @_ };
	}
}

# same unit factors as git:
sub parse_unsigned ($) {
	my ($val) = @_;

	$$val =~ /\A([0-9]+)([kmg])?\z/i or return;
	my ($n, $unit_factor) = ($1, $2 // '');
	my %u = ( k => 1024, m => 1024**2, g => 1024**3 );
	$$val = $n * ($u{lc($unit_factor)} // 1);
	1;
}

sub index_prepare ($$) {
	my ($opt, $cfg) = @_;
	my $env;
	if ($opt->{compact}) {
		require PublicInbox::Xapcmd;
		PublicInbox::Xapcmd::check_compact();
		$opt->{compact_opt} = { -coarse_lock => 1, compact => 1 };
		if (defined(my $jobs = $opt->{jobs})) {
			$opt->{compact_opt}->{jobs} = $jobs;
		}
	}
	for my $k (qw(max_size batch_size)) {
		my $git_key = "publicInbox.index".ucfirst($k);
		$git_key =~ s/_([a-z])/\U$1/g;
		defined(my $v = $opt->{$k} // $cfg->{lc($git_key)}) or next;
		parse_unsigned(\$v) or die "`$git_key=$v' not parsed\n";
		$v > 0 or die "`$git_key=$v' must be positive\n";
		$opt->{$k} = $v;
	}

	# out-of-the-box builds of Xapian 1.4.x are still limited to 32-bit
	# https://getting-started-with-xapian.readthedocs.io/en/latest/concepts/indexing/limitations.html
	$opt->{batch_size} and
		$env = { XAPIAN_FLUSH_THRESHOLD => '4294967295' };

	for my $k (qw(sequential-shard)) {
		my $git_key = "publicInbox.index".ucfirst($k);
		$git_key =~ s/-([a-z])/\U$1/g;
		defined(my $s = $opt->{$k} // $cfg->{lc($git_key)}) or next;
		defined(my $v = $cfg->git_bool($s))
					or die "`$git_key=$s' not boolean\n";
		$opt->{$k} = $v;
	}
	for my $k (qw(since until)) {
		my $v = $opt->{$k} // next;
		$opt->{reindex} or die "--$k=$v requires --reindex\n";
	}
	$env;
}

sub do_chdir ($) {
	my $chdir = $_[0] // return;
	for my $d (@$chdir) {
		next if $d eq ''; # same as git(1)
		chdir $d;
	}
}

sub fmt_localtime ($) {
	require POSIX;
	my @lt = localtime $_[0];
	my (undef, $M, $H, $d, $m, $Y) = @lt;
	sprintf('%u-%02u-%02u % 2u:%02u ', $Y + 1900, $m + 1, $d, $H, $M)
		.POSIX::strftime('%z', @lt);
}

1;
