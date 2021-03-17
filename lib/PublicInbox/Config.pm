# Copyright (C) 2014-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Used throughout the project for reading configuration
#
# Note: I hate camelCase; but git-config(1) uses it, but it's better
# than alllowercasewithoutunderscores, so use lc('configKey') where
# applicable for readability

package PublicInbox::Config;
use strict;
use v5.10.1;
use PublicInbox::Inbox;
use PublicInbox::Spawn qw(popen_rd);

sub _array ($) { ref($_[0]) eq 'ARRAY' ? $_[0] : [ $_[0] ] }

# returns key-value pairs of config directives in a hash
# if keys may be multi-value, the value is an array ref containing all values
sub new {
	my ($class, $file) = @_;
	$file //= default_file();
	my $self;
	if (ref($file) eq 'SCALAR') { # used by some tests
		open my $fh, '<', $file or die;  # PerlIO::scalar
		$self = config_fh_parse($fh, "\n", '=');
	} else {
		$self = git_config_dump($file);
		$self->{'-f'} = $file;
	}
	bless $self, $class;
	# caches
	$self->{-by_addr} = {};
	$self->{-by_list_id} = {};
	$self->{-by_name} = {};
	$self->{-by_newsgroup} = {};
	$self->{-by_eidx_key} = {};
	$self->{-no_obfuscate} = {};
	$self->{-limiters} = {};
	$self->{-code_repos} = {}; # nick => PublicInbox::Git object
	$self->{-cgitrc_unparsed} = $self->{'publicinbox.cgitrc'};

	if (my $no = delete $self->{'publicinbox.noobfuscate'}) {
		$no = _array($no);
		my @domains;
		foreach my $n (@$no) {
			my @n = split(/\s+/, $n);
			foreach (@n) {
				if (/\S+@\S+/) { # full address
					$self->{-no_obfuscate}->{lc $_} = 1;
				} else {
					# allow "example.com" or "@example.com"
					s/\A@//;
					push @domains, quotemeta($_);
				}
			}
		}
		my $nod = join('|', @domains);
		$self->{-no_obfuscate_re} = qr/(?:$nod)\z/i;
	}
	if (my $css = delete $self->{'publicinbox.css'}) {
		$self->{css} = _array($css);
	}

	$self;
}

sub noop {}
sub fill_all ($) { each_inbox($_[0], \&noop) }

sub _lookup_fill ($$$) {
	my ($self, $cache, $key) = @_;
	$self->{$cache}->{$key} // do {
		fill_all($self);
		$self->{$cache}->{$key};
	}
}

sub lookup {
	my ($self, $recipient) = @_;
	_lookup_fill($self, '-by_addr', lc($recipient));
}

sub lookup_list_id {
	my ($self, $list_id) = @_;
	_lookup_fill($self, '-by_list_id', lc($list_id));
}

sub lookup_name ($$) {
	my ($self, $name) = @_;
	$self->{-by_name}->{$name} // _fill_ibx($self, $name);
}

sub lookup_ei {
	my ($self, $name) = @_;
	$self->{-ei_by_name}->{$name} //= _fill_ei($self, $name);
}

# special case for [extindex "all"]
sub ALL { lookup_ei($_[0], 'all') }

sub each_inbox {
	my ($self, $cb, @arg) = @_;
	# may auto-vivify if config file is non-existent:
	foreach my $section (@{$self->{-section_order}}) {
		next if $section !~ m!\Apublicinbox\.([^/]+)\z!;
		my $ibx = lookup_name($self, $1) or next;
		$cb->($ibx, @arg);
	}
}

sub lookup_newsgroup {
	my ($self, $ng) = @_;
	_lookup_fill($self, '-by_newsgroup', lc($ng));
}

sub limiter {
	my ($self, $name) = @_;
	$self->{-limiters}->{$name} //= do {
		require PublicInbox::Qspawn;
		my $max = $self->{"publicinboxlimiter.$name.max"} || 1;
		my $limiter = PublicInbox::Qspawn::Limiter->new($max);
		$limiter->setup_rlimit($name, $self);
		$limiter;
	};
}

sub config_dir { $ENV{PI_DIR} // "$ENV{HOME}/.public-inbox" }

sub default_file {
	$ENV{PI_CONFIG} // (config_dir() . '/config');
}

sub config_fh_parse ($$$) {
	my ($fh, $rs, $fs) = @_;
	my (%rv, %seen, @section_order, $line, $k, $v, $section, $cur, $i);
	local $/ = $rs;
	while (defined($line = <$fh>)) { # perf critical with giant configs
		$i = index($line, $fs);
		$k = substr($line, 0, $i);
		$v = substr($line, $i + 1, -1); # chop off $fs
		$section = substr($k, 0, rindex($k, '.'));
		$seen{$section} //= push(@section_order, $section);

		if (defined($cur = $rv{$k})) {
			if (ref($cur) eq "ARRAY") {
				push @$cur, $v;
			} else {
				$rv{$k} = [ $cur, $v ];
			}
		} else {
			$rv{$k} = $v;
		}
	}
	$rv{-section_order} = \@section_order;

	\%rv;
}

sub git_config_dump {
	my ($file) = @_;
	return {} unless -e $file;
	my $cmd = [ qw(git config -z -l --includes), "--file=$file" ];
	my $fh = popen_rd($cmd);
	my $rv = config_fh_parse($fh, "\0", "\n");
	close $fh or die "failed to close (@$cmd) pipe: $?";
	$rv;
}

sub valid_foo_name ($;$) {
	my ($name, $pfx) = @_;

	# Similar rules found in git.git/remote.c::valid_remote_nick
	# and git.git/refs.c::check_refname_component
	# We don't reject /\.lock\z/, however, since we don't lock refs
	if ($name eq '' || $name =~ /\@\{/ ||
	    $name =~ /\.\./ || $name =~ m![/:\?\[\]\^~\s\f[:cntrl:]\*]! ||
	    $name =~ /\A\./ || $name =~ /\.\z/) {
		warn "invalid $pfx name: `$name'\n" if $pfx;
		return 0;
	}

	# Note: we allow URL-unfriendly characters; users may configure
	# non-HTTP-accessible inboxes
	1;
}

# XXX needs testing for cgit compatibility
# cf. cgit/scan-tree.c::add_repo
sub cgit_repo_merge ($$$) {
	my ($self, $base, $repo) = @_;
	my $path = $repo->{dir};
	if (defined(my $se = $self->{-cgit_strict_export})) {
		return unless -e "$path/$se";
	}
	return if -e "$path/noweb";
	# this comes from the cgit config, and AFAIK cgit only allows
	# repos to have one URL, but that's just the PATH_INFO component,
	# not the Host: portion
	# $repo = { url => 'foo.git', dir => '/path/to/foo.git' }
	my $rel = $repo->{url};
	unless (defined $rel) {
		my $off = index($path, $base, 0);
		if ($off != 0) {
			$rel = $path;
		} else {
			$rel = substr($path, length($base) + 1);
		}

		$rel =~ s!/\.git\z!! or
			$rel =~ s!/+\z!!;

		$self->{-cgit_remove_suffix} and
			$rel =~ s!/?\.git\z!!;
	}
	$self->{"coderepo.$rel.dir"} //= $path;
	$self->{"coderepo.$rel.cgiturl"} //= _array($rel);
}

sub is_git_dir ($) {
	my ($git_dir) = @_;
	-d "$git_dir/objects" && -f "$git_dir/HEAD";
}

# XXX needs testing for cgit compatibility
sub scan_path_coderepo {
	my ($self, $base, $path) = @_;
	opendir(my $dh, $path) or do {
		warn "error opening directory: $path\n";
		return
	};
	my $git_dir = $path;
	if (is_git_dir($git_dir) || is_git_dir($git_dir .= '/.git')) {
		my $repo = { dir => $git_dir };
		cgit_repo_merge($self, $base, $repo);
		return;
	}
	while (defined(my $dn = readdir $dh)) {
		next if $dn eq '.' || $dn eq '..';
		if (index($dn, '.') == 0 && !$self->{-cgit_scan_hidden_path}) {
			next;
		}
		my $dir = "$path/$dn";
		scan_path_coderepo($self, $base, $dir) if -d $dir;
	}
}

sub scan_tree_coderepo ($$) {
	my ($self, $path) = @_;
	scan_path_coderepo($self, $path, $path);
}

sub scan_projects_coderepo ($$$) {
	my ($self, $list, $path) = @_;
	open my $fh, '<', $list or do {
		warn "failed to open cgit projectlist=$list: $!\n";
		return;
	};
	while (<$fh>) {
		chomp;
		scan_path_coderepo($self, $path, "$path/$_");
	}
}

sub parse_cgitrc {
	my ($self, $cgitrc, $nesting) = @_;
	if ($nesting == 0) {
		# defaults:
		my %s = map { $_ => 1 } qw(/cgit.css /cgit.png
						/favicon.ico /robots.txt);
		$self->{-cgit_static} = \%s;
	}

	# same limit as cgit/configfile.c::parse_configfile
	return if $nesting > 8;

	open my $fh, '<', $cgitrc or do {
		warn "failed to open cgitrc=$cgitrc: $!\n";
		return;
	};

	# FIXME: this doesn't support macro expansion via $VARS, yet
	my $repo;
	while (<$fh>) {
		chomp;
		if (m!\Arepo\.url=(.+?)/*\z!) {
			my $nick = $1;
			cgit_repo_merge($self, $repo->{dir}, $repo) if $repo;
			$repo = { url => $nick };
		} elsif (m!\Arepo\.path=(.+)\z!) {
			if (defined $repo) {
				$repo->{dir} = $1;
			} else {
				warn "$_ without repo.url\n";
			}
		} elsif (m!\Ainclude=(.+)\z!) {
			parse_cgitrc($self, $1, $nesting + 1);
		} elsif (m!\A(scan-hidden-path|remove-suffix)=([0-9]+)\z!) {
			my ($k, $v) = ($1, $2);
			$k =~ tr/-/_/;
			$self->{"-cgit_$k"} = $v;
		} elsif (m!\A(project-list|strict-export)=(.+)\z!) {
			my ($k, $v) = ($1, $2);
			$k =~ tr/-/_/;
			$self->{"-cgit_$k"} = $v;
		} elsif (m!\Ascan-path=(.+)\z!) {
			if (defined(my $list = $self->{-cgit_project_list})) {
				scan_projects_coderepo($self, $list, $1);
			} else {
				scan_tree_coderepo($self, $1);
			}
		} elsif (m!\A(?:css|favicon|logo|repo\.logo)=(/.+)\z!) {
			# absolute paths for static files via PublicInbox::Cgit
			$self->{-cgit_static}->{$1} = 1;
		}
	}
	cgit_repo_merge($self, $repo->{dir}, $repo) if $repo;
}

# parse a code repo
# Only git is supported at the moment, but SVN and Hg are possibilities
sub _fill_code_repo {
	my ($self, $nick) = @_;
	my $pfx = "coderepo.$nick";

	# TODO: support gitweb and other repository viewers?
	if (defined(my $cgitrc = delete $self->{-cgitrc_unparsed})) {
		parse_cgitrc($self, $cgitrc, 0);
	}
	my $dir = $self->{"$pfx.dir"}; # aka "GIT_DIR"
	unless (defined $dir) {
		warn "$pfx.dir unset\n";
		return;
	}

	my $git = PublicInbox::Git->new($dir);
	foreach my $t (qw(blob commit tree tag)) {
		$git->{$t.'_url_format'} =
				_array($self->{lc("$pfx.${t}UrlFormat")});
	}

	if (defined(my $cgits = $self->{"$pfx.cgiturl"})) {
		$git->{cgit_url} = $cgits = _array($cgits);
		$self->{"$pfx.cgiturl"} = $cgits;

		# cgit supports "/blob/?id=%s", but it's only a plain-text
		# display and requires an unabbreviated id=
		foreach my $t (qw(blob commit tag)) {
			$git->{$t.'_url_format'} //= map {
				"$_/$t/?id=%s"
			} @$cgits;
		}
	}

	$git;
}

sub git_bool {
	my ($val) = $_[-1]; # $_[0] may be $self, or $val
	if ($val =~ /\A(?:false|no|off|[\-\+]?(?:0x)?0+)\z/i) {
		0;
	} elsif ($val =~ /\A(?:true|yes|on|[\-\+]?(?:0x)?[0-9]+)\z/i) {
		1;
	} else {
		undef;
	}
}

# abs_path resolves symlinks, so we want to avoid it if rel2abs
# is sufficient and doesn't leave "/.." or "/../"
sub rel2abs_collapsed {
	require File::Spec;
	my $p = File::Spec->rel2abs($_[-1]);
	return $p if substr($p, -3, 3) ne '/..' && index($p, '/../') < 0;
	require Cwd;
	Cwd::abs_path($p);
}

sub _one_val {
	my ($self, $pfx, $k) = @_;
	my $v = $self->{"$pfx.$k"} // return;
	return $v if !ref($v);
	warn "W: $pfx.$k has multiple values, only using `$v->[-1]'\n";
	$v->[-1];
}

sub _fill_ibx {
	my ($self, $name) = @_;
	my $pfx = "publicinbox.$name";
	my $ibx = {};
	for my $k (qw(watch nntpserver)) {
		my $v = $self->{"$pfx.$k"};
		$ibx->{$k} = $v if defined $v;
	}
	for my $k (qw(filter inboxdir newsgroup replyto httpbackendmax feedmax
			indexlevel indexsequentialshard)) {
		my $v = _one_val($self, $pfx, $k) // next;
		$ibx->{$k} = $v;
	}

	# "mainrepo" is backwards compatibility:
	my $dir = $ibx->{inboxdir} //= $self->{"$pfx.mainrepo"} // return;
	if (index($dir, "\n") >= 0) {
		warn "E: `$dir' must not contain `\\n'\n";
		return;
	}
	for my $k (qw(obfuscate)) {
		my $v = $self->{"$pfx.$k"} // next;
		if (defined(my $bval = git_bool($v))) {
			$ibx->{$k} = $bval;
		} else {
			warn "Ignoring $pfx.$k=$v in config, not boolean\n";
		}
	}
	# TODO: more arrays, we should support multi-value for
	# more things to encourage decentralization
	for my $k (qw(address altid nntpmirror coderepo hide listid url
			infourl watchheader)) {
		my $v = $self->{"$pfx.$k"} // next;
		$ibx->{$k} = _array($v);
	}

	return unless valid_foo_name($name, 'publicinbox');
	$ibx->{name} = $name;
	$ibx->{-pi_cfg} = $self;
	$ibx = PublicInbox::Inbox->new($ibx);
	foreach (@{$ibx->{address}}) {
		my $lc_addr = lc($_);
		$self->{-by_addr}->{$lc_addr} = $ibx;
		$self->{-no_obfuscate}->{$lc_addr} = 1;
	}
	if (my $listids = $ibx->{listid}) {
		# RFC2919 section 6 stipulates "case insensitive equality"
		foreach my $list_id (@$listids) {
			$self->{-by_list_id}->{lc($list_id)} = $ibx;
		}
	}
	if (defined(my $ngname = $ibx->{newsgroup})) {
		if (ref($ngname)) {
			delete $ibx->{newsgroup};
			warn 'multiple newsgroups not supported: '.
				join(', ', @$ngname). "\n";
		# Newsgroup name needs to be compatible with RFC 3977
		# wildmat-exact and RFC 3501 (IMAP) ATOM-CHAR.
		# Leave out a few chars likely to cause problems or conflicts:
		# '|', '<', '>', ';', '#', '$', '&',
		} elsif ($ngname =~ m![^A-Za-z0-9/_\.\-\~\@\+\=:]! ||
				$ngname eq '') {
			delete $ibx->{newsgroup};
			warn "newsgroup name invalid: `$ngname'\n";
		} else {
			# PublicInbox::NNTPD does stricter ->nntp_usable
			# checks, keep this lean for startup speed
			$self->{-by_newsgroup}->{$ngname} = $ibx;
		}
	}
	unless (defined $ibx->{newsgroup}) { # for ->eidx_key
		my $abs = rel2abs_collapsed($dir);
		if ($abs ne $dir) {
			warn "W: `$dir' canonicalized to `$abs'\n";
			$ibx->{inboxdir} = $abs;
		}
	}
	$self->{-by_name}->{$name} = $ibx;
	if ($ibx->{obfuscate}) {
		$ibx->{-no_obfuscate} = $self->{-no_obfuscate};
		$ibx->{-no_obfuscate_re} = $self->{-no_obfuscate_re};
		fill_all($self); # noop to populate -no_obfuscate
	}
	if (my $ibx_code_repos = $ibx->{coderepo}) {
		my $code_repos = $self->{-code_repos};
		my $repo_objs = $ibx->{-repo_objs} = [];
		foreach my $nick (@$ibx_code_repos) {
			my @parts = split(m!/!, $nick);
			my $valid = 0;
			$valid += valid_foo_name($_) foreach (@parts);
			$valid == scalar(@parts) or next;

			my $repo = $code_repos->{$nick} //=
						_fill_code_repo($self, $nick);
			push @$repo_objs, $repo if $repo;
		}
	}
	if (my $es = ALL($self)) {
		require PublicInbox::Isearch;
		$ibx->{isrch} = PublicInbox::Isearch->new($ibx, $es);
	}
	$self->{-by_eidx_key}->{$ibx->eidx_key} = $ibx;
}

sub _fill_ei ($$) {
	my ($self, $name) = @_;
	eval { require PublicInbox::ExtSearch } or return;
	my $pfx = "extindex.$name";
	my $d = $self->{"$pfx.topdir"} // return;
	-d $d or return;
	my $es = PublicInbox::ExtSearch->new($d);
	for my $k (qw(indexlevel indexsequentialshard)) {
		my $v = _one_val($self, $pfx, $k) // next;
		$es->{$k} = $v;
	}
	for my $k (qw(altid coderepo hide url infourl)) {
		my $v = $self->{"$pfx.$k"} // next;
		$es->{$k} = _array($v);
	}
	return unless valid_foo_name($name, 'extindex');
	$es->{name} = $name;
	$es;
}

sub urlmatch {
	my ($self, $key, $url) = @_;
	state $urlmatch_broken; # requires git 1.8.5
	return if $urlmatch_broken;
	my $file = $self->{'-f'} // default_file();
	my $cmd = [qw/git config -z --includes --get-urlmatch/,
		"--file=$file", $key, $url ];
	my $fh = popen_rd($cmd);
	local $/ = "\0";
	my $val = <$fh>;
	if (close($fh)) {
		chomp($val);
		$val;
	} else {
		$urlmatch_broken = 1 if (($? >> 8) != 1);
		undef;
	}
}

sub json {
	state $json;
	$json //= do {
		for my $mod (qw(Cpanel::JSON::XS JSON::MaybeXS JSON JSON::PP)) {
			eval "require $mod" or next;
			# ->ascii encodes non-ASCII to "\uXXXX"
			$json = $mod->new->ascii(1) and last;
		}
		$json;
	};
}

1;
