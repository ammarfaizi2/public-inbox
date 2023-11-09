# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# common reader code for IMAP and NNTP (and maybe JMAP)
package PublicInbox::NetReader;
use v5.12;
use parent qw(Exporter PublicInbox::IPC);
use PublicInbox::Eml;
use PublicInbox::Config;
our %IMAPflags2kw = map {; "\\\u$_" => $_ } qw(seen answered flagged draft);
$IMAPflags2kw{'$Forwarded'} = 'forwarded';  # RFC 5550

our @EXPORT = qw(uri_section imap_uri nntp_uri);

sub ndump {
	require Data::Dumper;
	Data::Dumper->new(\@_)->Useqq(1)->Terse(1)->Dump;
}

# returns the git config section name, e.g [imap "imaps://user@example.com"]
# without the mailbox, so we can share connections between different inboxes
sub uri_section ($) {
	my ($uri) = @_;
	$uri->scheme . '://' . $uri->authority;
}

sub socks_args ($) {
	my ($val) = @_;
	return if ($val // '') eq '';
	if ($val =~ m!\Asocks5h:// (?: \[ ([^\]]+) \] | ([^:/]+) )
					(?::([0-9]+))?/*\z!ix) {
		my ($h, $p) = ($1 // $2, $3 + 0);
		$h = '127.0.0.1' if $h eq '0';
		eval { require IO::Socket::Socks } or die <<EOM;
IO::Socket::Socks missing for socks5h://$h:$p
EOM
		# for IO::Socket::Socks
		return { ProxyAddr => $h, ProxyPort => $p };
	}
	die "$val not understood (only socks5h:// is supported)\n";
}

# gives an arrayref suitable for the Mail::IMAPClient Ssl or Starttls arg
sub mic_tls_opt ($$) {
	my ($o, $hostname) = @_;
	require IO::Socket::SSL;
	$o = {} if !ref($o);
	$o->{SSL_hostname} //= $hostname;
	[ map { ($_, $o->{$_}) } keys %$o ];
}

sub set_ssl_verify_mode ($$) {
	my ($o, $bool) = @_;
	require IO::Socket::SSL;
	$o->{SSL_verify_mode} = $bool ? IO::Socket::SSL::SSL_VERIFY_PEER() :
					IO::Socket::SSL::SSL_VERIFY_NONE();
}

sub mic_new ($$$$) {
	my ($self, $mic_arg, $sec, $uri) = @_;
	my %mic_arg = (%$mic_arg, Keepalive => 1);
	my $sa = $self->{cfg_opt}->{$sec}->{-proxy_cfg} || $self->{-proxy_cli};
	my ($mic, $s, $t);
	if ($sa) {
		# this `require' needed for worker[1..Inf], since socks_args
		# only got called in worker[0]
		require IO::Socket::Socks;
		my %opt = (%$sa, Keepalive => 1);
		$opt{SocksDebug} = 1 if $mic_arg{Debug};
		$opt{ConnectAddr} = delete $mic_arg{Server};
		$opt{ConnectPort} = delete $mic_arg{Port};
		do {
			$! = 0;
			$s = IO::Socket::Socks->new(%opt);
		} until ($s || !$!{EINTR} || $self->{quit});
		return if $self->{quit};
		$s or die "E: <$uri> ".eval('$IO::Socket::Socks::SOCKS_ERROR');
		$mic_arg{Socket} = $s;
		if (my $o = delete $mic_arg{Ssl}) { # for imaps://
			$o = mic_tls_opt($o, $opt{ConnectAddr});
			do {
				$! = 0;
				$t = IO::Socket::SSL->start_SSL($s, @$o);
			} until ($t || !$!{EINTR} || $self->{quit});
			return if $self->{quit};
			$t or die "E: <$uri> ".(IO::Socket::SSL->errstr // '');
			$mic_arg{Socket} = $t;
		} elsif ($o = $mic_arg{Starttls}) {
			# Mail::IMAPClient will use this:
			$mic_arg{Starttls} = mic_tls_opt($o, $opt{ConnectAddr});
		}
	} elsif ($mic_arg{Ssl} || $mic_arg{Starttls}) {
		for my $f (qw(Ssl Starttls)) {
			my $o = $mic_arg{$f} or next;
			$mic_arg{$f} = mic_tls_opt($o, $mic_arg{Server});
		}
	}
	do {
		$! = 0;
		$mic = PublicInbox::IMAPClient->new(%mic_arg);
	} until ($mic || !$!{EINTR} || $self->{quit});
	$mic;
}

sub auth_anon_cb { '' }; # for Mail::IMAPClient::Authcallback

sub onion_hint ($$) {
	my ($lei, $uri) = @_;
	$uri->host =~ /\.onion\z/i or return "\n";
	my $t = $uri->isa('PublicInbox::URIimap') ? 'imap' : 'nntp';
	my $url = PublicInbox::Config::squote_maybe(uri_section($uri));
	my $scheme = $uri->scheme;
	my $set_cfg = 'lei config';
	if (!$lei) { # public-inbox-watch
		my $f = PublicInbox::Config::squote_maybe(
				$ENV{PI_CONFIG} || '~/.public-inbox/config');
		$set_cfg = "git config -f $f";
	}
	my $dq = substr($url, 0, 1) eq "'" ? '"' : '';
	<<EOM

Assuming you have Tor configured and running locally on port 9050,
try configuring a socks5h:// proxy:

	url=$url
	$set_cfg $t.$dq\$url$dq.proxy socks5h://127.0.0.1:9050

git 2.26+ users may instead rely on `*' to match all .onion URLs:

	$set_cfg '$t.$scheme://*.onion.proxy' socks5h://127.0.0.1:9050

...before retrying your current command
EOM
}

# Net::NNTP doesn't support CAPABILITIES, yet; and both IMAP+NNTP
# servers may have multiple listen sockets.
sub try_starttls ($) {
	my ($host) = @_;
	return if $host =~ /\.onion\z/si;
	return if $host =~ /\A127\.[0-9]+\.[0-9]+\.[0-9]+\z/s;
	return if $host eq '::1';
	1;
}

# mic_for may prompt the user and store auth info, prepares mic_get
sub mic_for ($$$$) { # mic = Mail::IMAPClient
	my ($self, $uri, $mic_common, $lei) = @_;
	require PublicInbox::GitCredential;
	my $cred = bless {
		url => "$uri",
		protocol => $uri->scheme,
		host => $uri->host,
		username => $uri->user,
		password => $uri->password,
	}, 'PublicInbox::GitCredential';
	my $sec = uri_section($uri);
	my $common = $mic_common->{$sec} // {};
	# IMAPClient and Net::Netrc both mishandles `0', so we pass `127.0.0.1'
	my $host = $cred->{host};
	$host = '127.0.0.1' if $host eq '0';
	my $mic_arg = {
		Port => $uri->port,
		Server => $host,
		%$common, # may set Starttls, Compress, Debug ....
	};
	require PublicInbox::IMAPClient;
	my $mic = mic_new($self, $mic_arg, $sec, $uri);
	return if $self->{quit};
	($mic && $mic->IsConnected) or
		die "E: <$uri> new: $@".onion_hint($lei, $uri);

	# default to using STARTTLS if it's available, but allow
	# it to be disabled since I usually connect to localhost
	if (!$mic_arg->{Ssl} && !defined($mic_arg->{Starttls}) &&
			$mic->has_capability('STARTTLS') &&
			try_starttls($host) &&
			$mic->can('starttls')) {
		$mic->starttls or die "E: <$uri> STARTTLS: $@\n";
	}

	# do we even need credentials?
	if (!defined($cred->{username}) &&
			$mic->has_capability('AUTH=ANONYMOUS')) {
		$cred = undef;
	}
	if ($cred) {
		my $p = $cred->{password} // $cred->check_netrc($lei);
		$cred->fill($lei) unless defined($p); # may prompt user here
		$mic->User($mic_arg->{User} = $cred->{username});
		$mic->Password($mic_arg->{Password} = $cred->{password});
	} else { # AUTH=ANONYMOUS
		$mic->Authmechanism($mic_arg->{Authmechanism} = 'ANONYMOUS');
		$mic_arg->{Authcallback} = 'auth_anon_cb';
		$mic->Authcallback(\&auth_anon_cb);
	}
	my $err;
	if ($mic->login && $mic->IsAuthenticated) {
		# success! keep IMAPClient->new arg in case we get disconnected
		$self->{net_arg}->{$sec} = $mic_arg;
		if ($cred) {
			$uri->user($cred->{username}) if !defined($uri->user);
		} elsif ($mic_arg->{Authmechanism} eq 'ANONYMOUS') {
			$uri->auth('ANONYMOUS') if !defined($uri->auth);
		}
	} else {
		$err = "E: <$uri> LOGIN: $@\n";
		if ($cred && defined($cred->{password})) {
			$err =~ s/\Q$cred->{password}\E/*******/g;
		}
		$mic = undef;
	}
	$cred->run($mic ? 'approve' : 'reject') if $cred && $cred->{filled};
	if ($err) {
		$lei ? $lei->fail($err) : warn($err);
	}
	$mic;
}

sub nn_new ($$$$) {
	my ($self, $nn_arg, $nntp_cfg, $uri) = @_;
	my $nn;
	my ($Net_NNTP, $new) = qw(Net::NNTP new);
	if (defined $nn_arg->{ProxyAddr}) {
		require PublicInbox::NetNNTPSocks;
		($Net_NNTP, $new) = qw(PublicInbox::NetNNTPSocks new_socks);
		$nn_arg->{SocksDebug} = 1 if $nn_arg->{Debug};
	}
	do {
		$! = 0;
		$nn = $Net_NNTP->$new(%$nn_arg);
	} until ($nn || !$!{EINTR} || $self->{quit});
	$nn // return;
	setsockopt($nn, Socket::SOL_SOCKET(), Socket::SO_KEEPALIVE(), 1);

	# default to using STARTTLS if it's available, but allow
	# it to be disabled for localhost/VPN users
	if (!$nn_arg->{SSL} && $nn->can('starttls')) {
		if (!defined($nntp_cfg->{starttls}) &&
				try_starttls($nn_arg->{Host})) {
			# soft fail by default
			$nn->starttls or warn <<"";
W: <$uri> STARTTLS tried and failed (not requested)

		} elsif ($nntp_cfg->{starttls}) {
			# hard fail if explicitly configured
			$nn->starttls or die <<"";
E: <$uri> STARTTLS requested and failed

		}
	} elsif ($nntp_cfg->{starttls}) {
		$nn->can('starttls') or
			die "E: <$uri> Net::NNTP too old for STARTTLS\n";
		$nn->starttls or die <<"";
E: <$uri> STARTTLS requested and failed

	}
	$nn;
}

sub nn_for ($$$$) { # nn = Net::NNTP
	my ($self, $uri, $nn_common, $lei) = @_;
	my $sec = uri_section($uri);
	my $nntp_cfg = $self->{cfg_opt}->{$sec} //= {};
	my $host = $uri->host;
	# Net::NNTP and Net::Netrc both mishandle `0', so we pass `127.0.0.1'
	$host = '127.0.0.1' if $host eq '0';
	my $cred;
	my ($u, $p);
	if (defined(my $ui = $uri->userinfo)) {
		require PublicInbox::GitCredential;
		$cred = bless {
			url => $sec,
			protocol => $uri->scheme,
			host => $host,
		}, 'PublicInbox::GitCredential';
		($u, $p) = split(/:/, $ui, 2);
		($cred->{username}, $cred->{password}) = ($u, $p);
		$p //= $cred->check_netrc($lei);
	}
	my $common = $nn_common->{$sec} // {};
	my $nn_arg = {
		Port => $uri->port,
		Host => $host,
		%$common, # may Debug ....
	};
	$nn_arg->{SSL} = 1 if $uri->secure; # snews == nntps
	my $sa = $self->{-proxy_cli};
	%$nn_arg = (%$nn_arg, %$sa) if $sa;
	my $nn = nn_new($self, $nn_arg, $nntp_cfg, $uri);
	return if $self->{quit};
	$nn // die "E: <$uri> new: $@".onion_hint($lei, $uri);
	if ($cred) {
		$cred->fill($lei) unless defined($p); # may prompt user here
		if ($nn->authinfo($u, $p)) {
			push @{$nntp_cfg->{-postconn}}, [ 'authinfo', $u, $p ];
		} else {
			warn "E: <$uri> AUTHINFO $u XXXX failed\n";
			$nn = undef;
		}
	}

	if ($nntp_cfg->{compress}) {
		# https://rt.cpan.org/Ticket/Display.html?id=129967
		if ($nn->can('compress')) {
			if ($nn->compress) {
				push @{$nntp_cfg->{-postconn}}, [ 'compress' ];
			} else {
				warn "W: <$uri> COMPRESS failed\n";
			}
		} else {
			delete $nntp_cfg->{compress};
			warn <<"";
W: <$uri> COMPRESS not supported by Net::NNTP
W: see https://rt.cpan.org/Ticket/Display.html?id=129967 for updates

		}
	}

	$self->{net_arg}->{$sec} = $nn_arg;
	$cred->run($nn ? 'approve' : 'reject') if $cred && $cred->{filled};
	$nn;
}

sub imap_uri {
	my ($url, $ls_ok) = @_;
	require PublicInbox::URIimap;
	my $uri = PublicInbox::URIimap->new($url);
	$uri && ($ls_ok || $uri->mailbox) ? $uri->canonical : undef;
}

my %IS_NNTP = (news => 1, snews => 1, nntp => 1, nntps => 1);
sub nntp_uri {
	my ($url, $ls_ok) = @_;
	require PublicInbox::URInntps;
	my $uri = PublicInbox::URInntps->new($url);
	$uri && $IS_NNTP{$uri->scheme} && ($ls_ok || $uri->group) ?
		$uri->canonical : undef;
}

sub cfg_intvl ($$$) {
	my ($cfg, $key, $url) = @_;
	my $v = $cfg->urlmatch($key, $url) // return;
	$v =~ /\A[0-9]+(?:\.[0-9]+)?\z/s and return $v + 0;
	if (ref($v) eq 'ARRAY') {
		$v = join(', ', @$v);
		warn "W: $key has multiple values: $v\nW: $key ignored\n";
	} else {
		warn "W: $key=$v is not a numeric value in seconds\n";
	}
}

# flesh out common IMAP-specific data structures
sub imap_common_init ($;$) {
	my ($self, $lei) = @_;
	return unless $self->{imap_order};
	$self->{quiet} = 1 if $lei && $lei->{opt}->{quiet};
	eval { require PublicInbox::IMAPClient } or
		die "Mail::IMAPClient is required for IMAP:\n$@\n";
	($lei || eval { require PublicInbox::IMAPTracker }) or
		die "DBD::SQLite is required for IMAP\n:$@\n";
	require PublicInbox::URIimap;
	my $cfg = $self->{pi_cfg} // $lei->_lei_cfg;
	my $mic_common = {}; # scheme://authority => Mail:IMAPClient arg
	for my $uri (@{$self->{imap_order}}) {
		my $sec = uri_section($uri);

		# knobs directly for Mail::IMAPClient->new
		for my $k (qw(Starttls Debug Compress)) {
			my $v = $cfg->urlmatch('--bool', "imap.$k", $$uri);
			$mic_common->{$sec}->{$k} = $v if defined $v;
		}
		my $to = cfg_intvl($cfg, 'imap.timeout', $$uri);
		$mic_common->{$sec}->{Timeout} = $to if $to;
		$mic_common->{$sec}->{Ssl} = 1 if $uri->scheme eq 'imaps';

		# knobs we use ourselves:
		my $sa = socks_args($cfg->urlmatch('imap.Proxy', $$uri));
		$self->{cfg_opt}->{$sec}->{-proxy_cfg} = $sa if $sa;
		for my $k (qw(pollInterval idleInterval)) {
			$to = cfg_intvl($cfg, "imap.$k", $$uri) // next;
			$self->{cfg_opt}->{$sec}->{$k} = $to;
		}
		my $k = 'imap.fetchBatchSize';
		if (defined(my $bs = $cfg->urlmatch($k, $$uri))) {
			($bs =~ /\A([0-9]+)\z/ && $bs > 0) ?
				($self->{cfg_opt}->{$sec}->{batch_size} = $bs) :
				warn("$k=$bs is not a positive integer\n");
		}
		my $v = $cfg->urlmatch(qw(--bool imap.sslVerify), $$uri);
		if (defined $v) {
			my $cur = $mic_common->{$sec} //= {};
			$cur->{Starttls} //= 1 if !$cur->{Ssl};
			for my $f (grep { $cur->{$_} } qw(Ssl Starttls)) {
				set_ssl_verify_mode($cur->{$f} = {}, $v);
			}
		}
	}
	# make sure we can connect and cache the credentials in memory
	my $mics = {}; # schema://authority => IMAPClient obj
	for my $orig_uri (@{$self->{imap_order}}) {
		my $sec = uri_section($orig_uri);
		my $uri = PublicInbox::URIimap->new("$sec/");
		my $mic = $mics->{$sec} //=
				mic_for($self, $uri, $mic_common, $lei);
		return if $self->{quit};
		$mic // die "Unable to continue\n";
		next unless $self->isa('PublicInbox::NetWriter');
		next if $self->{-skip_creat};
		my $dst = $orig_uri->mailbox // next;
		next if $mic->exists($dst); # already exists
		$mic->create($dst) or die "CREATE $dst failed <$orig_uri>: $@";
	}
	$mics;
}

# flesh out common NNTP-specific data structures
sub nntp_common_init ($;$) {
	my ($self, $lei) = @_;
	return unless $self->{nntp_order};
	$self->{quiet} = 1 if $lei && $lei->{opt}->{quiet};
	eval { require Net::NNTP } or
		die "Net::NNTP is required for NNTP:\n$@\n";
	($lei || eval { require PublicInbox::IMAPTracker }) or
		die "DBD::SQLite is required for NNTP\n:$@\n";
	my $cfg = $self->{pi_cfg} // $lei->_lei_cfg;
	my $nn_common = {}; # scheme://authority => Net::NNTP->new arg
	for my $uri (@{$self->{nntp_order}}) {
		my $sec = uri_section($uri);
		my $args = $nn_common->{$sec} //= {};

		# Debug and Timeout are passed to Net::NNTP->new
		my $v = $cfg->urlmatch(qw(--bool nntp.Debug), $$uri);
		$args->{Debug} = $v if defined $v;
		my $to = cfg_intvl($cfg, 'nntp.Timeout', $$uri);
		$args->{Timeout} = $to if $to;
		my $sa = socks_args($cfg->urlmatch('nntp.Proxy', $$uri));
		%$args = (%$args, %$sa) if $sa;

		# Net::NNTP post-connect commands
		for my $k (qw(starttls compress)) {
			$v = $cfg->urlmatch('--bool', "nntp.$k", $$uri);
			$self->{cfg_opt}->{$sec}->{$k} = $v if defined $v;
		}
		$v = $cfg->urlmatch(qw(--bool nntp.sslVerify), $$uri);
		set_ssl_verify_mode($args, $v) if defined $v;

		# -watch internal option
		for my $k (qw(pollInterval)) {
			$to = cfg_intvl($cfg, "nntp.$k", $$uri) // next;
			$self->{cfg_opt}->{$sec}->{$k} = $to;
		}
	}
	# make sure we can connect and cache the credentials in memory
	my %nn; # schema://authority => Net::NNTP object
	for my $uri (@{$self->{nntp_order}}) {
		my $sec = uri_section($uri);
		$nn{$sec} //= nn_for($self, $uri, $nn_common, $lei);
	}
	\%nn; # for optional {nn_cached}
}

sub add_url {
	my ($self, $arg, $ls_ok) = @_;
	my $uri;
	if ($uri = imap_uri($arg, $ls_ok)) {
		$_[1] = $$uri; # canonicalized
		push @{$self->{imap_order}}, $uri;
	} elsif ($uri = nntp_uri($arg, $ls_ok)) {
		$_[1] = $$uri; # canonicalized
		push @{$self->{nntp_order}}, $uri;
	} else {
		push @{$self->{unsupported_url}}, $arg;
	}
}

sub errors {
	my ($self, $lei) = @_;
	if (my $u = $self->{unsupported_url}) {
		return "Unsupported URL(s): @$u";
	}
	if ($self->{imap_order}) {
		eval { require PublicInbox::IMAPClient } or
			die "Mail::IMAPClient is required for IMAP:\n$@\n";
	}
	if ($self->{nntp_order}) {
		eval { require Net::NNTP } or
			die "Net::NNTP is required for NNTP:\n$@\n";
	}
	my $sa = socks_args($lei ? $lei->{opt}->{proxy} : undef);
	$self->{-proxy_cli} = $sa if $sa;
	undef;
}

sub flags2kw ($$$$) {
	my ($self, $uri, $uid, $flags) = @_;
	my $kw = [];
	for my $f (split(/ /, $flags)) {
		if (my $k = $IMAPflags2kw{$f}) {
			push @$kw, $k;
		} elsif ($f eq "\\Recent") { # not in JMAP
		} elsif ($f eq "\\Deleted") { # not in JMAP
			return;
		} elsif ($self->{verbose}) {
			warn "# unknown IMAP flag $f <$uri/;UID=$uid>\n";
		}
	}
	@$kw = sort @$kw; # for LeiSearch->kw_changed and UI/UX purposes
	$kw;
}

sub _imap_do_msg ($$$$$) {
	my ($self, $uri, $uid, $raw, $flags) = @_;
	# our target audience expects LF-only, save storage
	$$raw =~ s/\r\n/\n/sg;
	my $kw = defined($flags) ?
		(flags2kw($self, $uri, $uid, $flags) // return) : undef;
	my ($eml_cb, @args) = @{$self->{eml_each}};
	$eml_cb->($uri, $uid, $kw, PublicInbox::Eml->new($raw), @args);
}

sub run_commit_cb ($) {
	my ($self) = @_;
	my $cmt_cb_args = $self->{on_commit} or return;
	my ($cb, @args) = @$cmt_cb_args;
	$cb->(@args);
}

sub itrk_last ($$;$$) {
	my ($self, $uri, $r_uidval, $mic) = @_;
	return (undef, undef, $r_uidval) unless $self->{incremental};
	my ($itrk, $l_uid, $l_uidval);
	if (defined(my $lms = $self->{-lms_rw})) { # LeiMailSync or 0
		$uri->uidvalidity($r_uidval) if defined $r_uidval;
		if ($mic) {
			my $auth = $mic->Authmechanism // '';
			$uri->auth($auth) if $auth eq 'ANONYMOUS';
			my $user = $mic->User;
			$uri->user($user) if defined($user);
		}
		my $x;
		$l_uid = ($lms && ($x = $lms->location_stats($$uri))) ?
				$x->{'uid.max'} : undef;
		# itrk remains undef, lei/store worker writes to
		# mail_sync.sqlite3
	} else {
		$itrk = PublicInbox::IMAPTracker->new($$uri);
		($l_uidval, $l_uid) = $itrk->get_last($$uri);
	}
	($itrk, $l_uid, $l_uidval //= $r_uidval);
}

# import flags of already-seen messages
sub each_old_flags ($$$$) {
	my ($self, $mic, $uri, $l_uid) = @_;
	$l_uid ||= 1;
	my $sec = uri_section($uri);
	my $bs = ($self->{cfg_opt}->{$sec}->{batch_size} // 1) * 10000;
	my ($eml_cb, @args) = @{$self->{eml_each}};
	$self->{quiet} or warn "# $uri syncing flags 1:$l_uid\n";
	for (my $n = 1; $n <= $l_uid; $n += $bs) {
		my $end = $n + $bs;
		$end = $l_uid if $end > $l_uid;
		my $r = $mic->fetch_hash("$n:$end", 'FLAGS');
		if (!$r) {
			return if $!{EINTR} && $self->{quit};
			return "E: $uri UID FETCH $n:$end error: $!";
		}
		while (my ($uid, $per_uid) = each %$r) {
			my $kw = flags2kw($self, $uri, $uid, $per_uid->{FLAGS})
				// next;
			# LeiImport->input_net_cb
			$eml_cb->($uri, $uid, $kw, undef, @args);
		}
	}
}

# returns true if PERMANENTFLAGS indicates FLAGS of already imported
# messages are meaningful
sub perm_fl_ok ($) {
	my ($perm_fl) = @_;
	return if !defined($perm_fl);
	for my $f (split(/[ \t]+/, $perm_fl)) {
		return 1 if $IMAPflags2kw{$f};
	}
	undef;
}

# may be overridden in NetWriter or Watch
sub folder_select { $_[0]->{each_old} ? 'select' : 'examine' }

sub _imap_fetch_bodies ($$$$) {
	my ($self, $mic, $uri, $uids) = @_;
	my $req = $mic->imap4rev1 ? 'BODY.PEEK[]' : 'RFC822.PEEK';
	my $key = $req;
	$key =~ s/\.PEEK//;
	my $sec = uri_section($uri);
	my $mbx = $uri->mailbox;
	my $bs = $self->{cfg_opt}->{$sec}->{batch_size} // 1;
	my ($last_uid, $err);
	my $use_fl = $self->{-use_fl};

	while (scalar @$uids) {
		my @batch = splice(@$uids, 0, $bs);
		my $batch = join(',', @batch);
		local $0 = "UID:$batch $mbx $sec";
		my $r = $mic->fetch_hash($batch, $req, 'FLAGS');
		unless ($r) { # network error?
			last if $!{EINTR} && $self->{quit};
			$err = "E: $uri UID FETCH $batch error: $!";
			last;
		}
		for my $uid (@batch) {
			# messages get deleted, so holes appear
			my $per_uid = delete $r->{$uid} // next;
			my $raw = delete($per_uid->{$key}) // next;
			my $fl = $use_fl ? $per_uid->{FLAGS} : undef;
			_imap_do_msg($self, $uri, $uid, \$raw, $fl);
			$last_uid = $uid;
			last if $self->{quit};
		}
		last if $self->{quit};
	}
	($last_uid, $err);
}

sub _imap_fetch_all ($$$) {
	my ($self, $mic, $orig_uri) = @_;
	my $sec = uri_section($orig_uri);
	my $mbx = $orig_uri->mailbox;
	$mic->Clear(1); # trim results history

	# we need to check for mailbox writability to see if we care about
	# FLAGS from already-imported messages.
	my $cmd = $self->folder_select;
	$mic->$cmd($mbx) or return "E: \U$cmd\E $mbx ($sec) failed: $!";

	my ($r_uidval, $r_uidnext, $perm_fl);
	for ($mic->Results) {
		/^\* OK \[PERMANENTFLAGS \(([^\)]*)\)\].*/ and $perm_fl = $1;
		/^\* OK \[UIDVALIDITY ([0-9]+)\].*/ and $r_uidval = $1;
		/^\* OK \[UIDNEXT ([0-9]+)\].*/ and $r_uidnext = $1;
	}
	$r_uidval //= $mic->uidvalidity($mbx) //
		return "E: $orig_uri cannot get UIDVALIDITY";
	$r_uidnext //= $mic->uidnext($mbx) //
		return "E: $orig_uri cannot get UIDNEXT";
	my $expect = $orig_uri->uidvalidity // $r_uidval;
	return <<EOF if $expect != $r_uidval;
E: $orig_uri UIDVALIDITY mismatch (got $r_uidval)
EOF

	my $uri = $orig_uri->clone;
	my $single_uid = $uri->uid;
	my ($itrk, $l_uid, $l_uidval) = itrk_last($self, $uri, $r_uidval, $mic);
	if (defined($single_uid)) {
		$itrk = $l_uid = undef;
		$uri->uid(undef); # for eml_cb
	}
	return <<EOF if $l_uidval != $r_uidval;
E: $uri UIDVALIDITY mismatch
E: local=$l_uidval != remote=$r_uidval
EOF
	$uri->uidvalidity($r_uidval);
	$l_uid //= 0;
	my $r_uid = $r_uidnext - 1;
	return <<EOF if $l_uid > $r_uid;
E: $uri local UID exceeds remote ($l_uid > $r_uid)
E: $uri strangely, UIDVALIDLITY matches ($l_uidval)
EOF
	$mic->Uid(1); # the default, we hope
	my $err;
	my $use_fl = perm_fl_ok($perm_fl);
	local $self->{-use_fl} = $use_fl;
	if (!defined($single_uid) && $self->{each_old} && $use_fl) {
		$err = each_old_flags($self, $mic, $uri, $l_uid);
		return $err if $err;
	}
	return if $l_uid >= $r_uid; # nothing to do
	$l_uid ||= 1;
	my ($mod, $shard) = @{$self->{shard_info} // []};
	unless ($self->{quiet}) {
		my $m = $mod ? " [(UID % $mod) == $shard]" : '';
		warn "# $uri fetching UID $l_uid:$r_uid$m\n";
	}
	my $fetch_cb = \&_imap_fetch_bodies;
	do {
		# I wish "UID FETCH $START:*" could work, but:
		# 1) servers do not need to return results in any order
		# 2) Mail::IMAPClient doesn't offer a streaming API
		my $uids;
		if (defined $single_uid) {
			$uids = [ $single_uid ];
		} elsif (!($uids = $mic->search("UID $l_uid:*"))) {
			return if $!{EINTR} && $self->{quit};
			return "E: $uri UID SEARCH $l_uid:* error: $!";
		}
		return if scalar(@$uids) == 0;

		# RFC 3501 doesn't seem to indicate order of UID SEARCH
		# responses, so sort it ourselves.  Order matters so
		# IMAPTracker can store the newest UID.
		@$uids = sort { $a <=> $b } @$uids;

		# Did we actually get new messages?
		return if $uids->[0] < $l_uid;

		$l_uid = $uids->[-1] + 1; # for next search
		@$uids = grep { ($_ % $mod) == $shard } @$uids if $mod;
		(my $last_uid, $err) = $fetch_cb->($self, $mic, $uri, $uids);
		run_commit_cb($self);
		$itrk->update_last($r_uidval, $last_uid) if $itrk;
	} until ($err || $self->{quit} || defined($single_uid));
	$err;
}

# uses cached auth info prepared by mic_for
sub mic_get {
	my ($self, $uri) = @_;
	my $sec = uri_section($uri);
	# see if caller saved result of imap_common_init
	my $cached = $self->{mics_cached};
	if ($cached) {
		my $mic = $cached->{$sec};
		return $mic if $mic && $mic->IsConnected;
		delete $cached->{$sec};
	}
	my $mic_arg = $self->{net_arg}->{$sec} or
			die "BUG: no Mail::IMAPClient->new arg for $sec";
	if (defined(my $cb_name = $mic_arg->{Authcallback})) {
		if (ref($cb_name) ne 'CODE') {
			$mic_arg->{Authcallback} = $self->can($cb_name);
		}
	}
	my $mic = mic_new($self, $mic_arg, $sec, $uri);
	$cached //= {}; # invalid placeholder if no cache enabled
	if ($mic && $mic->IsConnected) {
		$cached->{$sec} = $mic;
	} else {
		warn 'IMAP LastError: ',$mic->LastError, "\n" if $mic;
		warn "IMAP errno: $!\n" if $!;
		undef;
	}
}

sub imap_each {
	my ($self, $url, $eml_cb, @args) = @_;
	my $uri = ref($url) ? $url : PublicInbox::URIimap->new($url);
	my $sec = uri_section($uri);
	local $0 = $uri->mailbox." $sec";
	my $mic = mic_get($self, $uri);
	my $err;
	if ($mic) {
		local $self->{eml_each} = [ $eml_cb, @args ];
		$err = _imap_fetch_all($self, $mic, $uri);
	} else {
		$err = "E: <$uri> not connected: $!";
	}
	die $err if $err && $self->{-can_die};
	warn $err if $err;
	$mic;
}

# may used cached auth info prepared by nn_for once
sub nn_get {
	my ($self, $uri) = @_;
	my $sec = uri_section($uri);
	# see if caller saved result of nntp_common_init
	my $cached = $self->{nn_cached} // {};
	my $nn;
	$nn = delete($cached->{$sec}) and return $nn;
	my $nn_arg = $self->{net_arg}->{$sec} or
			die "BUG: no Net::NNTP->new arg for $sec";
	my $nntp_cfg = $self->{cfg_opt}->{$sec};
	$nn = nn_new($self, $nn_arg, $nntp_cfg, $uri) or return;
	if (my $postconn = $nntp_cfg->{-postconn}) {
		for my $m_arg (@$postconn) {
			my ($method, @args) = @$m_arg;
			$nn->$method(@args) and next;
			die "E: <$uri> $method failed\n";
			return;
		}
	}
	$nn;
}

sub _nntp_fetch_all ($$$) {
	my ($self, $nn, $uri) = @_;
	my ($group, $num_a, $num_b) = $uri->group;
	my $sec = uri_section($uri);
	my ($nr, $beg, $end) = $nn->group($group);
	unless (defined($nr)) {
		my $msg = ndump($nn->message);
		return "E: GROUP $group <$sec> $msg";
	}
	(defined($num_a) && defined($num_b) && $num_a > $num_b) and
		return "E: $uri: backwards range: $num_a > $num_b";
	if (defined($num_a)) { # no article numbers in mail_sync.sqlite3
		$uri = $uri->clone;
		$uri->group($group);
	}
	# IMAPTracker is also used for tracking NNTP, UID == article number
	# LIST.ACTIVE can get the equivalent of UIDVALIDITY, but that's
	# expensive.  So we assume newsgroups don't change:
	my ($itrk, $l_art) = itrk_last($self, $uri);

	if (defined($l_art) && !defined($num_a)) {
		return if $l_art >= $end; # nothing to do
		$beg = $l_art + 1;
	}
	# allow users to specify articles to refetch
	# cf. https://tools.ietf.org/id/draft-gilman-news-url-01.txt
	# nntp://example.com/inbox.foo/$num_a-$num_b
	$beg = $num_a if defined($num_a) && $num_a > $beg && $num_a <= $end;
	$end = $num_b if defined($num_b) && $num_b >= $beg && $num_b < $end;
	$end = $beg if defined($num_a) && !defined($num_b);
	my ($err, $last_art, $kw); # kw stays undef, no keywords in NNTP
	warn "# $uri fetching ARTICLE $beg..$end\n" if !$self->{quiet};
	my $n = $self->{max_batch};
	for my $art ($beg..$end) {
		last if $self->{quit};
		local $0 = "#$art $group $sec";
		if (--$n < 0) {
			run_commit_cb($self);
			$itrk->update_last(0, $last_art) if $itrk;
			$n = $self->{max_batch};
		}
		my $raw = $nn->article($art);
		unless (defined($raw)) {
			my $msg = ndump($nn->message);
			if ($nn->code == 421) { # pseudo response from Net::Cmd
				$err = "E: $msg";
				last;
			} else { # probably just a deleted message (spam)
				warn "W: $msg";
				next;
			}
		}
		$raw = join('', @$raw);
		$raw =~ s/\r\n/\n/sg;
		my ($eml_cb, @args) = @{$self->{eml_each}};
		$eml_cb->($uri, $art, $kw, PublicInbox::Eml->new(\$raw), @args);
		$last_art = $art;
	}
	run_commit_cb($self);
	$itrk->update_last(0, $last_art) if $itrk;
	$err;
}

sub nntp_each {
	my ($self, $url, $eml_cb, @args) = @_;
	my $uri = ref($url) ? $url : PublicInbox::URInntps->new($url);
	my $sec = uri_section($uri);
	local $0 = $uri->group ." $sec";
	my $nn = nn_get($self, $uri);
	return if $self->{quit};
	my $err;
	if ($nn) {
		local $self->{eml_each} = [ $eml_cb, @args ];
		$err = _nntp_fetch_all($self, $nn, $uri);
	} else {
		$err = "E: <$uri> not connected: $!";
	}
	die $err if $err && $self->{-can_die};
	warn $err if $err;
	$nn;
}

sub new { bless {}, shift };

# updates $uri with UIDVALIDITY
sub mic_for_folder {
	my ($self, $uri) = @_;
	my $mic = $self->mic_get($uri) or die "E: not connected: $@";
	my $m = $self->isa('PublicInbox::NetWriter') ? 'select' : 'examine';
	$mic->$m($uri->mailbox) or return;
	my $uidval;
	for ($mic->Results) {
		/^\* OK \[UIDVALIDITY ([0-9]+)\].*/ or next;
		$uidval = $1;
		last;
	}
	$uidval //= $mic->uidvalidity($uri->mailbox) or
		die "E: failed to get uidvalidity from <$uri>: $@";
	$uri->uidvalidity($uidval);
	$mic;
}


1;
