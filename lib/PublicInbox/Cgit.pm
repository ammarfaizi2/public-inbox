# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# wrapper for cgit(1) and git-http-backend(1) for browsing and
# serving git code repositories.  Requires 'publicinbox.cgitrc'
# directive to be set in the public-inbox config file.

package PublicInbox::Cgit;
use v5.12;
use parent qw(PublicInbox::WwwCoderepo);
use PublicInbox::GitHTTPBackend;
use PublicInbox::Git;
# not bothering with Exporter for a one-off
*input_prepare = \&PublicInbox::GitHTTPBackend::input_prepare;
*serve = \&PublicInbox::GitHTTPBackend::serve;
use PublicInbox::Qspawn;
use PublicInbox::WwwStatic qw(r);

sub locate_cgit ($) {
	my ($pi_cfg) = @_;
	my $cgit_bin = $pi_cfg->{'publicinbox.cgitbin'};
	my $cgit_data = $pi_cfg->{'publicinbox.cgitdata'};

	# /var/www/htdocs/cgit is the default install path from cgit.git
	# /usr/{lib,share}/cgit is where Debian puts cgit
	# TODO: check other distros for common paths
	unless (defined $cgit_bin) {
		foreach (qw(/var/www/htdocs/cgit /usr/lib/cgit)) {
			my $x = "$_/cgit.cgi";
			next unless -x $x;
			$cgit_bin = $x;
			last;
		}
	}
	unless (defined $cgit_data) {
		my @dirs = qw(/var/www/htdocs/cgit /usr/share/cgit);

		# local installs of cgit from source have
		# CGIT_SCRIPT_PATH==CGIT_DATA_PATH by default,
		# so we can usually infer the cgit_data path from cgit_bin
		if (defined($cgit_bin) && $cgit_bin =~ m!\A(.+?)/[^/]+\z!) {
			unshift @dirs, $1 if -d $1;
		}
		for (@dirs) {
			next unless -f "$_/cgit.css";
			$cgit_data = $_;
			last;
		}
	}
	($cgit_bin, $cgit_data);
}

sub new {
	my ($class, $pi_cfg) = @_;
	my ($cgit_bin, $cgit_data) = locate_cgit($pi_cfg);
	$cgit_bin // return; # fall back in WWW->cgit
	my $self = bless {
		cmd => [ $cgit_bin ],
		cgit_data => $cgit_data,
		pi_cfg => $pi_cfg,
	}, $class;

	# some cgit repos may not be mapped to inboxes, so ensure those exist:
	PublicInbox::WwwCoderepo::prepare_coderepos($self);
	my $s = join('|', map { quotemeta } keys %{$pi_cfg->{-cgit_static}});
	$self->{static} = qr/\A($s)\z/;
	$self;
}

# only what cgit cares about:
my @PASS_ENV = qw(
	HTTP_HOST
	QUERY_STRING
	REQUEST_METHOD
	SCRIPT_NAME
	SERVER_NAME
	SERVER_PORT
	HTTP_COOKIE
	HTTP_REFERER
	CONTENT_LENGTH
);
# XXX: cgit filters may care about more variables...

my $parse_cgi_headers = \&PublicInbox::GitHTTPBackend::parse_cgi_headers;

sub call {
	my ($self, $env, $ctx) = @_; # $ctx is optional, used by WWW
	my $path_info = $env->{PATH_INFO};
	my $cgit_data;

	# handle requests without spawning cgit iff possible:
	if ($path_info =~ m!\A/(.+?)/($PublicInbox::GitHTTPBackend::ANY)\z!ox) {
		my ($nick, $path) = ($1, $2);
		if (my $git = $self->{pi_cfg}->{-code_repos}->{$nick}) {
			return serve($env, $git, $path);
		}
	} elsif ($path_info =~ m!$self->{static}! &&
		 defined($cgit_data = $self->{cgit_data})) {
		my $f = $cgit_data.$1; # {static} only matches leading slash
		return PublicInbox::WwwStatic::response($env, [], $f);
	}

	my $cgi_env = { PATH_INFO => $path_info };
	foreach (@PASS_ENV) {
		my $v = $env->{$_} // next;
		$cgi_env->{$_} = $v;
	}
	$cgi_env->{'HTTPS'} = 'on' if $env->{'psgi.url_scheme'} eq 'https';

	my $rdr = input_prepare($env) or return r(500);
	my $qsp = PublicInbox::Qspawn->new($self->{cmd}, $cgi_env, $rdr);
	my $limiter = $self->{pi_cfg}->limiter('-cgit');
	$qsp->psgi_return($env, $limiter, $parse_cgi_headers, $ctx);
}

1;
