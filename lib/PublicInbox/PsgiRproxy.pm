# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# HTTP reverse proxy for HTTP(S) termination to varnish, not
# tied to PublicInbox::WWW but uses public-inbox-{httpd,netd} features
package PublicInbox::PsgiRproxy;
use v5.12;
use Socket ();
use Errno qw(EAGAIN EINPROGRESS);
use parent qw(Plack::Component); # for ->to_app
require PublicInbox::H1ReqRes;
use constant NB_STREAM => do {
	Socket::SOCK_STREAM | (eval { Socket::SOCK_NONBLOCK() } // 0);
};

sub new {
	my ($cls, $dest, %opt) = @_;
	my $response_headers = delete $opt{response_headers};
	$response_headers->{server} //= undef; # don't lie, don't advertise
	my ($res_hdr_excl, $res_hdr_add);
	while (my ($k, $v) = each %$response_headers) {
		if (defined $v) {
			push @$res_hdr_add, [ $k, $v ];
		} else {
			push @$res_hdr_excl, $k;
		}
	}
	$opt{proxy_buffering} = 1 unless
		(exists $opt{proxy_buffering} && !$opt{proxy_buffering});
	my $self = bless { %opt }, $cls;
	if ($res_hdr_excl) {
		$res_hdr_excl = join '|', map quotemeta, @$res_hdr_excl;
		$self->{res_hdr_excl} = qr/\A(?:$res_hdr_excl)\z/i;
	}
	$self->{res_hdr_add} = $res_hdr_add if $res_hdr_add;
	my $path;
	if ($dest =~ m{\Aunix:([^:]+)(?::(/.*))?\z}) {
		$path = $2;
		# XXX pack_sockaddr_un can see $1 as undef for some reason,
		# so we copy it:
		my $addr = $1;
		$addr = Socket::pack_sockaddr_un($addr);
		@{$self->{conninfo}} = (Socket::AF_UNIX, $addr, $dest);
	} elsif ($dest =~ m{\Ahttp://(?:\[([a-f0-9:]+)\]|([0-9\.]+))
				(?::([0-9]+))?(/.*)?\z}xi) {
		$path = $4;
		my ($host, $port) = ($1 // $2, $3 // 8080);

		# don't support DNS lookup for now, otherwise we'll need to
		# add a timer to track DNS changes or expect users to reload
		my ($err, @addr) = Socket::getaddrinfo($host, $port, {
			hints => Socket::AI_NUMERICHOST,
			socktype => Socket::SOCK_STREAM,
			protocol => Socket::IPPROTO_TCP
		});
		die "E: cannot resolve `$dest': $err\n" if $err || !@addr;
		@{$self->{conninfo}} = map {
			(@$_{qw(family addr)}, $dest)
		} @addr;
	} else {
		die <<EOM;
E: `$dest' must be an HTTP IP address URL or unix: path
EOM
	}
	$path //= '$fullpath'; # PublicInbox::H1ReqRes::fullpath
	my @vars = ($path =~ m/\$(\w+)/g);
	my @bad = grep !/\A(?:fullpath|host)\z/, @vars; # TODO: more vars
	die "E: bad vars in `$path': @bad\n" if @bad;
	# no leading slash iff using a path
	$path =~ s!\A/(\$(?:fullpath|path))!!;
	$self->{path} = $path;
	$self;
}

# ->to_app makes a sub which calls this
sub call {
	my ($self, $env) = @_;
	my @try = @{$self->{conninfo}};
	my ($sock, $family, $addr, $dest);
	while (@try) {
		($family, $addr, $dest) = splice @try, 0, 3;
		if (socket $sock, $family, NB_STREAM, 0) {
			$sock->blocking(0) if NB_STREAM == Socket::SOCK_STREAM;
			last if connect($sock, $addr) ||
				$! == EINPROGRESS || $! == EAGAIN;
			warn "E: connect($dest): $!";
		} else {
			warn "E: socket: $! ($dest)";
		}
		undef $sock;
	}
	$sock ? sub {
		my ($wcb) = @_; # PSGI server gives us this
		PublicInbox::H1ReqRes->new($sock, $env, $self, $wcb);
	} : [ 502, [], [] ];
}

1;
