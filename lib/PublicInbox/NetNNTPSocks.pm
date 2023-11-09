# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# wrap Net::NNTP client with SOCKS support.  Convoluted, but AFAIK this
# is the only way to get SOCKS working with Net::NNTP w/o LD_PRELOAD.
package PublicInbox::NetNNTPSocks;
use v5.12;
use Net::NNTP;
our %OPT; # used to pass options between ->new_socks and our ->new
our @ISA = qw(IO::Socket::Socks);

# use this instead of Net::NNTP->new if using Proxy*
sub new_socks {
	my (undef, %opt) = @_;
	require IO::Socket::Socks;
	local @Net::NNTP::ISA = (qw(Net::Cmd), __PACKAGE__);
	local %OPT = map {;
		defined($opt{$_}) ? ($_ => $opt{$_}) : ()
	} qw(ProxyAddr ProxyPort SocksVersion SocksDebug SocksResolve);
	no warnings 'uninitialized'; # needed for $SOCKS_ERROR
	my $ret = Net::NNTP->new(%opt); # calls PublicInbox::NetNNTPSocks::new
	return $ret if $ret || $!{EINTR};
	$ret // die "errors: \$!=$! SOCKS=",
				eval('$IO::Socket::Socks::SOCKS_ERROR // ""'),
				', SSL=',
				(eval('IO::Socket::SSL->errstr')  // ''), "\n";
}

# called by Net::NNTP->new
sub new {
	my ($self, %opt) = @_;
	@OPT{qw(ConnectAddr ConnectPort)} = @opt{qw(PeerAddr PeerPort)};
	$self->SUPER::new(%OPT);
}

1;
