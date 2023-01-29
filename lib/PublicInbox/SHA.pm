# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# OpenSSL exception added in commit 22711f81f4e79da6b796820e37803a05cae14645
# (README: add OpenSSL exception, 2015-10-05)

# Replaces most uses of Digest::SHA with OpenSSL via Net::SSLeay if
# possible.  OpenSSL SHA-256 is nearly twice as fast as Digest::SHA on
# x86-64, and SHA-1 is a bit faster as well.
# I don't think we can implement Digest::SHA->clone with what Net::SSLeay
# gives us...  (maybe EVP_MD_CTX_copy+EVP_MD_CTX_copy_ex need to be added
# to Net::SSLeay?)
package PublicInbox::SHA;
use v5.12;
require Exporter;
our @EXPORT_OK = qw(sha1_hex sha256_hex sha256);
our @ISA;

BEGIN {
	push @ISA, 'Exporter';
	unless (eval(<<'EOM')) {
use Net::SSLeay 1.43;
my %SHA = (
	1 => Net::SSLeay::EVP_get_digestbyname('sha1'),
	256 => Net::SSLeay::EVP_get_digestbyname('sha256'),
);

sub new {
	my ($cls, $n) = @_;
	my $mdctx = Net::SSLeay::EVP_MD_CTX_create();
	Net::SSLeay::EVP_DigestInit($mdctx, $SHA{$n}) or
			die "EVP_DigestInit $n: $!";
	bless \$mdctx, $cls;
}

sub add {
	my $self = shift;
	Net::SSLeay::EVP_DigestUpdate($$self, $_) for @_;
	$self;
}

sub digest { Net::SSLeay::EVP_DigestFinal(${$_[0]}) };
sub hexdigest { unpack('H*', Net::SSLeay::EVP_DigestFinal(${$_[0]})) }
sub DESTROY { Net::SSLeay::EVP_MD_CTX_destroy(${$_[0]}) };

sub sha1_hex { unpack('H*', Net::SSLeay::SHA1($_[0])) };
sub sha256_hex { unpack('H*', Net::SSLeay::SHA256($_[0])) };
*sha256 = \&Net::SSLeay::SHA256;
# end of eval
EOM
	require Digest::SHA; # stdlib fallback
	push @ISA, 'Digest::SHA';
	*sha1_hex = \&Digest::SHA::sha1_hex;
	*sha256_hex = \&Digest::SHA::sha256_hex;
	*sha256 = \&Digest::SHA::sha256;
}

} # /BEGIN
1;
