# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# cf. RFC 5092, which the `URI' package doesn't support
#
# This depends only on the documented public API of the `URI' dist,
# not on internal `_'-prefixed subclasses such as `URI::_server'
#
# <https://metacpan.org/pod/URI::imap> exists, but it appears
# unmaintained, isn't in common distros, nor does it support
# ';FOO=BAR' parameters such as UIDVALIDITY
#
# RFC 2192 also describes ";TYPE=<list_type>"
package PublicInbox::URIimap;
use strict;
use v5.10.1;
use URI::Split qw(uri_split uri_join); # part of URI
use URI::Escape qw(uri_unescape uri_escape);
use overload '""' => \&as_string;

my %default_ports = (imap => 143, imaps => 993);
# for enc-auth-type and enc-user in RFC 5092
my $achar = qr/[A-Za-z0-9%\-_\.\!\$'\(\)\+\,\&\=\*]+/;

sub new {
	my ($class, $url) = @_;
	$url =~ m!\Aimaps?://! ? bless \$url, $class : undef;
}

sub canonical {
	my ($self) = @_;

	# no #frag in RFC 5092 from what I can tell
	my ($scheme, $auth, $path, $query, $_frag) = uri_split($$self);
	$path =~ s!\A/+!/!; # excessive leading slash

	# upper-case uidvalidity= and uid= parameter names
	$path =~ s/;([^=]+)=([^;]*)/;\U$1\E=$2/g;

	# lowercase the host portion
	$auth =~ s#\A(.*@)?(.*?)(?::([0-9]+))?\z#
		my $ret = ($1//'').lc($2);
		if (defined(my $port = $3)) {
			if ($default_ports{lc($scheme)} != $port) {
				$ret .= ":$port";
			}
		}
		$ret#ei;

	ref($self)->new(uri_join(lc($scheme), $auth, $path, $query));
}

sub host {
	my ($self) = @_;
	my (undef, $auth) = uri_split($$self);
	$auth =~ s!\A.*?@!!;
	$auth =~ s!:[0-9]+\z!!;
	$auth =~ s!\A\[(.*)\]\z!$1!; # IPv6
	uri_unescape($auth);
}

# unescaped, may be used for globbing
sub path {
	my ($self) = @_;
	my (undef, undef, $path) = uri_split($$self);
	$path =~ s!\A/+!!;
	$path =~ s!/?;.*\z!!; # [;UIDVALIDITY=nz-number]/;UID=nz-number
	$path eq '' ? undef : $path;
}

sub mailbox {
	my ($self) = @_;
	my $path = path($self);
	defined($path) ? uri_unescape($path) : undef;
}

sub uidvalidity { # read/write
	my ($self, $val) = @_;
	my ($scheme, $auth, $path, $query, $frag) = uri_split($$self);
	if (defined $val) {
		if ($path =~ s!;UIDVALIDITY=[^;/]*\b!;UIDVALIDITY=$val!i or
				$path =~ s!/;!;UIDVALIDITY=$val/;!i) {
			# s// already changed it
		} else { # both s// failed, so just append
			$path .= ";UIDVALIDITY=$val";
		}
		$$self = uri_join($scheme, $auth, $path, $query, $frag);
	}
	$path =~ s!\A/+!!;
	$path =~ m!\A[^;]+;UIDVALIDITY=([1-9][0-9]*)\b!i ?
		($1 + 0) : undef;
}

sub uid {
	my ($self, $val) = @_;
	my ($scheme, $auth, $path, $query, $frag) = uri_split($$self);
	if (scalar(@_) == 2) {
		if (!defined $val) {
			$path =~ s!/;UID=[^;/]*\b!!i;
		} else {
			$path =~ s!/;UID=[^;/]*\b!/;UID=$val!i or
				$path .= "/;UID=$val";
		}
		$$self = uri_join($scheme, $auth, $path, $query);
	}
	$path =~ m!\A/[^;]+(?:;UIDVALIDITY=[^;/]+)?/;UID=([1-9][0-9]*)\b!i ?
		($1 + 0) : undef;
}

sub port {
	my ($self) = @_;
	my ($scheme, $auth) = uri_split($$self);
	$auth =~ /:([0-9]+)\z/ ? $1 + 0 : $default_ports{lc($scheme)};
}

sub authority {
	my ($self) = @_;
	my (undef, $auth) = uri_split($$self);
	$auth
}

sub user {
	my ($self, $val) = @_;
	my ($scheme, $auth, $path, $query) = uri_split($$self);
	my $at_host_port;
	$auth =~ s/(@.*)\z// and $at_host_port = $1; # stash host:port for now
	if (scalar(@_) == 2) { # set, this clobbers password, too
		if (defined $val) {
			my $uval = uri_escape($val);
			if (defined($at_host_port)) {
				$auth =~ s!\A.*?(;AUTH=$achar).*!$uval$1!ix
					or $auth = $uval;
			} else {
				substr($auth, 0, 0) = "$uval@";
			}
		} elsif (defined($at_host_port)) { # clobber
			$auth =~ s!\A.*?(;AUTH=$achar).*!$1!i or $auth = '';
			if ($at_host_port && $auth eq '') {
				$at_host_port =~ s/\A\@//;
			}
		}
		$at_host_port //= '';
		$$self = uri_join($scheme, $auth.$at_host_port, $path, $query);
		$val;
	} else { # read-only
		$at_host_port // return undef; # explicit undef for scalar
		$auth =~ s/;.*\z//; # drop ;AUTH=...
		$auth =~ s/:.*\z//; # drop password
		$auth eq '' ? undef : uri_unescape($auth);
	}
}

sub password {
	my ($self) = @_;
	my (undef, $auth) = uri_split($$self);
	$auth =~ s/@.*\z// or return undef; # drop host:port
	$auth =~ s/;.*\z//; # drop ;AUTH=...
	$auth =~ s/\A[^:]+:// ? uri_unescape($auth) : undef; # drop ->user
}

sub auth {
	my ($self, $val) = @_;
	my ($scheme, $auth, $path, $query) = uri_split($$self);
	my $at_host_port;
	$auth =~ s/(@.*)\z// and $at_host_port = $1; # stash host:port for now
	if (scalar(@_) == 2) {
		if (defined $val) {
			my $uval = uri_escape($val);
			if ($auth =~ s!;AUTH=$achar!;AUTH=$uval!ix) {
				# replaced existing
			} elsif (defined($at_host_port)) {
				$auth .= ";AUTH=$uval";
			} else {
				substr($auth, 0, 0) = ";AUTH=$uval@";
			}
		} else { # clobber
			$auth =~ s!;AUTH=$achar!!i;
			if ($at_host_port && $auth eq '') {
				$at_host_port =~ s/\A\@//;
			}
		}
		$at_host_port //= '';
		$$self = uri_join($scheme, $auth.$at_host_port, $path, $query);
		$val;
	} else { # read-only
		$auth =~ /;AUTH=(.+)\z/i ? uri_unescape($1) : undef;
	}
}

sub scheme {
	my ($self) = @_;
	(uri_split($$self))[0];
}

sub as_string { ${$_[0]} }

sub clone { ref($_[0])->new(as_string($_[0])) }

1;
