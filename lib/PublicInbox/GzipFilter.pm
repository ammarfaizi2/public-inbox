# Copyright (C) 2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Qspawn filter
package PublicInbox::GzipFilter;
use strict;
use Compress::Raw::Zlib qw(Z_FINISH Z_OK);
my %OPT = (-WindowBits => 15 + 16, -AppendOutput => 1);

sub new { bless {}, shift }

# for Qspawn if using $env->{'pi-httpd.async'}
sub attach {
	my ($self, $fh) = @_;
	$self->{fh} = $fh;
	$self
}

# for GetlineBody (via Qspawn) when NOT using $env->{'pi-httpd.async'}
sub translate ($$) {
	my $self = $_[0];

	# allocate the zlib context lazily here, instead of in ->new.
	# Deflate contexts are memory-intensive and this object may
	# be sitting in the Qspawn limiter queue for a while.
	my $gz = $self->{gz} //= do {
		my ($g, $err) = Compress::Raw::Zlib::Deflate->new(%OPT);
		$err == Z_OK or die "Deflate->new failed: $err";
		$g;
	};
	my $zbuf = delete($self->{zbuf});
	if (defined $_[1]) { # my $buf = $_[1];
		my $err = $gz->deflate($_[1], $zbuf);
		die "gzip->deflate: $err" if $err != Z_OK;
		return $zbuf if length($zbuf) >= 8192;

		$self->{zbuf} = $zbuf;
		'';
	} else { # undef == EOF
		my $err = $gz->flush($zbuf, Z_FINISH);
		die "gzip->flush: $err" if $err != Z_OK;
		$zbuf;
	}
}

sub write {
	# my $ret = bytes::length($_[1]); # XXX does anybody care?
	$_[0]->{fh}->write(translate($_[0], $_[1]));
}

sub close {
	my ($self) = @_;
	my $fh = delete $self->{fh};
	$fh->write(translate($self, undef));
	$fh->close;
}

1;
