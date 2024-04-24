# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# mocks Xapian::Mset and allows slow queries from blocking the event loop
package PublicInbox::XhcMset;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::XhcMsetIterator;
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT);

sub event_step {
	my ($self) = @_;
	my ($cb, @args) = @{delete $self->{cb_args} // return};
	my $rd = $self->{sock};
	eval {
		my $hdr = <$rd> // die "E: reading mset header: $!";
		for (split /\s+/, $hdr) { # read mset.size + estimated_matches
			my ($k, $v) = split /=/, $_, 2;
			$k =~ s/\A[^\.]*\.//; # s/(mset)?\./
			$self->{$k} = $v;
		}
		my $size = $self->{size} // die "E: bad xhc header: `$hdr'";
		my @it = map { PublicInbox::XhcMsetIterator::make($_) } <$rd>;
		$self->{items} = \@it;
		scalar(@it) == $size or die
			'E: got ',scalar(@it),", expected mset.size=$size";
	};
	my $err = $@;
	$self->close;
	eval { $cb->(@args, $self, $err) };
	warn "E: $@\n" if $@;
}

sub maybe_new {
	my (undef, $rd, $srch, @cb_args) = @_;
	my $self = bless { cb_args => \@cb_args, srch => $srch }, __PACKAGE__;
	if ($PublicInbox::DS::in_loop) { # async
		$self->SUPER::new($rd, EPOLLIN|EPOLLONESHOT);
	} else { # synchronous
		$self->{sock} = $rd;
		event_step($self);
		undef;
	}
}

eval(join('', map { "sub $_ { \$_[0]->{$_} }\n" } qw(size
	get_matches_estimated)));

sub items { @{$_[0]->{items}} }

1;
