# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# mocks Xapian::Mset and allows slow queries from blocking the event loop
package PublicInbox::XhcMset;
use v5.12;
use parent qw(PublicInbox::DS);
use autodie qw(seek);
use PublicInbox::IO qw(read_all);
use PublicInbox::XhcMsetIterator;
use PublicInbox::Syscall qw(EPOLLIN EPOLLONESHOT);
use Fcntl qw(SEEK_SET);
use PublicInbox::Git qw(git_quote);

sub die_err ($@) {
	my ($self, @msg) = @_;
	my $err_rw = delete $self->{err_rw};
	seek $err_rw, 0, SEEK_SET;
	my $s = read_all $err_rw;
	chomp $s;
	die $s, @msg, "\n";
}

sub event_step {
	my ($self) = @_;
	my ($cb, @args) = @{delete $self->{cb_args} // return};
	my $rd = $self->{sock};
	eval {
		my $hdr = <$rd> //
			die_err $self, $! ? ("reading mset header: $!") : ();
		for (split /\s+/, $hdr) { # read mset.size + estimated_matches
			my ($k, $v) = split /=/, $_, 2;
			$k =~ s/\A[^\.]*\.//; # s/(mset)?\./
			$self->{$k} = $v;
		}
		my $size = $self->{size} //
			die_err $self, 'bad xhc header: ', git_quote($hdr);
		my @it = map { PublicInbox::XhcMsetIterator::make($_) } <$rd>;
		$self->{items} = \@it;
		scalar(@it) == $size or die_err $self,
			'got ', scalar(@it), ', expected mset.size=', $size;
	};
	chomp(my $err = $@);
	$self->close;
	eval { $cb->(@args, $self, $err) };
	warn "E: $@\n" if $@;
}

sub maybe_new {
	my (undef, $out_rd, $err_rw, $srch, @cb_args) = @_;
	my $self = bless {
		cb_args => \@cb_args,
		srch => $srch,
		err_rw => $err_rw,
	}, __PACKAGE__;
	if ($PublicInbox::DS::in_loop) { # async
		$self->SUPER::new($out_rd, EPOLLIN|EPOLLONESHOT);
	} else { # synchronous
		$self->{sock} = $out_rd;
		event_step($self);
		undef;
	}
}

eval(join('', map { "sub $_ { \$_[0]->{$_} }\n" } qw(size
	get_matches_estimated)));

sub items { @{$_[0]->{items}} }

1;
