# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# temporary stack for public-inbox-index
# FIXME: needs to support multi-hash in the same repo once git itself can
package PublicInbox::IdxStack;
use v5.12;
use Fcntl qw(:seek);
use constant PACK_FMT => eval { pack('Q', 1) } ? 'A1QQH*H*' : 'A1IIH*H*';
use autodie qw(open seek);
use PublicInbox::IO qw(read_all);

# start off in write-only mode
sub new {
	open(my $io, '+>', undef);
	# latest_cmt is still useful when the newest revision is a `d'(elete),
	# otherwise we favor $self->{latest_cmt} for checkpoints and {quit}
	bless { wr => $io, latest_cmt => $_[1] }, __PACKAGE__
}

# file_char = [d|m]
sub push_rec {
	my ($self, $file_char, $at, $ct, $blob_oid, $cmt_oid) = @_;
	my $rec = pack(PACK_FMT, $file_char, $at, $ct, $blob_oid, $cmt_oid);
	$self->{unpack_fmt} // do {
		my $len = length($cmt_oid);
		my $fmt = PACK_FMT;
		$fmt =~ s/H\*/H$len/g;
		$self->{rec_size} = length($rec);
		$self->{unpack_fmt} = $fmt;
	};
	print { $self->{wr} } $rec;
	$self->{tot_size} += length($rec);
}

sub num_records {
	my ($self) = @_;
	$self->{rec_size} ? $self->{tot_size} / $self->{rec_size} : 0;
}

# switch into read-only mode and returns self
sub read_prepare {
	my ($self) = @_;
	my $io = $self->{rd} = delete($self->{wr});
	$io->flush or die "$io->flush: $!";
	$self;
}

sub pop_rec {
	my ($self) = @_;
	my $sz = $self->{rec_size} or return;
	my $rec_pos = $self->{tot_size} -= $sz;
	return if $rec_pos < 0;
	seek($self->{rd}, $rec_pos, SEEK_SET);
	unpack($self->{unpack_fmt}, read_all($self->{rd}, $sz));
}

1;
