# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::RepoBrowseQuery;
use strict;
use warnings;
use PublicInbox::Hval;

sub new {
	my ($class, $cgi) = @_;
	my $self = bless {}, $class;

	foreach my $k (qw(id h showmsg ofs)) {
		my $v = $cgi->param($k);
		$self->{$k} = defined $v ? $v : '';
	}
	$self;
}

sub qs {
	my ($self, %over) = @_;

	if (keys %over) {
		my $tmp = bless { %$self }, ref($self);
		foreach my $k (keys %over) { $tmp->{$k} = $over{$k}; }
		$self = $tmp;
	}

	my @qs;
	foreach my $k (qw(id h showmsg ofs)) {
		my $v = $self->{$k};

		next if ($v eq '');
		$v = PublicInbox::Hval->new($v)->as_href;
		push @qs, "$k=$v";
	}
	scalar(@qs) ? ('?' . join('&amp;', @qs)) : '';
}

1;
