# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# query parameter management for repobrowse
package PublicInbox::RepoGitQuery;
use strict;
use warnings;
use PublicInbox::Hval;
use URI::Escape qw(uri_unescape);
my @KNOWN_PARAMS = qw(ofs);

sub new {
	my ($class, $env) = @_;
	# we don't care about multi-value
	my %tmp = map {
		my ($k, $v) = split('=', uri_unescape($_), 2);
		$v = '' unless defined $v;
		$v =~ tr/+/ /;
		($k, $v)
	} split(/[&;]/, $env->{QUERY_STRING});

	my $self = {};
	foreach (@KNOWN_PARAMS) {
		my $v = $tmp{$_};
		$self->{$_} = defined $v ? $v : '';
	}
	bless $self, $class;
}

sub qs {
	my ($self, %over) = @_;

	if (keys %over) {
		my $tmp = bless { %$self }, ref($self);
		foreach my $k (keys %over) { $tmp->{$k} = $over{$k}; }
		$self = $tmp;
	}

	my @qs;
	foreach my $k (@KNOWN_PARAMS) {
		my $v = $self->{$k};

		next if ($v eq '');
		$v = PublicInbox::Hval->new($v)->as_href;
		push @qs, "$k=$v";
	}
	scalar(@qs) ? ('?' . join('&amp;', @qs)) : '';
}

1;
