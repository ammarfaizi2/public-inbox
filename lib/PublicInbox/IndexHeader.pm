# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# allow searching on arbitrary headers as text
package PublicInbox::IndexHeader;
use v5.12;
use URI::Escape qw(uri_unescape);

my %T2IDX = ( # map to PublicInbox::SearchIdx methods
	phrase => 'index_phrase1',
	boolean_term => 'index_boolean_term',
	text => 'index_text1',
);

# also called by AltId->new
sub extra_indexer_new_common ($$$$) {
	my ($self, $spec, $pfx, $query) = @_;
	$pfx =~ /\A[a-z][a-z0-9]*\z/ or
		warn "W: non-word prefix in `$spec' not searchable\n";
	$self->{prefix} = $pfx;
	my %params = map {
		my ($k, $v) = split /=/, uri_unescape($_), 2;
		($k, $v // '');
	} split /[&;]/, $query // '';
	my $xpfx = delete($params{index_prefix}) // "X\U$pfx";
	$xpfx =~ /\A[A-Z][A-Z0-9]*\z/ or die
		die "E: `index_prefix' in `$spec' must be ALL CAPS\n";
	$self->{xprefix} = $xpfx;
	\%params;
}

sub new {
	my ($cls, $ibx, $spec) = @_;
	my ($type, $pfx, $header, $query) = split /:/, $spec, 4;
	$pfx // die "E: `$spec' has no user prefix\n";
	$header // die "E: `$spec' has no mail header\n";
	my $self = bless { header => $header, type => $type }, $cls;
	my $params = extra_indexer_new_common $self, $spec, $pfx, $query;
	$self->{hdr_method} = delete $params->{raw} ? 'header_raw' : 'header';
	my @k = keys %$params;
	warn "W: unknown params in `$spec': ", join(', ', @k), "\n" if @k;
	$T2IDX{$type} // die
		"E: `$type' not supported in $spec, must be one of: ",
		join(', ', sort keys %T2IDX), "\n";
	$self;
}

sub index_extra { # for PublicInbox::SearchIdx
	my ($self, $sidx, $eml, $mids) = @_;
	my $idx_method = $self->{-idx_method} //= $T2IDX{$self->{type}};
	my $hdr_method = $self->{hdr_method};
	for my $val ($eml->$hdr_method($self->{header})) {
		$sidx->$idx_method($self->{xprefix}, $val);
	}
}

sub user_help { # for PublicInbox::Search
	my ($self) = @_;
	("$self->{prefix}:", <<EOF);
the `$self->{header}' mail header  e.g. $self->{prefix}:stable
EOF
}

my %TYPE_2_QPMETHOD = (
	phrase => 'add_prefix',
	boolean_term => 'add_boolean_prefix',
	text => 'add_prefix',
);

# callback for PublicInbox::Search
sub query_parser_method { $TYPE_2_QPMETHOD{$_[0]->{type}} }

1;
