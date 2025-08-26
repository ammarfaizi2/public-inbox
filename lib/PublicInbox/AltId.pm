# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Used for giving serial numbers to messages.  This can be tied to
# the msgmap for live updates to living lists (see
# PublicInbox::Filters::RubyLang), or kept separate for imports
# of defunct NNTP groups (e.g. scripts/xhdr-num2mid)
#
# Introducing NEW uses of serial numbers is discouraged because of
# it leads to reliance on centralization.  However, being able
# to use existing serial numbers is beneficial.
package PublicInbox::AltId;
use v5.12;
use parent qw(PublicInbox::IndexHeader);

# spec: TYPE:PREFIX:param1=value1&param2=value2&...
# The PREFIX will be a searchable boolean prefix in Xapian
# Example: serial:gmane:file=/path/to/altmsgmap.sqlite3
sub new {
	my ($class, $ibx, $spec, $writable) = @_;
	my ($type, $pfx, $query) = split /:/, $spec, 3;
	$type eq 'serial' or die "E: non-serial not supported, yet ($spec)\n";
	my $self = bless {}, $class;
	my $params = $self->extra_indexer_new_common($spec, $pfx, $query);
	my $f = delete $params->{file} or
		die "E: file= required for $type spec $spec\n";
	unless (index($f, '/') == 0) {
		if ($ibx->version == 1) {
			$f = "$ibx->{inboxdir}/public-inbox/$f";
		} else {
			$f = "$ibx->{inboxdir}/$f";
		}
	}
	my @k = keys %$params;
	warn "W: unknown params in `$spec': ", join(', ', @k), "\n" if @k;
	$self->{filename} = $f;
	$self->{writable} = $writable if $writable;
	$self;
}

sub mm_alt ($) {
	my ($self) = @_;
	$self->{mm_alt} ||= eval {
		require PublicInbox::Msgmap;
		# TODO: expose a way to disable fsync + enable WAL
		my $opt = $self->{writable} ? { fsync => 1 } : undef;
		PublicInbox::Msgmap->new_file($self->{filename}, $opt);
	};
}

sub index_extra { # for PublicInbox::SearchIdx
	my ($self, $sidx, $eml, $mids) = @_;
	for my $mid (@$mids) {
		my $id = mm_alt($self)->num_for($mid) // next;
		$sidx->index_boolean_term($self->{xprefix}, $id);
	}
}

sub user_help { # for PublicInbox::Search
	my ($self) = @_;
	($self->{prefix}, <<EOF);
alternate serial number  e.g. $self->{prefix}:12345 (boolean)
EOF
}

# callback for PublicInbox::Search
sub query_parser_method { 'add_boolean_prefix' }

1;
