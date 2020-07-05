# Copyright (C) 2016-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# For retrieving attachments from messages in the WWW interface
package PublicInbox::WwwAttach; # internal package
use strict;
use parent qw(PublicInbox::GzipFilter);
use bytes (); # only for bytes::length
use PublicInbox::EmlContentFoo qw(parse_content_type);
use PublicInbox::Eml;

sub get_attach_i { # ->each_part callback
	my ($part, $depth, $idx) = @{$_[0]};
	my $ctx = $_[1];
	return if $idx ne $ctx->{idx}; # [0-9]+(?:\.[0-9]+)+
	my $res = $ctx->{res};
	$res->[0] = 200;
	my $ct = $part->content_type;
	$ct = parse_content_type($ct) if $ct;

	if ($ct && (($ct->{type} || '') eq 'text')) {
		# display all text as text/plain:
		my $cset = $ct->{attributes}->{charset};
		if ($cset && ($cset =~ /\A[a-zA-Z0-9_\-]+\z/)) {
			$res->[1]->[1] .= qq(; charset=$cset);
		}
		$ctx->{gz} = PublicInbox::GzipFilter::gz_or_noop($res->[1],
								$ctx->{env});
		$part = $ctx->zflush($part->body);
	} else { # TODO: allow user to configure safe types
		$res->[1]->[1] = 'application/octet-stream';
		$part = $part->body;
	}
	push @{$res->[1]}, 'Content-Length', bytes::length($part);
	$res->[2]->[0] = $part;
}

sub async_eml { # ->{async_eml} for async_blob_cb
	my ($ctx, $eml) = @_;
	eval { $eml->each_part(\&get_attach_i, $ctx, 1) };
	if ($@) {
		$ctx->{res}->[0] = 500;
		warn "E: $@";
	}
}

sub async_next {
	my ($http) = @_;
	my $ctx = $http->{forward} or return; # client aborted
	# finally, we call the user-supplied callback
	eval { $ctx->{wcb}->($ctx->{res}) };
	warn "E: $@" if $@;
}

sub scan_attach ($) { # public-inbox-httpd only
	my ($ctx) = @_;
	$ctx->{env}->{'psgix.io'}->{forward} = $ctx;
	$ctx->{async_eml} = \&async_eml;
	$ctx->{async_next} = \&async_next;
	$ctx->smsg_blob($ctx->{smsg});
}

# /$LISTNAME/$MESSAGE_ID/$IDX-$FILENAME
sub get_attach ($$$) {
	my ($ctx, $idx, $fn) = @_;
	$ctx->{res} = [ 404, [ 'Content-Type' => 'text/plain' ],
				[ "Not found\n" ] ];
	$ctx->{idx} = $idx;
	bless $ctx, __PACKAGE__;
	my $eml;
	if ($ctx->{smsg} = $ctx->{-inbox}->smsg_by_mid($ctx->{mid})) {
		return sub { # public-inbox-httpd-only
			$ctx->{wcb} = $_[0];
			scan_attach($ctx);
		} if $ctx->{env}->{'pi-httpd.async'};
		# generic PSGI:
		$eml = $ctx->{-inbox}->smsg_eml($ctx->{smsg});
	} elsif (!$ctx->{-inbox}->over) {
		if (my $bref = $ctx->{-inbox}->msg_by_mid($ctx->{mid})) {
			$eml = PublicInbox::Eml->new($bref);
		}
	}
	$eml->each_part(\&get_attach_i, $ctx, 1) if $eml;
	$ctx->{res}
}

1;
