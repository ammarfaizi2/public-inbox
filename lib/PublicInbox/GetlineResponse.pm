# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# For generic PSGI servers (not public-inbox-httpd/netd) which assumes their
# getline response bodies can be backpressure-aware for slow clients
# This depends on rpipe being _blocking_ on getline.
package PublicInbox::GetlineResponse;
use v5.12;

sub response {
	my ($qsp) = @_;
	my ($res, $rbuf);
	do { # read header synchronously
		sysread($qsp->{rpipe}, $rbuf, 65536);
		$res = $qsp->parse_hdr_done($rbuf); # fills $bref
	} until defined($res);
	my ($wcb, $filter) = $qsp->yield_pass(undef, $res) or return;
	my $self = $res->[2] = bless {
		qsp => $qsp,
		filter => $filter,
	}, __PACKAGE__;
	my ($bref) = @{delete $qsp->{yield_parse_hdr}};
	$self->{rbuf} = $$bref if $$bref ne '';
	$wcb->($res);
}

sub getline {
	my ($self) = @_;
	my $rpipe = $self->{qsp}->{rpipe} // do {
		delete($self->{qsp})->finish;
		return; # EOF was set on previous call
	};
	my $buf = delete($self->{rbuf}) // $rpipe->getline;
	$buf // delete($self->{qsp}->{rpipe}); # set EOF for next call
	$self->{filter} ? $self->{filter}->translate($buf) : $buf;
}

sub close {}

1;
