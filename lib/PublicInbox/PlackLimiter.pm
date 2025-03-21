# Copyright (C) all contributors <meta@public-inbox.org>
# License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
# generic Plack/PSGI middleware to expose PublicInbox::Limiter, (see __END__)
package PublicInbox::PlackLimiter;
use v5.12;
use parent qw(Plack::Middleware);
use PublicInbox::Limiter;

sub prepare_app { # called via Plack::Component (used by Plack::Middleware)
	my ($self) = @_;
	$self->{match_cb} //= sub { 1 };
	$self->{max} //= 2;
	$self->{run_queue} = [];
	$self->{running} = 0;
}

sub lim_fail { # limiter->may_start fail_cb
	my ($ctx, $code, $msg) = @_;
	delete($ctx->{psgi_wcb})->([ $code, [ 'Content-Type' => 'text/plain',
		'Content-Length' => length($msg) ], [ $msg ] ]);
}

sub stats ($) {
	my ($self) = @_;
	my $nq = scalar @{$self->{run_queue}};
	my $res = <<EOM;
running: $self->{running}
queued: $nq
max: $self->{max}
EOM
	[ 200, [ 'Content-Type' => 'text/plain',
		'Content-Length' => length($res) ], [ $res ] ]
}

sub app_call { # limiter->may_start start_cb
	my ($ctx, $self) = @_;
	my $wcb = delete $ctx->{psgi_wcb};
	my $env = delete $ctx->{env}; # avoid cyclic ref
	push @{$env->{'limiter.ctx'}}, $ctx; # handoff limiter.next.$self
	my $res = eval { $self->app->($env) };
	return warn("W: $@") if $@;
	ref($res) eq 'CODE' ? $res->($wcb) : $wcb->($res);
}

sub call {
	my ($self, $env) = @_;
	if (defined $self->{stats_match_cb}) {
		return stats $self if $self->{stats_match_cb}->($env);
	}
	return $self->app->($env) if !$self->{match_cb}->($env);
	sub { # capture write cb from PSGI server
		my $ctx = { env => $env, psgi_wcb => $_[0] };
		PublicInbox::Limiter::may_start(
				$self, \&app_call, $ctx, \&lim_fail);
	}
}

1;
__END__

=head1 NAME

PublicInbox::PlackLimiter - limit concurrency to parts of a PSGI app

=head1 SYNOPSIS

	# In your .psgi file
	use Plack::Builder;
	builder {

	# by default, only 2 requests may be processed at once:
	enable '+PublicInbox::PlackLimiter';

	# You will likely only want to limit certain expensive endpoints,
	# while allowing maximum concurrency for inexpensive endpoints.
	# You can do that by passing a `match_cb' parameter:
	enable '+PublicInbox::PlackLimiter',
		# some expensive endpoints for my public-inbox instance, YMMV
		match_cb => sub {
			my ($env) = @_;
			$env->{PATH_INFO} =~ m!/(?:[Ttd]/|.+\.
						(?:mbox\.gz|atom|html))\z!x ||
				$env->{QUERY_STRING} =~ /\bx=[tA]\b/
		},
		# You can increase `max' and `depth' to higher numbers
		max => 3, # maximum concurrent requests
		depth => 128, # maximum queue depth (size)
		# You can also enable a stats endpoint if you wish (optional):
		stats_match_cb => sub {
			my ($env) = @_;
			$env->{REQUEST_URI} eq '/stats' &&
				$env->{REMOTE_ADDR} eq '127.0.0.1'
		};
	# ...
	}; # /builder

=head1 DESCRIPTION

PublicInbox::PlackLimiter lets a sysadmin limit concurrency to certain
expensive endpoints while allowing the normal concurrency level of the
server to run inexpensive requests.

=head1 SEE ALSO

L<Plack> L<Plack::Builder> L<Plack::Middleware>

=cut
