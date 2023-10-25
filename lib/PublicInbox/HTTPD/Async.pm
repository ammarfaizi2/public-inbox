# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# XXX This is a totally unstable API for public-inbox internal use only
# This is exposed via the 'pi-httpd.async' key in the PSGI env hash.
# The name of this key is not even stable!
# Currently intended for use with read-only pipes with expensive
# processes such as git-http-backend(1), cgit(1)
#
# fields:
# http: PublicInbox::HTTP ref
# fh: PublicInbox::HTTP::{Identity,Chunked} ref (can ->write + ->close)
# cb: initial read callback
# arg: arg for {cb}
# end_obj: CODE or object which responds to ->event_step when ->close is called
package PublicInbox::HTTPD::Async;
use v5.12;
use parent qw(PublicInbox::DS);
use Errno qw(EAGAIN);
use PublicInbox::Syscall qw(EPOLLIN);
use PublicInbox::ProcessIONBF;

# This is called via: $env->{'pi-httpd.async'}->()
# $io is a read-only pipe ($rpipe) for now, but may be a
# bidirectional socket in the future.
sub new {
	my ($class, $io, $cb, $arg, $end_obj) = @_;
	my $self = bless {
		cb => $cb, # initial read callback
		arg => $arg, # arg for $cb
		end_obj => $end_obj, # like END{}, can ->event_step
	}, $class;
	PublicInbox::ProcessIONBF->replace($io);
	$self->SUPER::new($io, EPOLLIN);
}

sub event_step {
	my ($self) = @_;
	if (defined $self->{cb}) {
		# this may call async_pass when headers are done
		$self->{cb}->($self->{arg});
	} elsif (my $sock = $self->{sock}) {
		# $http may be undef if discarding body output from cgit on 404
		my $http = $self->{http} or return $self->close;
		# $self->{sock} is a read pipe for git-http-backend or cgit
		# and 65536 is the default Linux pipe size
		my $r = sysread($sock, my $buf, 65536);
		if ($r) {
			$self->{ofh}->write($buf); # may call $http->close
			# let other clients get some work done, too
			return if $http->{sock}; # !closed

			# else: fall through to close below...
		} elsif (!defined $r && $! == EAGAIN) {
			return; # EPOLLIN means we'll be notified
		}

		# Done! Error handling will happen in $self->{ofh}->close
		# called by end_obj->event_step handler
		delete $http->{forward};
		$self->close; # queues end_obj->event_step to be called
	} # else { # we may've been requeued but closed by $http
}

# once this is called, all data we read is passed to the
# to the PublicInbox::HTTP instance ($http) via $ofh->write
# $ofh is typically PublicInbox::HTTP::{Chunked,Identity}, but
# may be PublicInbox::GzipFilter or $PublicInbox::Qspawn::qx_fh
sub async_pass {
	my ($self, $http, $ofh, $bref) = @_;
	delete @$self{qw(cb arg)};
	# In case the client HTTP connection ($http) dies, it
	# will automatically close this ($self) object.
	$http->{forward} = $self;

	# write anything we overread when we were reading headers.
	# This is typically PublicInbox:HTTP::{chunked,identity}_wcb,
	# but may be PublicInbox::GzipFilter::write.  PSGI requires
	# *_wcb methods respond to ->write (and ->close), not ->print
	$ofh->write($$bref);

	$self->{http} = $http;
	$self->{ofh} = $ofh;
}

# may be called as $forward->close in PublicInbox::HTTP or EOF (event_step)
sub close {
	my $self = $_[0];
	$self->SUPER::close; # DS::close
	delete @$self{qw(cb arg)};

	# we defer this to the next timer loop since close is deferred
	if (my $end_obj = delete $self->{end_obj}) {
		# this calls $end_obj->event_step
		# (likely PublicInbox::Qspawn::event_step,
		#  NOT PublicInbox::HTTPD::Async::event_step)
		PublicInbox::DS::requeue($end_obj);
	}
}

1;
