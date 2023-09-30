# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# connects public-inbox processes to PublicInbox::Gcf2::loop()
package PublicInbox::Gcf2Client;
use v5.12;
use parent qw(PublicInbox::DS);
use PublicInbox::Git;
use PublicInbox::Gcf2; # fails if Inline::C or libgit2-dev isn't available
use PublicInbox::Spawn qw(spawn);
use Socket qw(AF_UNIX SOCK_STREAM);
use PublicInbox::Syscall qw(EPOLLIN EPOLLET);
use PublicInbox::ProcessPipe;

# fields:
#	sock => socket to Gcf2::loop
# The rest of these fields are compatible with what PublicInbox::Git
# uses code-sharing
#	pid => PID of Gcf2::loop process
#	pid.owner => process which spawned {pid}
#	in => same as {sock}, for compatibility with PublicInbox::Git
#	inflight => array (see PublicInbox::Git)
#	rbuf => scalarref, may be non-existent or empty
sub new  {
	my ($opt) = @_;
	my $self = bless {}, __PACKAGE__;
	# ensure the child process has the same @INC we do:
	my $env = { PERL5LIB => join(':', @INC) };
	my ($s1, $s2);
	socketpair($s1, $s2, AF_UNIX, SOCK_STREAM, 0) or die "socketpair $!";
	$s1->blocking(0);
	$opt->{0} = $opt->{1} = $s2;
	my $cmd = [$^X, qw[-MPublicInbox::Gcf2 -e PublicInbox::Gcf2::loop]];
	my $pid = spawn($cmd, $env, $opt);
	my $sock = PublicInbox::ProcessPipe->maybe_new($pid, $s1);
	$self->{inflight} = [];
	$self->{epwatch} = \undef; # for Git->cleanup
	$self->SUPER::new($sock, EPOLLIN|EPOLLET);
}

sub gcf2_async ($$$;$) {
	my ($self, $req, $cb, $arg) = @_;
	my $inflight = $self->{inflight} or return $self->close;
	PublicInbox::Git::write_all($self, $$req, \&cat_async_step, $inflight);
	push @$inflight, $req, $cb, $arg;
}

# ensure PublicInbox::Git::cat_async_step never calls cat_async_retry
sub alternates_changed {}

no warnings 'once';

*cat_async_step = \&PublicInbox::Git::cat_async_step; # for event_step
*event_step = \&PublicInbox::Git::event_step;
*DESTROY = \&PublicInbox::Git::DESTROY;

1;
