# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::OnDestroy;
use v5.12;
use parent qw(Exporter);
use autodie qw(fork);
our @EXPORT = qw(on_destroy);
our $fork_gen = 0;

# either parent or child is expected to exit or exec shortly after this:
sub fork_tmp () {
	my $pid = fork;
	++$fork_gen if $pid == 0;
	$pid;
}

# all children
sub all (@) { bless [ undef, @_ ], __PACKAGE__ }

# same process
sub on_destroy (@) { bless [ $fork_gen, @_ ], __PACKAGE__ }

sub cancel { @{$_[0]} = () }

sub DESTROY {
	my ($fgen, $cb, @args) = @{$_[0]};
	$cb->(@args) if ($cb && ($fgen // $fork_gen) == $fork_gen);
}

1;
