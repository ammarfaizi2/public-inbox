# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# config writer, may use libgit2 in the future
package PublicInbox::CfgWr;
use v5.12;
use PublicInbox::Git qw(git_exe);
use PublicInbox::Spawn qw(run_die run_wait);

sub new {
	my ($cls, $f) = @_;
	bless { -f => $f }, $cls;
}

sub set {
	my ($self, $k, $v) = @_;
	push @{$self->{todo}}, [ $k, $v ];
	$self;
}

sub add {
	my ($self, $k, $v) = @_;
	push @{$self->{todo}}, [ '--add', $k, $v ];
	$self;
}

sub replace_all {
	my ($self, $k, $v, $re) = @_;
	push @{$self->{todo}}, [ '--replace-all', $k, $v, $re ];
	$self;
}

sub unset_all {
	my ($self, $k) = @_;
	push @{$self->{todo}}, [ '--unset-all', $k ];
	$self;
}

sub commit {
	my ($self, $opt) = @_;
	my @x = (git_exe, 'config', '-f', $self->{-f});
	for my $c (@{delete $self->{todo} // []}) {
		unshift @$c, @x;
		if ($c->[scalar(@x)] eq '--unset-all') {
			run_wait $c, undef, $opt;
			# ignore ret=5 if no matches (see git-config(1))
			die "E: @$c \$?=$?" if ($? && ($? >> 8) != 5);
		} else {
			run_die $c, undef, $opt;
		}
	}
}

1;
