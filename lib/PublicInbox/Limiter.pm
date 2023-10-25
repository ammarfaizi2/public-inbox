# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::Limiter;
use v5.12;
use PublicInbox::Spawn;

sub new {
	my ($class, $max) = @_;
	bless {
		# 32 is same as the git-daemon connection limit
		max => $max || 32,
		running => 0,
		run_queue => [],
		# RLIMIT_CPU => undef,
		# RLIMIT_DATA => undef,
		# RLIMIT_CORE => undef,
	}, $class;
}

sub setup_rlimit {
	my ($self, $name, $cfg) = @_;
	for my $rlim (@PublicInbox::Spawn::RLIMITS) {
		my $k = lc($rlim);
		$k =~ tr/_//d;
		$k = "publicinboxlimiter.$name.$k";
		my $v = $cfg->{$k} // next;
		my @rlimit = split(/\s*,\s*/, $v);
		if (scalar(@rlimit) == 1) {
			push @rlimit, $rlimit[0];
		} elsif (scalar(@rlimit) != 2) {
			warn "could not parse $k: $v\n";
		}
		eval { require BSD::Resource };
		if ($@) {
			warn "BSD::Resource missing for $rlim";
			next;
		}
		for my $i (0..$#rlimit) {
			next if $rlimit[$i] ne 'INFINITY';
			$rlimit[$i] = BSD::Resource::RLIM_INFINITY();
		}
		$self->{$rlim} = \@rlimit;
	}
}

1;
