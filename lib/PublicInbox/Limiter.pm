# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::Limiter;
use v5.12;
use PublicInbox::Spawn;

sub new {
	my ($class, $max) = @_;
	bless {
		# 32 is same as the git-daemon connection limit, but
		# -cgit and -codeblob internal limiters default to 1
		max => $max || 32,
		running => 0,
		run_queue => [],
		# RLIMIT_CPU => undef,
		# RLIMIT_DATA => undef,
		# RLIMIT_CORE => undef,
	}, $class;
}

sub setup_limiter {
	my ($self, $name, $cfg) = @_;
	my $k = "publicinboxlimiter.$name.depth";
	my $v = $cfg->{$k};
	if (defined $v) {
		if ($v =~ /\A[1-9][0-9]*\z/) {
			$self->{depth} = $v + 0;
		} else {
			warn "W: `$v' not a positive integer in $cfg->{-f}\n";
		}
	}
	for my $rlim (@PublicInbox::Spawn::RLIMITS) {
		$k = lc($rlim);
		$k =~ tr/_//d;
		$k = "publicinboxlimiter.$name.$k";
		$v = $cfg->{$k} // next;
		my @rlimit = split(/\s*,\s*/, $v);
		if (scalar(@rlimit) == 1) {
			$rlimit[1] = $rlimit[0];
		} elsif (scalar(@rlimit) != 2) {
			warn "W: could not parse $k: $v (ignored)\n";
			next;
		}
		my $inf = $v =~ /\binfinity\b/i ?
			$PublicInbox::Spawn::RLIMITS{RLIM_INFINITY} // eval {
				require BSD::Resource;
				BSD::Resource::RLIM_INFINITY();
			} // do {
				warn "BSD::Resource missing for $rlim";
				next;
			} : undef;
		for (@rlimit) {
			$_ = $inf if $_ eq 'INFINITY';
		}
		$self->{$rlim} = \@rlimit;
	}
}

sub is_too_busy {
	my ($self) = @_;
	scalar(@{$self->{run_queue}}) > ($self->{depth} // 32)
}

1;
