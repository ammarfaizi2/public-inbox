# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::Limiter;
use v5.12;
use PublicInbox::Spawn;
use PublicInbox::OnDestroy;

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
	for my $f (qw(max depth)) {
		my $k = "publicinboxlimiter.$name.$f";
		my $v = $cfg->{$k} // next;
		if ($v =~ /\A[1-9][0-9]*\z/) {
			$self->{$f} = $v + 0;
		} else {
			warn <<EOM
W: `$k=$v' is not a positive integer in $cfg->{-f}
EOM
		}
	}
	for my $rlim (@PublicInbox::Spawn::RLIMITS) {
		my $k = lc($rlim);
		$k =~ tr/_//d;
		$k = "publicinboxlimiter.$name.$k";
		my $v = $cfg->{$k} // next;
		my @rlimit = split(/\s*,\s*/, $v);
		if (scalar(@rlimit) == 1) {
			$rlimit[1] = $rlimit[0];
		} elsif (scalar(@rlimit) != 2) {
			warn <<EOM;
W: could not parse `$k=$v' in $cfg->{-f} (ignored)
EOM
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

sub _do_start ($$$$) {
	my ($self, $start_cb, $ctx, $fail_cb) = @_;
	$ctx->{"limiter.next.$self"} = on_destroy \&_start_next, $self;
	++$self->{running};
	eval { $start_cb->($ctx, $self) };
	if ($@) {
		print { $ctx->{env}->{'psgi.errors'} } "E: $@\n";
		$fail_cb->($ctx, 500, 'internal error');
	}
}

sub _start_next { # on_destroy cb
	my ($self) = @_;
	--$self->{running};
	my ($rec, $ck, $start_cb, $ctx, $fail_cb);
	while (1) {
		$rec = shift @{$self->{run_queue}} or return;
		($start_cb, $ctx, $fail_cb) = @$rec;
		$ck = $ctx->{env}->{'pi-httpd.ckhup'} or last;
		$ck->($ctx->{env}->{'psgix.io'}->{sock}) or last;
		$fail_cb->($ctx, 499, 'client disconnected');
	}
	_do_start $self, $start_cb, $ctx, $fail_cb;
}

sub may_start {
	my ($self, $start_cb, $ctx, $fail_cb) = @_;
	if ($self->{running} < $self->{max}) {
		_do_start $self, $start_cb, $ctx, $fail_cb;
	} elsif (@{$self->{run_queue}} > ($self->{depth} // 32)) {
		$fail_cb->($ctx, 503, 'too busy');
	} else {
		push @{$self->{run_queue}}, [ $start_cb, $ctx, $fail_cb ];
	}
}

1;
