# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::OnDestroy;

sub new {
	shift; # ($class, $cb, @args)
	bless [ @_ ], __PACKAGE__;
}

sub DESTROY {
	my ($cb, @args) = @{$_[0]};
	if (!ref($cb) && $cb) {
		my $pid = $cb;
		return if $pid != $$;
		$cb = shift @args;
	}
	$cb->(@args) if $cb;
}

1;
