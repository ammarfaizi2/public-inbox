# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Spamchecking used by -watch and -mda tools
package PublicInbox::Spamcheck;
use v5.12;

sub get {
	my ($cfg, $key, $default) = @_;
	my $spamcheck = $cfg->{$key} || $default;

	return if !$spamcheck || $spamcheck eq 'none';

	$spamcheck = 'PublicInbox::Spamcheck::Spamc' if $spamcheck eq 'spamc';
	if ($spamcheck =~ /::/) {
		eval "require $spamcheck";
		return $spamcheck->new;
	}
	warn "unsupported $key=$spamcheck\n";
	undef;
}

1;
