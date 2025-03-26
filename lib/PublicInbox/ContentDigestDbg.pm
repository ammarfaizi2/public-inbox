# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::ContentDigestDbg; # cf. PublicInbox::ContentDigest
use v5.12;
use PublicInbox::Git qw(git_quote);
use PublicInbox::SHA;

sub new { bless [ PublicInbox::SHA->new(256), $_[1] ], __PACKAGE__ }

sub add {
	my ($dig, $out) = @{$_[0]};
	$dig->add($_[1]);
	my @dbg = split /^/sm, $_[1];
	# fold excessively long lines
	@dbg && $dbg[0] =~ /\A(?:To|Cc)\0.*?\,/ and
		@dbg = map { split(/,/s, $_) } @dbg;
	for (@dbg) {
		$_ = git_quote($_);
		s/\\000/\\0/g; # less ugly
		# n.b. caller must (autodie) `close' to error check `say'
		say $out ' ', $_;
	}
}

sub hexdigest { $_[0]->[0]->hexdigest }

1;
