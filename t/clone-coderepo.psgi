#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# for clone-coderepo.t
use v5.12;
use Plack::Builder;
use PublicInbox::WwwStatic;
use PublicInbox::WWW;
my $www = PublicInbox::WWW->new;
my $static = PublicInbox::WwwStatic->new(docroot => $ENV{TEST_DOCROOT});
builder {
	enable 'Head';
	sub {
		my ($env) = @_;
		if ($env->{PATH_INFO} eq '/manifest.js.gz') {
			my $res = $static->call($env);
			return $res if $res->[0] != 404;
		}
		$www->call($env);
	};
}
