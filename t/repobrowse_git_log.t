# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
my $test = require './t/repobrowse_common_git.perl';
use Test::More;

test_psgi($test->{app}, sub {
	my ($cb) = @_;
	my $req = 'http://example.com/test.git/log';
	my $res = $cb->(GET($req));
	is($res->code, 200, 'got 200');
	is($res->header('Content-Type'), 'text/html',
		'got correct Content-Type');
	my $body = dechunk($res);
	like($body, qr!</html>!, 'valid HTML :)');
});

done_testing();
