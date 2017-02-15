# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;

my $test = require './t/repobrowse_common_git.perl';
test_psgi($test->{app}, sub {
	my ($cb) = @_;
	my $path = '/path/to/something';
	my $req = 'http://example.com/test.git/commit';
	my $res;

	$res = $cb->(GET($req));
	is($res->code, 200, 'got proper 200 response for default');
	my $body = dechunk($res);
	like($body, qr!</html>\z!, 'response body finished');
});

done_testing();
