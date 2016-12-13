# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;

my $test = require './t/repobrowse_common_git.perl';
test_psgi($test->{app}, sub {
	my ($cb) = @_;
	my $req = 'http://example.com/test.git/tree/dir';
	my $res = $cb->(GET($req . '/'));
	is($res->code, 301, 'got 301 with trailing slash');
	is($res->header('Location'), $req, 'redirected without tslash');

	my $q = '?id=deadbeef';

	$res = $cb->(GET($req . "/$q"));
	is($res->code, 301, 'got 301 with trailing slash + query string');
	is($res->header('Location'), $req.$q, 'redirected without tslash');
});

done_testing();
