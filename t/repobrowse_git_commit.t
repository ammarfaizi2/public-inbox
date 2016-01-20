# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;

my $test = require './t/repobrowse_common_git.perl';
test_psgi($test->{app}, sub {
	my ($cb) = @_;
	my $path = '/path/to/something';
	my $req = 'http://example.com/test.git/commit';
	my $res = $cb->(GET($req . $path));
	is($res->code, 301, 'got 301 to anchor');
	is($res->header('Location'), "$req#path:to:something",
		'redirected to anchor from path');

	my $q = '?id=deadbeef';
	$res = $cb->(GET($req . $path . $q));
	is($res->code, 301, 'got 301 with query string');
	is($res->header('Location'), "$req$q#path:to:something",
		'redirected to anchor from path with query');
});

done_testing();
