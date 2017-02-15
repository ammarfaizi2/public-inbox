# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
my $test = require './t/repobrowse_common_git.perl';

test_psgi($test->{app}, sub {
	my ($cb) = @_;

	my $req = 'http://example.com/test.git/tree/HEAD/dir';
	my $res = $cb->(GET($req));
	is(200, $res->code, 'got 200 response from dir');
	my $noslash_body = dechunk($res);
	like($noslash_body, qr{href="dir/dur">dur/</a>},
		'path ok w/o slash');

	my $slash = $req . '/';
	my $r2 = $cb->(GET($slash));
	is(301, $r2->code, 'got 301 response from dir with slash');
	is($req, $r2->header('Location'), 'redirected w/o slash');

	$req = 'http://example.com/test.git/tree/master/foo.txt';
	my $blob = $cb->(GET($req));
	is($blob->header('Content-Type'), 'text/html; charset=UTF-8',
		'got text/html blob');

	my $body = dechunk($blob);
	foreach (qw(----- hello world)) {
		ok(index($body, $_) >= 0, "substring $_ in body");
	}
});

done_testing();
