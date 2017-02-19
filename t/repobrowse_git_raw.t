# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
my $test = require './t/repobrowse_common_git.perl';

test_psgi($test->{app}, sub {
	my ($cb) = @_;

	my $req = 'http://example.com/test.git/raw/master/dir';
	my $res = $cb->(GET($req));
	is(200, $res->code, 'got 200 response from dir');
	my $noslash_body = dechunk($res);
	like($noslash_body, qr{href="dir/dur">dur</a></li>},
		'path ok w/o slash');

	$req = 'http://example.com/test.git/raw/master/foo.txt';
	my $blob = $cb->(GET($req));
	like($blob->header('Content-Type'), qr!\Atext/plain\b!,
		'got text/plain blob');
	is($blob->content, "-----\nhello\nworld\n", 'raw blob passed');
});

done_testing();
