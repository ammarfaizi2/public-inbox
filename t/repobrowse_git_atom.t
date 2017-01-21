# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
my $have_xml_feed = eval { require XML::Feed; 1 };
my $test = require './t/repobrowse_common_git.perl';
use Test::More;

test_psgi($test->{app}, sub {
	my ($cb) = @_;
	my $req = 'http://example.com/test.git/atom';
	my $res = $cb->(GET($req));
	is($res->code, 200, 'got 200');
	is($res->header('Content-Type'), 'application/atom+xml',
		'got correct Content-Type');
	my $body = dechunk($res);
	SKIP: {
		skip 'XML::Feed missing', 2 unless $have_xml_feed;
		my $p = XML::Feed->parse(\$body);
		is($p->format, "Atom", "parsed atom feed");
		is(scalar $p->entries, 6, "parsed six entries");
	}
	like($body, qr!<pre\s*[^>]+>\* header:\n  add header</pre>!,
		'body wrapped in <pre>');

	$res = $cb->(GET($req . '/'));
	my $sl = dechunk($res);
	is($body, $sl, 'slash returned identical to non-trailing slash');

	$res = $cb->(GET($req . '/foo.txt'));
	is($res->code, 200, 'got 200');
	$body = dechunk($res);
	like($body, qr{\bhref="http://[^/]+/test\.git/}, 'hrefs OK');
	SKIP: {
		skip 'XML::Feed missing', 2 unless $have_xml_feed;
		my $p = XML::Feed->parse(\$body);
		is($p->format, "Atom", "parsed atom feed");
		is(scalar $p->entries, 4, "parsed 4 entries");
	}
});

done_testing();
