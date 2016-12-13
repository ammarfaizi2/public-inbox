# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
my $test = require './t/repobrowse_common_git.perl';

test_psgi($test->{app}, sub {
	my ($cb) = @_;
	my ($req, $rc, $res);

	$req = 'http://example.com/test.git/snapshot/test-master.tar.gz';
	$res = $cb->(GET($req));
	is($res->code, 200, 'got 200 response from $NAME-master-tar.gz');
	is($res->header('Content-Type'), 'application/x-gzip',
		'Content-Type is as expected');

	$req = 'http://example.com/test.git/snapshot/test-nonexistent.tar.gz';
	$res = $cb->(GET($req));
	is($res->code, 404, 'got 404 for non-existent');

	$rc = system('git', "--git-dir=$test->{git_dir}", 'tag', '-a',
			'-m', 'annotated tag!', 'v1.0.0');
	is($rc, 0, 'created annotated 1.0.0 tag');
	$req = 'http://example.com/test.git/snapshot/test-1.0.0.tar.gz';
	$res = $cb->(GET($req));
	is($res->code, 200, 'got 200 response for tag');
	is($res->header('Content-Type'), 'application/x-gzip',
		'Content-Type is as expected');
	is($res->header('Content-Disposition'),
		'inline; filename="test-1.0.0.tar.gz"',
		'Content-Disposition is as expected');

	$rc = system('git', "--git-dir=$test->{git_dir}", 'tag',
			'-m', 'lightweight tag!', 'v2.0.0');
	is($rc, 0, 'created lightweight 2.0.0 tag');
	$req = 'http://example.com/test.git/snapshot/test-2.0.0.tar.gz';
	$res = $cb->(GET($req));
	is($res->code, 200, 'got 200 response for tag');
	is($res->header('Content-Type'), 'application/x-gzip',
		'Content-Type is as expected');
	is($res->header('Content-Disposition'),
		'inline; filename="test-2.0.0.tar.gz"',
		'Content-Disposition is as expected');
});

done_testing();
