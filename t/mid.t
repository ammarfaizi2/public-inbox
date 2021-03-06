# Copyright (C) 2016-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use Test::More;
use PublicInbox::Eml;
use PublicInbox::MID qw(mid_escape mids references mids_for_index id_compress);

is(mid_escape('foo!@(bar)'), 'foo!@(bar)');
is(mid_escape('foo%!@(bar)'), 'foo%25!@(bar)');
is(mid_escape('foo%!@(bar)'), 'foo%25!@(bar)');

# n.b: this is probably invalid since we dropped CGI for PSGI:
like(id_compress('foo%bar@wtf'), qr/\A[a-f0-9]{40}\z/,
	"percent always converted to sha1 to workaround buggy httpds");

is(id_compress('foobar-wtf'), 'foobar-wtf', 'regular ID not compressed');

{
	my $mime = PublicInbox::Eml->new("Message-ID: <mid-1\@a>\n\n");
	$mime->header_set('X-Alt-Message-ID', '<alt-id-for-nntp>');
	is_deeply(['mid-1@a'], mids($mime->header_obj), 'mids in common case');
	$mime->header_set('Message-Id', '<mid-1@a>', '<mid-2@b>');
	is_deeply(['mid-1@a', 'mid-2@b'], mids($mime->header_obj), '2 mids');
	$mime->header_set('Message-Id', '<mid-1@a>', '<mid-1@a>');
	is_deeply(['mid-1@a'], mids($mime->header_obj), 'dup mids');
	$mime->header_set('Message-Id', '<mid-1@a> comment');
	is_deeply(['mid-1@a'], mids($mime->header_obj), 'comment ignored');
	$mime->header_set('Message-Id', 'bare-mid');
	is_deeply(['bare-mid'], mids($mime->header_obj), 'bare mid OK');

	$mime->header_set('References', '<hello> <world>');
	$mime->header_set('In-Reply-To', '<weld>');
	is_deeply(['hello', 'world', 'weld'], references($mime->header_obj),
		'references combines with In-Reply-To');

	$mime->header_set('References', "<hello>\n\t<world>");
	$mime->header_set('In-Reply-To');
	is_deeply(references($mime->header_obj), ['hello', 'world'],
		'multiline References OK');
	$mime->header_set('References', "<hello\tworld>");
	is_deeply(references($mime->header_obj), ['helloworld'],
		'drop \t in References <656C30A1EFC89F6B2082D9B6@localhost>');
	$mime->header_set('Message-ID', "<hello\tworld>");
	is_deeply(mids($mime->header_obj), ['helloworld'],
		'drop \t in Message-ID');

	$mime->header_set('To', 'u@example.com');
	$mime->header_set('References', '<hello> <world> <n> <u@example.com>');
	is_deeply(references($mime->header_obj), [qw(hello world)]);

	is_deeply([qw(helloworld alt-id-for-nntp)],
		mids_for_index($mime->header_obj),
		'X-Alt-Message-ID can be indexed');
}

done_testing();
1;
