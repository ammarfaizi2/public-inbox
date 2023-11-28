#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12; use PublicInbox::TestCommon;

test_lei(sub {
	ok(!lei('mail-diff', 't/data/0001.patch', 't/data/binary.patch'),
		'different messages are different');
	like($lei_out, qr/^\+/m, 'diff shown');
	unlike $lei_out, qr/No newline at end of file/;
	lei_ok('mail-diff', 't/data/0001.patch', 't/data/0001.patch');
	is($lei_out, '', 'no output if identical');
});

done_testing;
