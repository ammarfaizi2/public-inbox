# Copyright (C) 2017-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use_ok 'PublicInbox::Hval', qw(to_attr ascii_html);

# ascii_html must escape every byte %xhtml_map covers, and must not
# double-escape the `&' it introduces itself.  \x09 and \x0a map to
# themselves, everything else in \x00-\x1f and \x7f changes.
is(ascii_html(''), '', 'empty string');
is(ascii_html('plain ascii text'), 'plain ascii text', 'no-op on plain text');
is(ascii_html(q{<>&'"}), '&lt;&gt;&#38;&#39;&#34;', 'HTML metacharacters');
is(ascii_html('&amp;'), '&#38;amp;', '`&\' escaped exactly once');
is(ascii_html("<script>alert('x')</script>"),
	'&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt;', 'script tag');
is(ascii_html("a\tb\nc"), "a\tb\nc", 'tab and LF pass through');
is(ascii_html("a\r\nb"), "a\\r\nb", 'CR escaped, LF kept');
is(ascii_html("\0\x07\x08\x0b\x0c\x1b\x7f"), '\\0\\a\\b\\v\\f^[\\x7f',
	'C escape sequences');
is(ascii_html("\x01\x02\x1f"), '\\x01\\x02\\x1f', 'generic control chars');
is(ascii_html(join('', map(chr, 0..31))),
	'\\0\\x01\\x02\\x03\\x04\\x05\\x06\\a\\b'."\t\n".
		'\\v\\f\\r\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16'.
		'\\x17\\x18\\x19\\x1a^[\\x1c\\x1d\\x1e\\x1f',
	'all of \x00-\x1f');
is(ascii_html("caf\xc3\xa9"), 'caf&#195;&#169;', 'UTF-8 bytes -> HTMLCREF');
is(ascii_html("caf\x{e9} \x{2713}"), 'caf&#233; &#10003;',
	'wide chars -> HTMLCREF');
is(ascii_html("mixed \x01<b>\t' \" &\x7f"),
	'mixed \\x01&lt;b&gt;'."\t".'&#39; &#34; &#38;\\x7f', 'mixed input');

# reverse the result of to_attr
sub from_attr ($) {
	my ($str) = @_;
	my $first = '';
	if ($str =~ s/\AZ([a-f0-9]{2})//ms) {
		$first = chr(hex($1));
	}
	$str =~ s!::([a-f0-9]{2})!chr(hex($1))!egms;
	$str =~ tr!:!/!;
	utf8::decode($str);
	$first . $str;
}

my $ibx = {
	-no_obfuscate_re => qr/(?:example\.com)\z/i,
	-no_obfuscate => {
		'meta@public-inbox.org' => 1,
	}
};

my $html = <<'EOF';
hello@example.comm
hello@example.com
meta@public-inbox.org
test@public-inbox.org
test@a.b.c.org
te.st@example.org
EOF

PublicInbox::Hval::obfuscate_addrs($ibx, $html);

my $exp = <<'EOF';
hello@example&#8226;comm
hello@example.com
meta@public-inbox.org
test@public-inbox&#8226;org
test@a&#8226;b.c.org
te.st@example&#8226;org
EOF

is($html, $exp, 'only obfuscated relevant addresses');

$exp = 'https://example.net/foo@example.net';
PublicInbox::Hval::obfuscate_addrs($ibx, my $res = $exp);
is($res, $exp, 'does not obfuscate URL with Message-ID');

is(PublicInbox::Hval::to_filename('foo bar  '), 'foo-bar',
	'to_filename has no trailing -');

is(PublicInbox::Hval::to_filename("foo   bar\nanother line\n"), 'foo-bar',
	'to_filename has no repeated -, and nothing past LF');

is(PublicInbox::Hval::to_filename("foo....bar"), 'foo.bar',
	'to_filename squeezes -');

is(PublicInbox::Hval::to_filename(''), undef, 'empty string returns undef');

my $s = "\0\x07\n";
PublicInbox::Hval::src_escape($s);
is($s, "\\0\\a\n", 'src_escape works as intended');

foreach my $s ('Hello/World.pm', 'Zcat', 'hello world.c', 'Eléanor', '$at') {
	my $attr = to_attr($s);
	is(from_attr($attr), $s, "$s => $attr => $s round trips");
}

{
	my $bad = to_attr('foo//bar');
	ok(!$bad, 'double-slash rejected');
}

done_testing();
