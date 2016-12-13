# Copyright (C) 2014-2015 all contributors <meta@public-inbox.org>
# License: AGPLv3 or later (https://www.gnu.org/licenses/agpl-3.0.txt)
#
# represents a header value in various forms.  Used for HTML generation
# in our web interface(s)
package PublicInbox::Hval;
use strict;
use warnings;
use Encode qw(find_encoding);
use PublicInbox::MID qw/mid_clean mid_escape/;
use URI::Escape qw(uri_escape_utf8);
use base qw/Exporter/;
our @EXPORT_OK = qw/ascii_html utf8_html to_attr from_attr/;

# for user-generated content (UGC) which may have excessively long lines
# and screw up rendering on some browsers.  This is the only CSS style
# feature we use.
use constant STYLE => '<style>pre{white-space:pre-wrap}</style>';

my $enc_utf8 = find_encoding('UTF-8');
my $enc_ascii = find_encoding('us-ascii');

sub utf8 {
	my ($class, $raw, $href) = @_;

	$raw = $enc_utf8->decode($raw);
	bless {
		raw => $raw,
		href => defined $href ? $href : $raw,
	}, $class;
}

sub new {
	my ($class, $raw, $href) = @_;

	# we never care about trailing whitespace
	$raw =~ s/\s*\z//;
	bless {
		raw => $raw,
		href => defined $href ? $href : $raw,
	}, $class;
}

sub new_msgid {
	my ($class, $msgid) = @_;
	$class->new($msgid, mid_escape($msgid));
}

sub new_oneline {
	my ($class, $raw) = @_;
	$raw = '' unless defined $raw;
	$raw =~ tr/\t\n / /s; # squeeze spaces
	$raw =~ tr/\r//d; # kill CR
	$class->new($raw);
}

my %xhtml_map = (
	'"' => '&#34;',
	'&' => '&#38;',
	"'" => '&#39;',
	'<' => '&lt;',
	'>' => '&gt;',
);

$xhtml_map{chr($_)} = sprintf('\\x%02x', $_) for (0..31);
# some of these overrides are standard C escapes so they're
# easy-to-understand when rendered.
$xhtml_map{"\x00"} = '\\0'; # NUL
$xhtml_map{"\x07"} = '\\a'; # bell
$xhtml_map{"\x08"} = '\\b'; # backspace
$xhtml_map{"\x09"} = "\t"; # obvious to show as-is
$xhtml_map{"\x0a"} = "\n"; # obvious to show as-is
$xhtml_map{"\x0b"} = '\\v'; # vertical tab
$xhtml_map{"\x0c"} = '\\f'; # form feed
$xhtml_map{"\x0d"} = '\\r'; # carriage ret (not preceding \n)
$xhtml_map{"\x1b"} = '^['; # ASCII escape (mutt seems to escape this way)
$xhtml_map{"\x7f"} = '\\x7f'; # DEL

sub ascii_html {
	my ($s) = @_;
	$s =~ s/\r\n/\n/sg; # fixup bad line endings
	$s =~ s/([<>&'"\x7f\x00-\x1f])/$xhtml_map{$1}/sge;
	$enc_ascii->encode($s, Encode::HTMLCREF);
}

sub utf8_html {
	my ($raw) = @_;
	ascii_html($enc_utf8->decode($raw));
}

sub as_html { ascii_html($_[0]->{raw}) }
sub as_href { ascii_html(uri_escape_utf8($_[0]->{href})) }

sub as_path {
	my $p = uri_escape_utf8($_[0]->{href});
	$p =~ s!%2[fF]!/!g;
	ascii_html($p);
}

sub raw {
	if (defined $_[1]) {
		$_[0]->{raw} = $_[1];
	} else {
		$_[0]->{raw};
	}
}

sub prurl {
	my ($env, $u) = @_;
	index($u, '//') == 0 ? "$env->{'psgi.url_scheme'}:$u" : $u;
}

# convert a filename (or any string) to HTML attribute

my %ESCAPES = map { chr($_) => sprintf('::%02x', $_) } (0..255);
$ESCAPES{'/'} = ':'; # common

sub to_attr ($) {
	my ($str) = @_;

	# git would never do this to us:
	die "invalid filename: $str" if index($str, '//') >= 0;

	my $first = '';
	if ($str =~ s/\A([^A-Ya-z])//ms) { # start with a letter
		  $first = sprintf('Z%02x', ord($1));
	}
	$str =~ s/([^A-Za-z0-9_\.\-])/$ESCAPES{$1}/egms;
	$first . $str;
}

# reverse the result of to_attr
sub from_attr ($) {
	my ($str) = @_;
	my $first = '';
	if ($str =~ s/\AZ([a-f0-9]{2})//ms) {
		$first = chr(hex($1));
	}
	$str =~ s!::([a-f0-9]{2})!chr(hex($1))!egms;
	$str =~ tr!:!/!;
	$first . $str;
}

1;
