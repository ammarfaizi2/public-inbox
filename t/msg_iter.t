# Copyright (C) 2016-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict; use v5.10.1; use PublicInbox::TestCommon;
use PublicInbox::Hval qw(ascii_html);
use MIME::QuotedPrint 3.05 qw(encode_qp);
use_ok('PublicInbox::MsgIter');

{
	my $mime = eml_load 't/msg_iter-order.eml';
	my @parts;
	msg_iter($mime, sub {
		my ($part, $level, @ex) = @{$_[0]};
		my $s = $part->body_str;
		$s =~ s/\s+//s;
		push @parts, [ $s, $level, @ex ];
	});
	is_deeply(\@parts, [ [ qw(a 1 1) ], [ qw(b 1 2) ] ], 'order is fine');
}

{
	my $mime = eml_load 't/msg_iter-nested.eml';
	my @parts;
	msg_iter($mime, sub {
		my ($part, $level, @ex) = @{$_[0]};
		my $s = $part->body_str;
		$s =~ s/\s+//s;
		push @parts, [ $s, $level, @ex ];
	});
	is_deeply(\@parts, [ [qw(a 2 1.1)], [qw(b 2 1.2)], [qw(sig 1 2)] ],
		'nested part shows up properly');
}

{
	my $mime = eml_load 't/iso-2202-jp.eml';
	my $raw = '';
	msg_iter($mime, sub {
		my ($part, $level, @ex) = @{$_[0]};
		my ($s, $err) = msg_part_text($part, 'text/plain');
		ok(!$err, 'no error');
		$raw .= $s;
	});
	ok(length($raw) > 0, 'got non-empty message');
	is(index($raw, '$$$'), -1, 'no unescaped $$$');
}

{
	my $mime = eml_load 't/x-unknown-alpine.eml';
	my $raw = '';
	msg_iter($mime, sub {
		my ($part, $level, @ex) = @{$_[0]};
		my ($s, $err) = msg_part_text($part, 'text/plain');
		$raw .= $s;
	});
	like($raw, qr!^\thttps://!ms, 'tab expanded with X-UNKNOWN');
	like(ascii_html($raw), qr/&#8226; bullet point/s,
		'got bullet point when X-UNKNOWN assumes UTF-8');
}

{ # API not finalized
	my @warn;
	local $SIG{__WARN__} = sub { push @warn, [ @_ ] };
	my $attr = "So and so wrote:\n";
	my $q = "> hello world\n" x 10;
	my $nq = "hello world\n" x 10;
	my @sections = PublicInbox::MsgIter::split_quotes($attr . $q . $nq);
	is($sections[0], $attr, 'attribution matches');
	is($sections[1], $q, 'quoted section matches');
	is($sections[2], $nq, 'non-quoted section matches');
	is(scalar(@sections), 3, 'only three sections for short message');
	is_deeply(\@warn, [], 'no warnings');

	$q x= 3300;
	$nq x= 3300;
	@sections = PublicInbox::MsgIter::split_quotes($attr . $q . $nq);
	is_deeply(\@warn, [], 'no warnings on giant message');
	is(join('', @sections), $attr . $q . $nq, 'result matches expected');
	is(shift(@sections), $attr, 'attribution is first section');
	my @check = ('', '');
	while (defined(my $l = shift @sections)) {
		next if $l eq '';
		like($l, qr/\n\z/s, 'section ends with newline');
		my $idx = ($l =~ /\A>/) ? 0 : 1;
		$check[$idx] .= $l;
	}
	is($check[0], $q, 'long quoted section matches');
	is($check[1], $nq, 'long quoted section matches');
}

{
	open my $fh, '<', 't/utf8.eml' or BAIL_OUT $!;
	my $expect = do { local $/; <$fh>  };
	my $qp_patch = encode_qp($expect, "\r\n");
	my $common = <<EOM;
Content-Type: multipart/mixed; boundary="DEADBEEF"
MIME-Version: 1.0

--DEADBEEF
Content-Transfer-Encoding: quoted-printable
Content-Type: text/plain;
	charset=utf-8

blah

--DEADBEEF
Content-Disposition: attachment;
	filename=foo.patch
Content-Type: application/octet-stream;
	x-unix-mode=0644;
	name="foo.patch"
Content-Transfer-Encoding: quoted-printable
EOM
	my $eml = PublicInbox::Eml->new(<<EOM);
$common
$qp_patch
--DEADBEEF--
EOM
	my @parts;
	$eml->each_part(sub {
		my ($part, $level, @ex) = @{$_[0]};
		my ($s, $err) = msg_part_text($part, $part->content_type);
		push @parts, $s;
	});
	$expect =~ s/\n/\r\n/sg;
	utf8::decode($expect); # aka "bytes2str"
	is_deeply(\@parts, [ "blah\r\n", $expect ],
		'fallback to application/octet-stream as UTF-8 text');

	my $qp_binary = encode_qp("Binary\0crap", "\r\n");
	$eml = PublicInbox::Eml->new(<<EOM);
$common
$qp_binary
--DEADBEEF--
EOM
	@parts = ();
	my @err;
	$eml->each_part(sub {
		my ($part, $level, @ex) = @{$_[0]};
		my ($s, $err) = msg_part_text($part, $part->content_type);
		push @parts, $s;
		push @err, $err;
	});
	is_deeply(\@parts, [ "blah\r\n", undef ],
		'non-text ignored in octet-stream');
	ok($err[1], 'got error for second element');
}

done_testing();
