# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12;
use PublicInbox::TestCommon;
use PublicInbox::Eml;
require_mods(qw(DBD::SQLite));
use_ok 'PublicInbox::NNTP';
use PublicInbox::Config;
use POSIX qw(strftime);

{
	my $wm_prepare = sub {
		my ($wm) = @_;
		my $orig = qq{'$wm'};
		my $re = PublicInbox::NNTP::wildmat2re($wm);
		my $new = explain($re);
		($orig, $new, $re);
	};

	my $wildmat_like = sub {
		my ($str, $wm) = @_;
		my ($orig, $new, $re) = $wm_prepare->($wm);
		like($str, $re, "$orig matches '$str' using $new");
	};

	my $wildmat_unlike = sub {
		my ($str, $wm, $check_ex) = @_;
		if ($check_ex) {
			use re 'eval';
			my $re = qr/$wm/;
			like($str, $re, "normal re with $wm matches, but ...");
		}
		my ($orig, $new, $re) = $wm_prepare->($wm);
		unlike($str, $re, "$orig does not match '$str' using $new");
	};

	$wildmat_like->('[foo]', '[\[foo\]]');
	$wildmat_like->('any', '*');
	$wildmat_unlike->('bar.foo.bar', 'foo.*');

	# no code execution
	$wildmat_unlike->('HI', '(?{"HI"})', 1);
	$wildmat_unlike->('HI', '[(?{"HI"})]', 1);
}

{
	my $ngpat_like = sub {
		my ($str, $pat) = @_;
		my $orig = $pat;
		my $re = PublicInbox::NNTP::ngpat2re($pat);
		like($str, $re, "'$orig' matches '$str' using $re");
	};

	$ngpat_like->('any', '*');
	$ngpat_like->('a.s.r', 'a.t,a.s.r');
	$ngpat_like->('a.s.r', 'a.t,a.s.*');
}

{
	my $time_roundtrip = sub {
		my ($date, $time, $gmt) = @_;
		my $m = join(' ', @_);
		my $ts = PublicInbox::NNTP::parse_time(@_);
		my @t = $gmt ? gmtime($ts) : localtime($ts);
		my ($d, $t) = split(' ', strftime('%Y%m%d %H%M%S', @t));
		if (length($date) != 8) { # Net::NNTP uses YYMMDD :<
			$d =~ s/^[0-9]{2}//;
		}
		is_deeply([$d, $t], [$date, $time], "roundtripped: $m");
		$ts;
	};
	my $x1 = $time_roundtrip->(qw(20141109 060606 GMT));
	my $x2 = $time_roundtrip->(qw(141109 060606 GMT));
	my $x3 = $time_roundtrip->(qw(930724 060606 GMT));
	my $x5 = $time_roundtrip->(qw(710101 000000));
	my $x6 = $time_roundtrip->(qw(720101 000000));
	SKIP: {
		skip('YYMMDD test needs updating', 6) if (time > 0x7fffffff);
		# our world probably ends in 2038, but if not we'll try to
		# remember to update the test then
		is($x1, $x2, 'YYYYMMDD and YYMMDD parse identically');
		is(strftime('%Y', gmtime($x3)), '1993', '930724 was in 1993');

		my $epoch = $time_roundtrip->(qw(700101 000000 GMT));
		is($epoch, 0, 'epoch parsed correctly');
		ok($x6 > $x5, '1972 > 1971');
		ok($x5 > $epoch, '1971 > Unix epoch');
	}
}

{ # test setting NNTP headers in HEAD and ARTICLE requests
	my $u = 'https://example.com/a/';
	my $ibx = PublicInbox::Inbox->new({ name => 'test',
					inboxdir => 'test.git',
					address => 'a@example.com',
					-primary_address => 'a@example.com',
					newsgroup => 'test',
					domain => 'example.com',
					url => [ '//example.com/a' ]});
	is($ibx->base_url, $u, 'URL expanded');
	my $mid = 'a@b';
	my $mime = PublicInbox::Eml->new("Message-ID: <$mid>\r\n\r\n");
	my $hdr = $mime->header_obj;
	my $mock_self = {
		nntpd => {
			servername => 'example.com',
			pi_cfg => bless {}, 'PublicInbox::Config',
		},
		ibx => $ibx,
	};
	my $smsg = { num => 1, mid => $mid, nntp => $mock_self, -ibx => $ibx };
	PublicInbox::NNTP::set_nntp_headers($hdr, $smsg);
	is_deeply([ $mime->header('Message-ID') ], [ "<$mid>" ],
		'Message-ID unchanged');
	is_deeply([ $mime->header('Newsgroups') ], [ 'test' ],
		'Newsgroups: set');
	is_deeply([ $mime->header('Xref') ], [ 'example.com test:1' ],
		'Xref: set');

	$smsg->{num} = 2;
	PublicInbox::NNTP::set_nntp_headers($hdr, $smsg);
	is_deeply([ $mime->header('Message-ID') ], [ "<$mid>" ],
		'Message-ID unchanged');
	is_deeply([ $mime->header('Xref') ], [ 'example.com test:2' ],
		'Old Xref: clobbered');
}

done_testing();
