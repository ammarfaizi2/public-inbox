# Copyright (C) 2013-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use Test::More;
use PublicInbox::TestCommon;
require_mods('Plack::Util');
use_ok 'PublicInbox::View';

# this only tests View.pm internals which are subject to change,
# see t/plack.t for tests against the PSGI interface.

my $cols = PublicInbox::View::COLS();
my @addr;
until (length(join(', ', @addr)) > ($cols * 2)) {
	push @addr, '"l, f" <a@a>';
	my $n = int(rand(20)) + 1;
	push @addr, ('x'x$n).'@x';
}
my $orig = join(', ', @addr);
my $res = PublicInbox::View::fold_addresses($orig.'');
isnt($res, $orig, 'folded result');
unlike($res, qr/l,\n\tf/s, '"last, first" no broken');
my @nospc = ($res, $orig);
s/\s+//g for @nospc;
is($nospc[0], $nospc[1], 'no addresses lost in translation');
my $tws = PublicInbox::View::fold_addresses($orig.' ');
# (Email::Simple drops leading whitespace, but not trailing)
$tws =~ s/ \z//;
is($tws, $res, 'not thrown off by trailing whitespace');

# diffstat anchors are only made for git diffs, and must not leak from one
# message to the next: a non-git diff after a git one used to link its
# diffstat to anchors which were never created
{
	require PublicInbox::Eml;
	my $git = PublicInbox::Eml->new(<<'EOM');
From: a@example.com
Subject: [PATCH 1/2] add foo
Message-ID: <git@example.com>

---
 foo | 1 +
 1 file changed, 1 insertion(+)

diff --git a/foo b/foo
index 0000000..257cc56 100644
--- a/foo
+++ b/foo
@@ -0,0 +1 @@
+foo
EOM
	my $quilt = <<'EOM';
From: a@example.com
Subject: [PATCH 2/2] add bar
Message-ID: <quilt@example.com>

---
 bar | 1 +
 1 file changed, 1 insertion(+)

--- a/bar
+++ b/bar
@@ -0,0 +1 @@
+bar
EOM
	my $render = sub {
		my ($ctx, $eml, $mhref) = @_;
		$ctx->{mhref} = $mhref;
		$ctx->{changed_href} = '#e1';
		my $beg = length($ctx->{pbuf} // '');
		$eml->each_part(\&PublicInbox::View::add_text_body, $ctx, 1);
		close(delete $ctx->{zfh});
		substr($ctx->{pbuf}, $beg);
	};
	my $new_ctx = sub {
		bless { ibx => { name => 'test' } }, 'PublicInbox::WwwStream'
	};
	my $alone = $render->($new_ctx->(), PublicInbox::Eml->new($quilt),
				'../quilt/');
	my $ctx = $new_ctx->();
	my $git_html = $render->($ctx, $git, '../git/');
	like($git_html, qr/href=#[^>]+>foo</, 'git diffstat is linked');
	my $after = $render->($ctx, PublicInbox::Eml->new($quilt),
				'../quilt/');
	is($after, $alone, 'non-git diff renders the same after a git diff');
	my %id = map { $_ => 1 } ($after =~ /\bid=([^\s>]+)/g);
	my @dangling = grep { !$id{$_} } ($after =~ /href=#([^\s>"]+)/g);
	is_deeply(\@dangling, [], 'no links to missing anchors');
}

done_testing();
