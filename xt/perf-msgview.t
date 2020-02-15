# Copyright (C) 2019-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use Benchmark qw(:all);
use PublicInbox::Inbox;
use PublicInbox::View;
use PublicInbox::TestCommon;

my $inboxdir = $ENV{GIANT_INBOX_DIR} // $ENV{GIANT_PI_DIR};
plan skip_all => "GIANT_INBOX_DIR not defined for $0" unless $inboxdir;

my @cat = qw(cat-file --buffer --batch-check --batch-all-objects);
if (require_git(2.19, 1)) {
	push @cat, '--unordered';
} else {
	warn
"git <2.19, cat-file lacks --unordered, locality suffers\n";
}
require_mods qw(Plack::Util);
use_ok 'Plack::Util';
my $ibx = PublicInbox::Inbox->new({ inboxdir => $inboxdir, name => 'name' });
my $git = $ibx->git;
my $fh = $git->popen(@cat);
my $vec = '';
vec($vec, fileno($fh), 1) = 1;
select($vec, undef, undef, 60) or die "timed out waiting for --batch-check";

my $ctx = {
	env => { HTTP_HOST => 'example.com', 'psgi.url_scheme' => 'https' },
	-inbox => $ibx,
	www => Plack::Util::inline_object(style => sub {''}),
};
my ($str, $mime, $res, $cmt, $type);
my $n = 0;
my $t = timeit(1, sub {
	my $obuf = '';
	$ctx->{obuf} = \$obuf;
	$ctx->{mhref} = '../';
	while (<$fh>) {
		($cmt, $type) = split / /;
		next if $type ne 'blob';
		++$n;
		$str = $git->cat_file($cmt);
		$mime = PublicInbox::MIME->new($str);
		PublicInbox::View::multipart_text_as_html($mime, $ctx);
		$obuf = '';
	}
});
diag 'multipart_text_as_html took '.timestr($t)." for $n messages";
ok 1;
done_testing();
