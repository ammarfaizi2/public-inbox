#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use PublicInbox::TestCommon;
require_ok 'PublicInbox::MHreader';
use PublicInbox::IO qw(write_file);
use PublicInbox::Lock;
use PublicInbox::OnDestroy;
use PublicInbox::Eml;
use File::Path qw(remove_tree);
use autodie;
opendir my $cwdfh, '.';

my $normal = create_dir 'normal', sub {
	write_file '>', 3, "Subject: replied a\n\n";
	write_file '>', 4, "Subject: replied b\n\n";
	write_file '>', 1, "Subject: unseen\n\n";
	write_file '>', 2, "Subject: unseen flagged\n\n";
	write_file '>', '.mh_sequences', <<EOM;
unseen: 1 2
flagged: 2
replied: 3 4
EOM
};

my $for_sort = create_dir 'size', sub {
	for (1..3) {
		my $name = 10 - $_;
		write_file '>', $name, "Subject: ".($_ x $_)."\n\n";
	}
};

my $stale = create_dir 'stale', sub {
	write_file '>', 4, "Subject: msg 4\n\n";
	write_file '>', '.mh_sequences', <<EOM;
unseen: 1 2
EOM
};

{
	my $mhr = PublicInbox::MHreader->new("$normal/", $cwdfh);
	$mhr->{sort} = [ '' ];
	my @res;
	$mhr->mh_each_eml(sub { push @res, \@_; }, [ 'bogus' ]);
	is scalar(@res), 4, 'got 4 messages' or diag explain(\@res);
	is_deeply [map { $_->[1] } @res], [1, 2, 3, 4],
		'got messages in expected order';
	is scalar(grep { $_->[4]->[0] eq 'bogus' } @res), scalar(@res),
		'cb arg passed to all messages' or diag explain(\@res);

	$mhr = PublicInbox::MHreader->new("$stale/", $cwdfh);
	@res = ();
	$mhr->mh_each_eml(sub { push @res, \@_; });
	is scalar(@res), 1, 'ignored stale messages';
}

test_lei(sub {
	lei_ok qw(convert -f mboxrd), $normal;
	my @msgs = grep /\S/s, split /^From .[^\n]+\n/sm, $lei_out;
	my @eml = map { PublicInbox::Eml->new($_) } @msgs;
	my $h = 'Subject';
	@eml = sort { $a->header_raw($h) cmp $b->header_raw($h) } @eml;
	my @has = map { scalar $_->header_raw($h) } @eml;
	is_xdeeply \@has,
		[ 'replied a', 'replied b', 'unseen', 'unseen flagged' ],
		'subjects sorted';
	$h = 'X-Status';
	@has = map { scalar $_->header_raw($h) } @eml;
	is_xdeeply \@has, [ 'A', 'A', undef, 'F' ], 'answered and flagged kw';
	$h = 'Status';
	@has = map { scalar $_->header_raw($h) } @eml;
	is_xdeeply \@has, ['RO', 'RO', 'O', 'O'], 'read and old';
	lei_ok qw(import +L:normal), $normal;
	lei_ok qw(q L:normal -f mboxrd);
	@msgs = grep /\S/s, split /^From .[^\n]+\n/sm, $lei_out;
	my @eml2 = map { PublicInbox::Eml->new($_) } @msgs;
	$h = 'Subject';
	@eml2 = sort { $a->header_raw($h) cmp $b->header_raw($h) } @eml2;
	is_xdeeply \@eml2, \@eml, 'import preserved kw';

	lei_ok 'ls-mail-sync';
	is $lei_out, 'mh:'.File::Spec->rel2abs($normal)."\n",
		'mail sync stored';

	lei_ok qw(convert -s size -f mboxrd), "mh:$for_sort";
	chomp(my @s = grep /^Subject:/, split(/^/sm, $lei_out));
	s/^Subject: // for @s;
	is_xdeeply \@s, [ 1, 22, 333 ], 'sorted by size';

	for my $s ([], [ 'name' ], [ 'sequence' ]) {
		lei_ok qw(convert -f mboxrd), "mh:$for_sort", '-s', @$s;
		chomp(@s = grep /^Subject:/, split(/^/sm, $lei_out));
		s/^Subject: // for @s;
		my $desc = "@$s" || '(default)';
		is_xdeeply \@s, [ 333, 22, 1 ], "sorted by: $desc";
	}

	lei_ok qw(import +L:sorttest), "MH:$for_sort";
	lei_ok 'ls-mail-sync', $for_sort;
	is $lei_out, 'mh:'.File::Spec->rel2abs($for_sort)."\n",
		"mail sync stored with `MH' normalized to `mh'";
	lei_ok qw(index), 'mh:'.$stale;
	lei qw(q -f mboxrd), 's:msg 4';
	like $lei_out, qr/^Subject: msg 4\nStatus: RO\n\n\n/ms,
		"message retrieved after `lei index'";

	lei_ok qw(convert -s none -f text), "mh:$for_sort", \'--sort=none';

	# ensure sort works for _input_ when output disallows sort
	my $v2out = "$ENV{HOME}/v2-out";
	for my $sort (['--sort=sequence'], []) { # sequence is the default
		lei_ok qw(convert), @$sort, "mh:$for_sort", '-o', "v2:$v2out";
		my $g = PublicInbox::Git->new("$v2out/git/0.git");
		chomp(my @l = $g->qx(qw(log --pretty=oneline --format=%s)));
		is_xdeeply \@l, [1, 22, 333], 'sequence order preserved for v2';
		File::Path::remove_tree $v2out;
	}
});

done_testing;
