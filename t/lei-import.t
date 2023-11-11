#!perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use v5.12; use PublicInbox::TestCommon;
use PublicInbox::DS qw(now);
use PublicInbox::IO qw(write_file);
use autodie qw(open close truncate);
test_lei(sub {
ok(!lei(qw(import -F bogus), 't/plack-qp.eml'), 'fails with bogus format');
like($lei_err, qr/\bis `eml', not --in-format/, 'gave error message');

lei_ok(qw(q s:boolean), \'search miss before import');
unlike($lei_out, qr/boolean/i, 'no results, yet');
open my $fh, '<', 't/data/0001.patch';
lei_ok([qw(import -F eml -)], undef, { %$lei_opt, 0 => $fh },
	\'import single file from stdin') or diag $lei_err;
close $fh;
lei_ok(qw(q s:boolean), \'search hit after import');
lei_ok(qw(q s:boolean -f mboxrd), \'blob accessible after import');
{
	my $expect = [ eml_load('t/data/0001.patch') ];
	require PublicInbox::MboxReader;
	my @cmp;
	open my $fh, '<', \$lei_out;
	PublicInbox::MboxReader->mboxrd($fh, sub {
		my ($eml) = @_;
		$eml->header_set('Status');
		push @cmp, $eml;
	});
	is_deeply(\@cmp, $expect, 'got expected message in mboxrd');
}
lei_ok(qw(import -F eml), 't/data/message_embed.eml',
	\'import single file by path');

lei_ok(qw(q m:testmessage@example.com));
is($lei_out, "[null]\n", 'no results, yet');
my $oid = '9bf1002c49eb075df47247b74d69bcd555e23422';
my $eml = eml_load('t/utf8.eml');
my $in = 'From x@y Fri Oct  2 00:00:00 1993'."\n".$eml->as_string;
lei_ok([qw(import -F eml -)], undef, { %$lei_opt, 0 => \$in });
lei_ok(qw(q m:testmessage@example.com));
is(json_utf8->decode($lei_out)->[0]->{'blob'}, $oid,
	'got expected OID w/o From');

my $eml_str = <<'';
From: a@b
Message-ID: <x@y>
Status: RO

my $opt = { %$lei_opt, 0 => \$eml_str };
lei_ok([qw(import -F eml -)], undef, $opt,
	\'import single file with keywords from stdin');
lei_ok(qw(q m:x@y));
my $res = json_utf8->decode($lei_out);
is($res->[1], undef, 'only one result');
is($res->[0]->{'m'}, 'x@y', 'got expected message');
is($res->[0]->{kw}, undef, 'Status ignored for eml');
lei_ok(qw(q -f mboxrd m:x@y));
unlike($lei_out, qr/^Status:/, 'no Status: in imported message');
lei_ok('blob', $res->[0]->{blob});
is($lei_out, "From: a\@b\nMessage-ID: <x\@y>\n", 'got blob back');


$eml->header_set('Message-ID', '<v@y>');
$eml->header_set('Status', 'RO');
$in = 'From v@y Fri Oct  2 00:00:00 1993'."\n".$eml->as_string;
lei_ok([qw(import --no-kw -F mboxrd -)], undef, { %$lei_opt, 0 => \$in },
	\'import single file with --no-kw from stdin');
lei(qw(q m:v@y));
$res = json_utf8->decode($lei_out);
is($res->[1], undef, 'only one result');
is($res->[0]->{'m'}, 'v@y', 'got expected message');
is($res->[0]->{kw}, undef, 'no keywords set');

$eml->header_set('Message-ID', '<k@y>');
$in = 'From k@y Fri Oct  2 00:00:00 1993'."\n".$eml->as_string;
lei_ok([qw(import -F mboxrd /dev/fd/0)], undef, { %$lei_opt, 0 => \$in },
	\'import single file with --kw (default) from stdin');
lei(qw(q m:k@y));
$res = json_utf8->decode($lei_out);
is($res->[1], undef, 'only one result');
is($res->[0]->{'m'}, 'k@y', 'got expected message');
is_deeply($res->[0]->{kw}, ['seen'], "`seen' keywords set");

# no From, Sender, or Message-ID
$eml_str = <<'EOM';
Subject: draft message with no sender
References: <y@y>
Resent-Message-ID: <resent-test@example>

No use for a name
EOM
lei_ok([qw(import -F eml -)], undef, { %$lei_opt, 0 => \$eml_str });
lei_ok(['q', 's:draft message with no sender']);
my $draft_a = json_utf8->decode($lei_out);
ok(!exists $draft_a->[0]->{'m'}, 'no fake mid stored or exposed');
lei_ok([qw(tag -F eml - +kw:draft)], undef, { %$lei_opt, 0 => \$eml_str });
lei_ok(['q', 's:draft message with no sender']);
my $draft_b = json_utf8->decode($lei_out);
my $kw = delete $draft_b->[0]->{kw};
is_deeply($kw, ['draft'], 'draft kw set');
is_deeply($draft_a, $draft_b, 'fake Message-ID lookup') or
				diag explain($draft_a, $draft_b);
lei_ok('blob', '--mail', $draft_b->[0]->{blob});
is($lei_out, $eml_str, 'draft retrieved by blob');


$eml_str = "Message-ID: <inbox\@example.com>\nSubject: label-this\n\n";
lei_ok([qw(import -F eml - +kw:seen +L:inbox)],
	undef, { %$lei_opt, 0 => \$eml_str });
lei_ok(qw(q m:inbox@example.com));
$res = json_utf8->decode($lei_out);
is_deeply($res->[0]->{kw}, ['seen'], 'keyword set');
is_deeply($res->[0]->{L}, ['inbox'], 'label set');

# idempotent import can add label
lei_ok([qw(import -F eml - +L:boombox)],
	undef, { %$lei_opt, 0 => \$eml_str });
lei_ok(qw(q m:inbox@example.com));
$res = json_utf8->decode($lei_out);
is_deeply($res->[0]->{kw}, ['seen'], 'keyword remains set');
is_deeply($res->[0]->{L}, [qw(boombox inbox)], 'new label added');

# idempotent import can add keyword
lei_ok([qw(import -F eml - +kw:answered)],
	undef, { %$lei_opt, 0 => \$eml_str });
lei_ok(qw(q m:inbox@example.com));
$res = json_utf8->decode($lei_out);
is_deeply($res->[0]->{kw}, [qw(answered seen)], 'keyword added');
is_deeply($res->[0]->{L}, [qw(boombox inbox)], 'labels preserved');

# +kw:seen is not a location
open my $null, '<', '/dev/null';
ok(!lei([qw(import -F eml +kw:seen)], undef, { %$lei_opt, 0 => $null }),
	'import fails w/ only kw arg');
like($lei_err, qr/\bLOCATION\.\.\. or --stdin must be set/s, 'error message');

lei_ok([qw(import -F eml +kw:flagged)], # no lone dash (`-')
	undef, { %$lei_opt, 0 => \$eml_str },
	'import succeeds with implicit --stdin');
lei_ok(qw(q m:inbox@example.com));
$res = json_utf8->decode($lei_out);
is_deeply($res->[0]->{kw}, [qw(answered flagged seen)], 'keyword added');
is_deeply($res->[0]->{L}, [qw(boombox inbox)], 'labels preserved');

lei_ok qw(import --commit-delay=1 +L:bin -F eml t/data/binary.patch);
lei_ok 'ls-label';
unlike($lei_out, qr/\bbin\b/, 'commit-delay delays label');
my $end = now + 10;
my $n = 1;
diag 'waiting for lei/store commit...';
do {
	tick $n;
	$n = 0.1;
} until (!lei('ls-label') || $lei_out =~ /\bbin\b/ || now > $end);
like($lei_out, qr/\bbin\b/, 'commit-delay eventually commits');

SKIP: {
	my $strace = strace_inject(1); # skips if strace is old or non-Linux
	my $tmpdir = tmpdir;
	my $tr = "$tmpdir/tr";
	my $cmd = [ $strace, '-q', "-o$tr", '-f',
		"-P", File::Spec->rel2abs('t/plack-qp.eml'),
		'-e', 'inject=readv,read:error=EIO'];
	lei_ok qw(daemon-pid);
	chomp(my $daemon_pid = $lei_out);
	push @$cmd, '-p', $daemon_pid;
	require PublicInbox::Spawn;
	require PublicInbox::AutoReap;
	my $pid = PublicInbox::Spawn::spawn($cmd, \%ENV);
	my $ar = PublicInbox::AutoReap->new($pid);
	tick; # wait for strace to attach
	ok(!lei(qw(import -F eml t/plack-qp.eml)),
		'-F eml import fails on pathname error injection');
	like($lei_err, qr!error reading t/plack-qp\.eml: Input/output error!,
		'EIO noted in stderr');
	open $fh, '<', 't/plack-qp.eml';
	ok(!lei(qw(import -F eml -), undef, { %$lei_opt, 0 => $fh }),
		'-F eml import fails on stdin error injection');
	like($lei_err, qr!error reading .*?: Input/output error!,
		'EIO noted in stderr');
}

{
	local $ENV{PI_CONFIG} = "$ENV{HOME}/pi_config";
	write_file '>', $ENV{PI_CONFIG}, <<EOM;
[publicinboxImport]
	dropUniqueUnsubscribe
EOM
	my $in = <<EOM;
List-Unsubscribe: <https://example.com/some-UUID-here/test>
List-Unsubscribe-Post: List-Unsubscribe=One-Click
Message-ID: <unsubscribe-1\@example>
Subject: unsubscribe-1 example
From: u\@example.com
To: 2\@example.com
Date: Fri, 02 Oct 1993 00:00:00 +0000

EOM
	lei_ok [qw(import -F eml +L:unsub)], undef, { %$lei_opt, 0 => \$in },
		'import succeeds w/ List-Unsubscribe';
	lei_ok qw(q L:unsub -f mboxrd);
	like $lei_out, qr/some-UUID-here/,
		'Unsubscribe header preserved despite PI_CONFIG dropping';
	lei_ok qw(q L:unsub -o), "v2:$ENV{HOME}/v2-1";
	lei_ok qw(q s:unsubscribe -f mboxrd --only), "$ENV{HOME}/v2-1";
	unlike $lei_out, qr/some-UUID-here/,
		'Unsubscribe header dropped w/ dropUniqueUnsubscribe';
	like $lei_out, qr/Message-ID: <unsubscribe-1\@example>/,
		'wrote expected message to v2 output';

	# the default for compatibility:
	truncate $ENV{PI_CONFIG}, 0;
	lei_ok qw(q L:unsub -o), "v2:$ENV{HOME}/v2-2";
	lei_ok qw(q s:unsubscribe -f mboxrd --only), "$ENV{HOME}/v2-2";
	like $lei_out, qr/some-UUID-here/,
		'Unsubscribe header preserved by default :<';

	# ensure we can fail
	write_file '>', $ENV{PI_CONFIG}, <<EOM;
[publicinboxImport]
	dropUniqueUnsubscribe = bogus
EOM
	ok(!lei(qw(q L:unsub -o), "v2:$ENV{HOME}/v2-3"), 'bad config fails');
	like $lei_err, qr/is not boolean/, 'non-booleaness noted in stderr';
	ok !-d "$ENV{HOME}/v2-3", 'v2 directory not created';
}

# see t/lei_to_mail.t for "import -F mbox*"
});
done_testing;
