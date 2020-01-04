use 5.010_001;
#	hdr_lines => string of various header lines for mode information
	my ($self, $diffs, $pre, $post, $ibx, $smsg) = @$arg;
	state $LF = qr!\r?\n!;
	state $FN = qr!(?:("?[^/\n]+/[^\r\n]+)|/dev/null)!;

	$s =~ m!( # $1 start header lines we save for debugging:

		# everything before ^index is optional, but we don't
		# want to match ^(old|copy|rename|deleted|...) unless
		# we match /^diff --git/ first:
		(?: # begin optional stuff:

		# try to get the pre-and-post filenames as $2 and $3
		(?:^diff\x20--git\x20$FN\x20$FN$LF)

		# old mode $4
		(?:^old mode\x20(100644|120000|100755)$LF)?

		# ignore other info
		(?:^(?:copy|rename|deleted|dissimilarity|similarity).*$LF)?

		# new mode (possibly new file) ($5)
		(?:^new\x20(?:file\x20)?mode\x20(100644|120000|100755)$LF)?

		# ignore other info
		(?:^(?:copy|rename|deleted|dissimilarity|similarity).*$LF)?

		)? # end of optional stuff, everything below is required

		# match the pre and post-image OIDs as $6 $7
		^index\x20(${pre}[a-f0-9]*)\.\.(${post}[a-f0-9]*)
			# mode if unchanged $8
			(?:\x20(100644|120000|100755))?$LF
	) # end of header lines ($1)
	( # $9 is the patch body
		# "--- a/foo.c" sets pre-filename ($10) in case
		# $2 is missing
		(?:^---\x20$FN$LF)

		# "+++ b/foo.c" sets post-filename ($11) in case
		# $3 is missing
		(?:^\+{3}\x20$FN$LF)

		# the meat of the diff, including "^\\No newline ..."
		# We also allow for totally blank lines w/o leading spaces,
		# because git-apply(1) handles that case, too
		(?:^(?:[\@\+\x20\-\\][^\r\n]*|)$LF)+
	)!smx or return;

	my $hdr_lines = $1;
	my $path_a = $2 // $10;
	my $path_b = $3 // $11;
	$di->{oid_a} = $6;
	$di->{oid_b} = $7;
	$di->{mode_a} = $5 // $8 // $4; # new (file) // unchanged // old
	my $patch = $9;

	# don't care for leading 'a/' and 'b/'
	my (undef, @a) = split(m{/}, git_unquote($path_a));
	my (undef, @b) = split(m{/}, git_unquote($path_b));

	# get rid of path-traversal attempts and junk patches:
	# it's junk at best, an attack attempt at worse:
	state $bad_component = { map { $_ => 1 } ('', '.', '..') };
	foreach (@a, @b) { return if $bad_component->{$_} }

	$di->{path_a} = join('/', @a);
	$di->{path_b} = join('/', @b);

	utf8::encode($hdr_lines);
	utf8::encode($patch);
	my $path = ++$self->{tot};
	$di->{n} = $path;
	open(my $tmp, '>', $self->{tmp}->dirname . "/$path") or
		die "open(tmp): $!";
	print $tmp $hdr_lines, $patch or die "print(tmp): $!";

	# for debugging/diagnostics:
	$di->{ibx} = $ibx;
	$di->{smsg} = $smsg;
	$di->{hdr_lines} = $hdr_lines;


				[$self, $diffs, $pre, $post, $ibx, $smsg]);
	my $mode_a = $di->{mode_a} // '100644';
			return do_git_apply($self);
	my @cmd = (qw(git apply --cached --ignore-whitespace
			"\n" . $di->{hdr_lines});
	my $opt = { 2 => 1, -C => $dn, quiet => 1 };
	my $qsp = PublicInbox::Qspawn->new(\@cmd, $self->{git_env}, $opt);