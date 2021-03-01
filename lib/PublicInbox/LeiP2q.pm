# Copyright (C) 2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# front-end for the "lei patch-to-query" sub-command
package PublicInbox::LeiP2q;
use strict;
use v5.10.1;
use parent qw(PublicInbox::IPC);
use PublicInbox::Eml;
use PublicInbox::Smsg;
use PublicInbox::MsgIter qw(msg_part_text);
use PublicInbox::Git qw(git_unquote);
use PublicInbox::Spawn qw(popen_rd);
use URI::Escape qw(uri_escape_utf8);

sub xphrase ($) {
	my ($s) = @_;
	return () unless $s =~ /\S/;
	# cf. xapian-core/queryparser/queryparser.lemony
	# [\./:\\\@] - is_phrase_generator (implicit phrase search)
	# FIXME not really sure about these..., we basically want to
	# extract the longest phrase possible that Xapian can handle
	map {
		s/\A\s*//;
		s/\s+\z//;
		/[\|=><,\sA-Z]/ && !m![\./:\\\@]! ? qq("$_") : $_;
	} ($s =~ m!(\w[\|=><,\./:\\\@\-\w\s]+)!g);
}

sub extract_terms { # eml->each_part callback
	my ($p, $lei) = @_;
	my $part = $p->[0]; # ignore $depth and @idx;
	my $ct = $part->content_type || 'text/plain';
	my ($s, undef) = msg_part_text($part, $ct);
	defined $s or return;
	my $in_diff;
	# TODO: b: nq: q:
	for (split(/\n/, $s)) {
		if ($in_diff && s/^ //) { # diff context
			push @{$lei->{qterms}->{dfctx}}, xphrase($_);
		} elsif (/^-- $/) { # email signature begins
			$in_diff = undef;
		} elsif (m!^diff --git "?[^/]+/.+ "?[^/]+/.+\z!) {
			# wait until "---" and "+++" to capture filenames
			$in_diff = 1;
		} elsif (/^index ([a-f0-9]+)\.\.([a-f0-9]+)\b/) {
			my ($oa, $ob) = ($1, $2);
			push @{$lei->{qterms}->{dfpre}}, $oa;
			push @{$lei->{qterms}->{dfpost}}, $ob;
			# who uses dfblob?
		} elsif (m!^(?:---|\+{3}) ("?[^/]+/.+)!) {
			my $fn = (split(m!/!, git_unquote($1.''), 2))[1];
			push @{$lei->{qterms}->{dfn}}, xphrase($fn);
		} elsif ($in_diff && s/^\+//) { # diff added
			push @{$lei->{qterms}->{dfb}}, xphrase($_);
		} elsif ($in_diff && s/^-//) { # diff removed
			push @{$lei->{qterms}->{dfa}}, xphrase($_);
		} elsif (/^@@ (?:\S+) (?:\S+) @@\s*(\S+.*)/) {
			push @{$lei->{qterms}->{dfhh}}, xphrase($1);
		} elsif (/^(?:dis)similarity index/ ||
				/^(?:old|new) mode/ ||
				/^(?:deleted|new) file mode/ ||
				/^(?:copy|rename) (?:from|to) / ||
				/^(?:dis)?similarity index / ||
				/^\\ No newline at end of file/ ||
				/^Binary files .* differ/) {
		} elsif ($_ eq '') {
			# possible to be in diff context, some mail may be
			# stripped by MUA or even GNU diff(1).  "git apply"
			# treats a bare "\n" as diff context, too
		} else {
			$in_diff = undef;
		}
	}
}

my %pfx2smsg = (
	t => [ qw(to) ],
	c => [ qw(cc) ],
	f => [ qw(from) ],
	tc => [ qw(to cc) ],
	tcf => [ qw(to cc from) ],
	a => [ qw(to cc from) ],
	s => [ qw(subject) ],
	bs => [ qw(subject) ], # body handled elsewhere
	d => [ qw(ds) ], # nonsense?
	dt => [ qw(ds) ], # ditto...
	rt => [ qw(ts) ], # ditto...
);

sub do_p2q { # via wq_do
	my ($self) = @_;
	my $lei = $self->{lei};
	my $want = $lei->{opt}->{want} // [ qw(dfpost7) ];
	my @want = split(/[, ]+/, "@$want");
	for (@want) {
		/\A(?:(d|dt|rt):)?([0-9]+)(\.(?:day|weeks)s?)?\z/ or next;
		my ($pfx, $n, $unit) = ($1, $2, $3);
		$n *= 86400 * ($unit =~ /week/i ? 7 : 1);
		$_ = [ $pfx, $n ];
	}
	my $smsg = bless {}, 'PublicInbox::Smsg';
	my $in = $self->{0};
	unless ($in) {
		my $input = $self->{input};
		if (-e $input) {
			$in = $lei->fopen('<', $input) or
				return $lei->fail("open < $input: $!");
		} else {
			my @cmd = (qw(git format-patch --stdout -1), $input);
			$in = popen_rd(\@cmd, undef, { 2 => $lei->{2} });
		}
	};
	my $eml = PublicInbox::Eml->new(\(do { local $/; <$in> }));
	$lei->{diff_want} = +{ map { $_ => 1 } @want };
	$smsg->populate($eml);
	while (my ($pfx, $fields) = each %pfx2smsg) {
		next unless $lei->{diff_want}->{$pfx};
		for my $f (@$fields) {
			my $v = $smsg->{$f} // next;
			push @{$lei->{qterms}->{$pfx}}, xphrase($v);
		}
	}
	$eml->each_part(\&extract_terms, $lei, 1);
	if ($lei->{opt}->{debug}) {
		my $json = ref(PublicInbox::Config->json)->new;
		$json->utf8->canonical->pretty;
		$lei->err($json->encode($lei->{qterms}));
	}
	my (@q, %seen);
	for my $pfx (@want) {
		if (ref($pfx) eq 'ARRAY') {
			my ($p, $t_range) = @$pfx; # TODO

		} elsif ($pfx =~ m!\A(?:OR|XOR|AND|NOT)\z! ||
				$pfx =~ m!\A(?:ADJ|NEAR)(?:/[0-9]+)?\z!) {
			push @q, $pfx;
		} else {
			my $plusminus = ($pfx =~ s/\A([\+\-])//) ? $1 : '';
			my $end = ($pfx =~ s/([0-9\*]+)\z//) ? $1 : '';
			my $x = delete($lei->{qterms}->{$pfx}) or next;
			my $star = $end =~ tr/*//d ? '*' : '';
			my $min_len = ($end // 0) + 0;

			# no wildcards for bool_pfx_external
			$star = '' if $pfx =~ /\A(dfpre|dfpost|mid)\z/;
			$pfx = "$plusminus$pfx:";
			if ($min_len) {
				push @q, map {
					my @t = ($pfx.$_.$star);
					while (length > $min_len) {
						chop $_;
						push @t, 'OR', $pfx.$_.$star;
					}
					@t;
				} @$x;
			} else {
				push @q, map {
					my $k = $pfx.$_.$star;
					$seen{$k}++ ? () : $k
				} @$x;
			}
		}
	}
	if ($lei->{opt}->{uri}) {
		@q = (join('+', map { uri_escape_utf8($_) } @q));
	} else {
		@q = (join(' ', @q));
	}
	$lei->out(@q, "\n");
}

sub call { # the "lei patch-to-query" entry point
	my ($cls, $lei, $input) = @_;
	my $self = $lei->{p2q} = bless {}, $cls;
	if ($lei->{opt}->{stdin}) {
		$self->{0} = delete $lei->{0}; # guard from lei_atfork_child
	} else {
		$self->{input} = $input;
	}
	my $op = $lei->workers_start($self, 'lei patch2query', 1, {
		'' => [ $lei->{p2q_done} // $lei->can('dclose'), $lei ]
	});
	$self->wq_io_do('do_p2q', []);
	$self->wq_close(1);
	while ($op && $op->{sock}) { $op->event_step }
}

sub ipc_atfork_child {
	my ($self) = @_;
	my $lei = $self->{lei};
	$lei->lei_atfork_child;
	$SIG{__WARN__} = PublicInbox::Eml::warn_ignore_cb();
	$self->SUPER::ipc_atfork_child;
}

1;
