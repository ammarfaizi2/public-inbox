# Copyright (C) 2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# per-mitem/smsg iterators for search results
# "ovv" => "Overview viewer"
package PublicInbox::LeiOverview;
use strict;
use v5.10.1;
use POSIX qw(strftime);
use File::Spec;
use PublicInbox::MID qw($MID_EXTRACT);
use PublicInbox::Address qw(pairs);
use PublicInbox::Config;
use PublicInbox::Search qw(get_pct);

# cf. https://en.wikipedia.org/wiki/JSON_streaming
my $JSONL = 'ldjson|ndjson|jsonl'; # 3 names for the same thing

sub _iso8601 ($) { strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($_[0])) }

sub new {
	my ($class, $lei) = @_;
	my $opt = $lei->{opt};
	my $out = $opt->{output} // '-';
	$out = '/dev/stdout' if $out eq '-';

	my $fmt = $opt->{'format'};
	$fmt = lc($fmt) if defined $fmt;
	if ($out =~ s/\A([a-z]+)://is) { # e.g. Maildir:/home/user/Mail/
		my $ofmt = lc $1;
		$fmt //= $ofmt;
		return $lei->fail(<<"") if $fmt ne $ofmt;
--format=$fmt and --output=$ofmt conflict

	}
	$fmt //= 'json' if $out eq '/dev/stdout';
	$fmt //= 'maildir'; # TODO

	if (index($out, '://') < 0) { # not a URL, so assume path
		 $out = File::Spec->canonpath($out);
	} # else URL

	my $self = bless { fmt => $fmt, out => $out }, $class;
	my $json;
	if ($fmt =~ /\A($JSONL|(?:concat)?json)\z/) {
		$json = $self->{json} = ref(PublicInbox::Config->json);
	}
	my ($isatty, $seekable);
	if ($out eq '/dev/stdout') {
		$isatty = -t $lei->{1};
		$lei->start_pager if $isatty;
		$opt->{pretty} //= $isatty;
	} elsif ($json) {
		return $lei->fail('JSON formats only output to stdout');
	}
	$self;
}

# called once by parent
sub ovv_begin {
	my ($self, $lei) = @_;
	if ($self->{fmt} eq 'json') {
		print { $lei->{1} } '[';
	} # TODO HTML/Atom/...
}

# called once by parent (via PublicInbox::EOFpipe)
sub ovv_end {
	my ($self, $lei) = @_;
	if ($self->{fmt} eq 'json') {
		# JSON doesn't allow trailing commas, and preventing
		# trailing commas is a PITA when parallelizing outputs
		print { $lei->{1} } "null]\n";
	} elsif ($self->{fmt} eq 'concatjson') {
		print { $lei->{1} } "\n";
	}
}

sub ovv_atfork_child {
	my ($self) = @_;
	# reopen dedupe here
}

# prepares an smsg for JSON
sub _unbless_smsg {
	my ($smsg, $mitem) = @_;

	delete @$smsg{qw(lines bytes num tid)};
	$smsg->{rcvd} = _iso8601(delete $smsg->{ts}); # JMAP receivedAt
	$smsg->{dt} = _iso8601(delete $smsg->{ds}); # JMAP UTCDate
	$smsg->{relevance} = get_pct($mitem) if $mitem;

	if (my $r = delete $smsg->{references}) {
		$smsg->{references} = [
				map { "<$_>" } ($r =~ m/$MID_EXTRACT/go) ];
	}
	if (my $m = delete($smsg->{mid})) {
		$smsg->{'m'} = "<$m>";
	}
	for my $f (qw(from to cc)) {
		my $v = delete $smsg->{$f} or next;
		$smsg->{substr($f, 0, 1)} = pairs($v);
	}
	$smsg->{'s'} = delete $smsg->{subject};
	# can we be bothered to parse From/To/Cc into arrays?
	scalar { %$smsg }; # unbless
}

sub ovv_atexit_child {
	my ($self, $lei) = @_;
	my $bref = delete $lei->{ovv_buf} or return;
	print { $lei->{1} } $$bref;
}

# JSON module ->pretty output wastes too much vertical white space,
# this (IMHO) provides better use of screen real-estate while not
# being excessively compact:
sub _json_pretty {
	my ($json, $k, $v) = @_;
	if (ref $v eq 'ARRAY') {
		if (@$v) {
			my $sep = ",\n" . (' ' x (length($k) + 7));
			if (ref($v->[0])) { # f/t/c
				$v = '[' . join($sep, map {
					my $pair = $json->encode($_);
					$pair =~ s/(null|"),"/$1, "/g;
					$pair;
				} @$v) . ']';
			} else { # references
				$v = '[' . join($sep, map {
					substr($json->encode([$_]), 1, -1);
				} @$v) . ']';
			}
		} else {
			$v = '[]';
		}
	}
	qq{  "$k": }.$v;
}

sub ovv_each_smsg_cb {
	my ($self, $lei) = @_;
	$lei->{ovv_buf} = \(my $buf = '');
	my $json = $self->{json}->new;
	if ($json) {
		$json->utf8->canonical;
		$json->ascii(1) if $lei->{opt}->{ascii};
	}
	if ($self->{fmt} =~ /\A(concat)?json\z/ && $lei->{opt}->{pretty}) {
		my $EOR = ($1//'') eq 'concat' ? "\n}" : "\n},";
		sub { # DIY prettiness :P
			my ($smsg, $mitem) = @_;
			$smsg = _unbless_smsg($smsg, $mitem);
			$buf .= "{\n";
			$buf .= join(",\n", map {
				my $v = $smsg->{$_};
				if (ref($v)) {
					_json_pretty($json, $_, $v);
				} else {
					$v = $json->encode([$v]);
					qq{  "$_": }.substr($v, 1, -1);
				}
			} sort keys %$smsg);
			$buf .= $EOR;
			if (length($buf) > 65536) {
				print { $lei->{1} } $buf;
				$buf = '';
			}
		}
	} elsif ($json) {
		my $ORS = $self->{fmt} eq 'json' ? ",\n" : "\n"; # JSONL
		sub {
			my ($smsg, $mitem) = @_;
			delete @$smsg{qw(tid num)};
			$buf .= $json->encode(_unbless_smsg(@_)) . $ORS;
			if (length($buf) > 65536) {
				print { $lei->{1} } $buf;
				$buf = '';
			}
		}
	} elsif ($self->{fmt} eq 'oid') {
		sub {
			my ($smsg, $mitem) = @_;
		}
	} # else { ...
}

1;