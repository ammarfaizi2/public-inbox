# Copyright (C) 2015-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Displays search results for the web interface
package PublicInbox::SearchView;
use strict;
use warnings;
use URI::Escape qw(uri_unescape uri_escape);
use PublicInbox::Smsg;
use PublicInbox::Hval qw(ascii_html obfuscate_addrs mid_href);
use PublicInbox::View;
use PublicInbox::WwwAtomStream;
use PublicInbox::WwwStream qw(html_oneshot);
use PublicInbox::SearchThread;
our $LIM = 200;
my %rmap_inc;

sub mbox_results {
	my ($ctx) = @_;
	my $q = PublicInbox::SearchQuery->new($ctx->{qp});
	my $x = $q->{x};
	require PublicInbox::Mbox;
	return PublicInbox::Mbox::mbox_all($ctx, $q->{'q'}) if $x eq 'm';
	sres_top_html($ctx);
}

sub sres_top_html {
	my ($ctx) = @_;
	my $srch = $ctx->{-inbox}->search or
		return PublicInbox::WWW::need($ctx, 'Search');
	my $q = PublicInbox::SearchQuery->new($ctx->{qp});
	my $x = $q->{x};
	my $query = $q->{'q'};
	my $o = $q->{o};
	my $asc;
	if ($o < 0) {
		$asc = 1;
		$o = -($o + 1); # so [-1] is the last element, like Perl lists
	}

	my $code = 200;
	# double the limit for expanded views:
	my $opts = {
		limit => $q->{l},
		offset => $o,
		mset => 1,
		relevance => $q->{r},
		asc => $asc,
	};
	my ($mset, $total, $err, $html);
retry:
	eval {
		$mset = $srch->query($query, $opts);
		$total = $mset->get_matches_estimated;
	};
	$err = $@;
	ctx_prepare($q, $ctx);
	if ($err) {
		$code = 400;
		$html = '<pre>'.err_txt($ctx, $err).'</pre><hr>';
	} elsif ($total == 0) {
		if (defined($ctx->{-uxs_retried})) {
			# undo retry damage:
			$q->{'q'} = $ctx->{-uxs_retried};
		} elsif (index($q->{'q'}, '%') >= 0) {
			$ctx->{-uxs_retried} = $q->{'q'};
			$q->{'q'} = uri_unescape($q->{'q'});
			goto retry;
		}
		$code = 404;
		$html = "<pre>\n[No results found]</pre><hr>";
	} else {
		return adump($_[0], $mset, $q, $ctx) if $x eq 'A';

		$ctx->{-html_tip} = search_nav_top($mset, $q, $ctx);
		return mset_thread($ctx, $mset, $q) if $x eq 't';
		mset_summary($ctx, $mset, $q); # appends to {-html_tip}
		$html = '';
	}
	html_oneshot($ctx, $code);
}

# display non-nested search results similar to what users expect from
# regular WWW search engines:
sub mset_summary {
	my ($ctx, $mset, $q) = @_;

	my $total = $mset->get_matches_estimated;
	my $pad = length("$total");
	my $pfx = ' ' x $pad;
	my $res = \($ctx->{-html_tip});
	my $ibx = $ctx->{-inbox};
	my $srch = $ibx->search;
	my $obfs_ibx = $ibx->{obfuscate} ? $ibx : undef;
	foreach my $m ($mset->items) {
		my $rank = sprintf("%${pad}d", $m->get_rank + 1);
		my $pct = get_pct($m);
		my $smsg = PublicInbox::Smsg::from_mitem($m, $srch);
		unless ($smsg) {
			eval {
				$m = "$m ".$m->get_docid . " expired\n";
				$ctx->{env}->{'psgi.errors'}->print($m);
			};
			next;
		}
		my $s = ascii_html($smsg->{subject});
		my $f = ascii_html($smsg->{from_name});
		if ($obfs_ibx) {
			obfuscate_addrs($obfs_ibx, $s);
			obfuscate_addrs($obfs_ibx, $f);
		}
		my $date = PublicInbox::View::fmt_ts($smsg->{ds});
		my $mid = mid_href($smsg->{mid});
		$s = '(no subject)' if $s eq '';
		$$res .= qq{$rank. <b><a\nhref="$mid/">}.
			$s . "</a></b>\n";
		$$res .= "$pfx  - by $f @ $date UTC [$pct%]\n\n";
	}
	$$res .= search_nav_bot($mset, $q);
	undef;
}

# shorten "/full/path/to/Foo/Bar.pm" to "Foo/Bar.pm" so error
# messages don't reveal FS layout info in case people use non-standard
# installation paths
sub path2inc ($) {
	my $full = $_[0];
	if (my $short = $rmap_inc{$full}) {
		return $short;
	} elsif (!scalar(keys %rmap_inc) && -e $full) {
		%rmap_inc = map {; "$INC{$_}" => $_ } keys %INC;
		# fall back to basename as last resort
		$rmap_inc{$full} // (split('/', $full))[-1];
	} else {
		$full;
	}
}

sub err_txt {
	my ($ctx, $err) = @_;
	my $u = $ctx->{-inbox}->base_url($ctx->{env}) . '_/text/help/';
	$err =~ s/^\s*Exception:\s*//; # bad word to show users :P
	$err =~ s!(\S+)!path2inc($1)!sge;
	$err = ascii_html($err);
	"\nBad query: <b>$err</b>\n" .
		qq{See <a\nhref="$u">$u</a> for help on using search};
}

sub search_nav_top {
	my ($mset, $q, $ctx) = @_;
	my $m = $q->qs_html(x => 'm', r => undef);
	my $rv = qq{<form\naction="?$m"\nmethod="post"><pre>};
	my $initial_q = $ctx->{-uxs_retried};
	if (defined $initial_q) {
		my $rewritten = $q->{'q'};
		utf8::decode($initial_q);
		utf8::decode($rewritten);
		$initial_q = ascii_html($initial_q);
		$rewritten = ascii_html($rewritten);
		$rv .= " Warning: Initial query:\n <b>$initial_q</b>\n";
		$rv .= " returned no results, used:\n";
		$rv .= " <b>$rewritten</b>\n instead\n\n";
	}

	$rv .= 'Search results ordered by [';
	if ($q->{r}) {
		my $d = $q->qs_html(r => 0);
		$rv .= qq{<a\nhref="?$d">date</a>|<b>relevance</b>};
	} else {
		my $d = $q->qs_html(r => 1);
		$rv .= qq{<b>date</b>|<a\nhref="?$d">relevance</a>};
	}

	$rv .= ']  view[';

	my $x = $q->{x};
	if ($x eq '') {
		my $t = $q->qs_html(x => 't');
		$rv .= qq{<b>summary</b>|<a\nhref="?$t">nested</a>}
	} elsif ($q->{x} eq 't') {
		my $s = $q->qs_html(x => '');
		$rv .= qq{<a\nhref="?$s">summary</a>|<b>nested</b>};
	}
	my $A = $q->qs_html(x => 'A', r => undef);
	$rv .= qq{|<a\nhref="?$A">Atom feed</a>]};
	$rv .= qq{\n\t\t\t\t\t\tdownload: };
	$rv .= qq{<input\ntype=submit\nvalue="mbox.gz"/></pre></form><pre>};
}

sub search_nav_bot {
	my ($mset, $q) = @_;
	my $total = $mset->get_matches_estimated;
	my $l = $q->{l};
	my $rv = '</pre><hr><pre id=t>';
	my $o = $q->{o};
	my $off = $o < 0 ? -($o + 1) : $o;
	my $end = $off + $mset->size;
	my $beg = $off + 1;

	if ($beg <= $end) {
		$rv .= "Results $beg-$end of $total";
		$rv .= ' (estimated)' if $end != $total;
	} else {
		$rv .= "No more results, only $total";
	}
	my ($next, $join, $prev);

	if ($o >= 0) { # sort descending
		my $n = $o + $l;
		if ($n < $total) {
			$next = $q->qs_html(o => $n, l => $l);
		}
		if ($o > 0) {
			$join = $n < $total ? '/' : '       ';
			my $p = $o - $l;
			$prev = $q->qs_html(o => ($p > 0 ? $p : 0));
		}
	} else { # o < 0, sort ascending
		my $n = $o - $l;

		if (-$n < $total) {
			$next = $q->qs_html(o => $n, l => $l);
		}
		if ($o < -1) {
			$join = -$n < $total ? '/' : '       ';
			my $p = $o + $l;
			$prev = $q->qs_html(o => ($p < 0 ? $p : 0));
		}
	}

	$rv .= qq{  <a\nhref="?$next"\nrel=next>next</a>} if $next;
	$rv .= $join if $join;
	$rv .= qq{<a\nhref="?$prev"\nrel=prev>prev</a>} if $prev;

	my $rev = $q->qs_html(o => $o < 0 ? 0 : -1);
	$rv .= qq{ | <a\nhref="?$rev">reverse results</a></pre>};
}

sub sort_relevance {
	[ sort {
		(eval { $b->topmost->{pct} } // 0) <=>
		(eval { $a->topmost->{pct} } // 0)
	} @{$_[0]} ]
}

sub get_pct ($) {
	# Capped at "99%" since "100%" takes an extra column in the
	# thread skeleton view.  <xapian/mset.h> says the value isn't
	# very meaningful, anyways.
	my $n = $_[0]->get_percent;
	$n > 99 ? 99 : $n;
}

sub load_msgs {
	my ($mset) = @_;
	[ map {
		my $mi = $_;
		my $smsg = PublicInbox::Smsg::from_mitem($mi);
		$smsg->{pct} = get_pct($mi);
		$smsg;
	} ($mset->items) ]
}

sub mset_thread {
	my ($ctx, $mset, $q) = @_;
	my $ibx = $ctx->{-inbox};
	my $msgs = $ibx->search->retry_reopen(\&load_msgs, $mset);
	my $r = $q->{r};
	my $rootset = PublicInbox::SearchThread::thread($msgs,
		$r ? \&sort_relevance : \&PublicInbox::View::sort_ds,
		$ctx);
	my $skel = search_nav_bot($mset, $q). "<pre>";
	$ctx->{-upfx} = '';
	$ctx->{anchor_idx} = 1;
	$ctx->{cur_level} = 0;
	$ctx->{skel} = \$skel;
	$ctx->{mapping} = {};
	$ctx->{searchview} = 1;
	$ctx->{prev_attr} = '';
	$ctx->{prev_level} = 0;
	$ctx->{s_nr} = scalar(@$msgs).'+ results';

	# reduce hash lookups in skel_dump
	$ctx->{-obfs_ibx} = $ibx->{obfuscate} ? $ibx : undef;
	PublicInbox::View::walk_thread($rootset, $ctx,
		\&PublicInbox::View::pre_thread);

	@$msgs = reverse @$msgs if $r;
	$ctx->{msgs} = $msgs;
	PublicInbox::WwwStream::aresponse($ctx, 200, \&mset_thread_i);
}

# callback for PublicInbox::WwwStream::getline
sub mset_thread_i {
	my ($ctx, $eml) = @_;
	$ctx->zmore($ctx->html_top) if exists $ctx->{-html_tip};
	$eml and return PublicInbox::View::eml_entry($ctx, $eml);
	my $smsg = shift @{$ctx->{msgs}} or
		$ctx->zmore(${delete($ctx->{skel})});
	$smsg;
}

sub ctx_prepare {
	my ($q, $ctx) = @_;
	my $qh = $q->{'q'};
	utf8::decode($qh);
	$qh = ascii_html($qh);
	$ctx->{-q_value_html} = $qh;
	$ctx->{-atom} = '?'.$q->qs_html(x => 'A', r => undef);
	$ctx->{-title_html} = "$qh - search results";
	my $extra = '';
	$extra .= qq{<input\ntype=hidden\nname=r />} if $q->{r};
	if (my $x = $q->{x}) {
		$x = ascii_html($x);
		$extra .= qq{<input\ntype=hidden\nname=x\nvalue="$x" />};
	}
	$ctx->{-extra_form_html} = $extra;
}

sub adump {
	my ($cb, $mset, $q, $ctx) = @_;
	$ctx->{items} = [ $mset->items ];
	$ctx->{search_query} = $q; # used by WwwAtomStream::atom_header
	PublicInbox::WwwAtomStream->response($ctx, 200, \&adump_i);
}

# callback for PublicInbox::WwwAtomStream::getline
sub adump_i {
	my ($ctx) = @_;
	while (my $mi = shift @{$ctx->{items}}) {
		my $srch = $ctx->{-inbox}->search(undef, $ctx) or return;
		my $smsg = eval {
			PublicInbox::Smsg::from_mitem($mi, $srch);
		} or next;
		return $smsg;
	}
}

package PublicInbox::SearchQuery;
use strict;
use warnings;
use URI::Escape qw(uri_escape);
use PublicInbox::MID qw(MID_ESC);

sub new {
	my ($class, $qp) = @_;

	my $r = $qp->{r};
	my ($l) = (($qp->{l} || '') =~ /([0-9]+)/);
	$l = $LIM if !$l || $l > $LIM;
	bless {
		q => $qp->{'q'},
		x => $qp->{x} || '',
		o => (($qp->{o} || '0') =~ /(-?[0-9]+)/),
		l => $l,
		r => (defined $r && $r ne '0'),
	}, $class;
}

sub qs_html {
	my ($self, %override) = @_;

	if (scalar(keys(%override))) {
		$self = bless { (%$self, %override) }, ref($self);
	}

	my $q = uri_escape($self->{'q'}, MID_ESC);
	$q =~ s/%20/+/g; # improve URL readability
	my $qs = "q=$q";

	if (my $o = $self->{o}) { # ignore o == 0
		$qs .= "&amp;o=$o";
	}
	if (my $l = $self->{l}) {
		$qs .= "&amp;l=$l" unless $l == $LIM;
	}
	if (my $r = $self->{r}) {
		$qs .= "&amp;r";
	}
	if (my $x = $self->{x}) {
		$qs .= "&amp;x=$x" if ($x eq 't' || $x eq 'A' || $x eq 'm');
	}
	$qs;
}

1;
