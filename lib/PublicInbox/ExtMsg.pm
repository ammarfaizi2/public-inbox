# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Used by the web interface to link to messages outside of the our
# public-inboxes.  Mail threads may cross projects/threads; so
# we should ensure users can find more easily find them on other
# sites.
package PublicInbox::ExtMsg;
use strict;
use warnings;
use PublicInbox::Hval qw(ascii_html prurl mid_href);
use PublicInbox::WwwStream qw(html_oneshot);
use PublicInbox::Smsg;
our $MIN_PARTIAL_LEN = 14; # for 'XXXXXXXXXX.fsf' msgids gnus generates
use B qw(cstring);

# TODO: user-configurable
our @EXT_URL = map { ascii_html($_) } (
	# leading "//" denotes protocol-relative (http:// or https://)
	'//marc.info/?i=%s',
	'//www.mail-archive.com/search?l=mid&q=%s',
	'nntp://news.gmane.io/%s',
	'https://lists.debian.org/msgid-search/%s',
	'//docs.FreeBSD.org/cgi/mid.cgi?db=mid&id=%s',
	'https://www.w3.org/mid/%s',
	'http://www.postgresql.org/message-id/%s',
	'https://lists.debconf.org/cgi-lurker/keyword.cgi?'.
		'doc-url=/lurker&format=en.html&query=id:%s'
);

sub PARTIAL_MAX () { 100 }

sub partial_cb { # async_mset cb
	my ($ctx, $srch, $mset, $err) = @_;
	if ($err) {
		my $msg = "W: query failed: $! ($err)";
		++$ctx->{ext_msg_partial_fail};
		warn $msg, "\n",
			map { ' '.cstring($_)."\n" } @{$ctx->{try}};
	} else {
		my $seen = $ctx->{partial_seen} //= {};
		my (@mid, $mid);
		for (@{$srch->mset_to_smsg($ctx->{partial_ibx}, $mset)}) {
			$mid = $_->{mid};
			$seen->{$mid} //= push @mid, $mid;
		}
		if (scalar @mid) {
			push @{$ctx->{partial}}, [ $ctx->{partial_ibx}, \@mid ];
			(($ctx->{n_partial} += scalar(@mid)) >= PARTIAL_MAX) and
				delete $ctx->{again}; # done
		}
	}
	PublicInbox::DS::requeue($ctx) if $ctx->{env}->{'pi-httpd.app'};
}

# returns true if started asynchronously
sub partial_ibx_start ($$) {
	my ($ctx, $ibx) = @_;
	my $mid = $ctx->{mid};
	return finalize_partial($ctx) if length($mid) < $MIN_PARTIAL_LEN;
	my $srch;
	while (!defined($srch = $ibx->isrch)) {
		$ibx = shift @{$ctx->{again}} or last;
	}
	return finalize_partial($ctx) if !$srch;
	my @try = ("m:$mid*");
	# some email obfuscators may replace parts of addresses with "..."
	# and confuse the QP into attempting to parse a range op:
	# "foo.bar....@example.com"
	my ($pfx, $domain) = split /\@/, $mid, 2;
	if ($pfx =~ s/\.\.+\z//s) {
		my @pfx = split /\W+/, $pfx;
		push @try, join ' ', map { "m:$_" } @pfx;
		$try[-1] .= '*';
		$try[-1] .= ' '.join ' ', map { "m:$_" } split /\W+/, $domain
			if defined $domain;
	}
	my $chop = $mid;
	if ($chop =~ s/(\W+)(\w*)\z//) {
		my ($delim, $word) = ($1, $2);
		if (length($word)) {
			push @try, "m:$chop$delim";
			push @try, "m:$chop$delim*";
		}
		push @try, "m:$chop";
		push @try, "m:$chop*";
	}

	# break out long words individually to search for, because
	# too many messages begin with "Pine.LNX." (or "alpine" or "nycvar")
	if ($mid =~ /\w{9,}/) {
		my @long = sort { length $b <=> length $a }
			($mid =~ m!(\w{3,})!g);

		# are some elements long enough to not trigger excessive
		# wildcard matches?
		for (@long) {
			last if 8 >= length;
			$_ .= '*';
		}
		push @try, join ' ', map { "m:$_" } @long;
	}
	$ctx->{partial_ibx} = $ibx;
	my $opt = { limit => PARTIAL_MAX, sort_col => -1, asc => 1 };
	@{$ctx->{try}} = @try;
	$srch->async_mset(\@try, $opt, \&partial_cb, $ctx, $srch);
}

sub ext_msg_i {
	my ($other, $ctx) = @_;

	return if $other->{name} eq $ctx->{ibx}->{name} ||
		!$other->base_url(@{$ctx->{-url_env}});

	my $mm = $other->mm or return;

	# try to find the URL with Msgmap to avoid forking
	my $num = $mm->num_for($ctx->{mid});
	if (defined $num) {
		push @{$ctx->{found}}, $other;
	} else {
		# no point in trying the fork fallback if we
		# know Xapian is up-to-date but missing the
		# message in the current repo
		push @{$ctx->{again}}, $other;
	}
}

sub ext_msg_step {
	my ($pi_cfg, $section, $ctx) = @_;
	if (defined($section)) {
		return if $section !~ m!\Apublicinbox\.([^/]+)\z!;
		my $ibx = $pi_cfg->lookup_name($1) or return;
		ext_msg_i($ibx, $ctx);
	} else { # undef == "EOF"
		finalize_exact($ctx);
	}
}

sub partial_enter ($) {
	my ($ctx) = @_;
	bless $ctx, __PACKAGE__; # for ExtMsg->event_step
	return $ctx->event_step if $ctx->{env}->{'pi-httpd.app'};
	$ctx->event_step while $ctx->{-wcb}; # generic PSGI
}

sub partial_prepare ($@) {
	my ($ctx, @try_ibxish) = @_;
	$ctx->{again} = \@try_ibxish;
	sub {
		$ctx->{-wcb} = $_[0]; # HTTP server write callback
		partial_enter $ctx;
	}
}

sub ext_msg_ALL ($) {
	my ($ctx) = @_;
	my $ALL = $ctx->{www}->{pi_cfg}->ALL or return;
	return partial_prepare($ctx, $ALL) if $ALL == $ctx->{ibx};
	my $by_eidx_key = $ctx->{www}->{pi_cfg}->{-by_eidx_key};
	my $cur_key = $ctx->{ibx}->eidx_key;
	my %seen = ($cur_key => 1);
	my ($id, $prev);
	while (my $x = $ALL->over->next_by_mid($ctx->{mid}, \$id, \$prev)) {
		my $xr3 = $ALL->over->get_xref3($x->{num});
		for my $k (@$xr3) {
			$k =~ s/:[0-9]+:$x->{blob}\z// or next;
			next if $k eq $cur_key;
			my $ibx = $by_eidx_key->{$k} // next;
			$ibx->base_url(@{$ctx->{-url_env}}) or next;
			push(@{$ctx->{found}}, $ibx) unless $seen{$k}++;
		}
	}
	$ctx->{found} ? exact($ctx) : partial_prepare($ctx, $ctx->{ibx}, $ALL);
}

# only public entry point
sub ext_msg {
	my ($ctx) = @_;
	@{$ctx->{-url_env}} = $ctx->{www}->{pi_cfg}->{
			lc 'publicinbox.nameIsUrl'} ? ($ctx->{env}) : ();
	ext_msg_ALL($ctx) // sub {
		$ctx->{-wcb} = $_[0]; # HTTP server write callback

		if ($ctx->{env}->{'pi-httpd.app'}) {
			require PublicInbox::ConfigIter;
			my $iter = PublicInbox::ConfigIter->new(
						$ctx->{www}->{pi_cfg},
						\&ext_msg_step, $ctx);
			$iter->event_step;
		} else {
			$ctx->{www}->{pi_cfg}->each_inbox(\&ext_msg_i, $ctx);
			finalize_exact($ctx);
		}
	};
}

# called via PublicInbox::DS::event_loop
sub event_step {
	my ($ctx) = @_;
	# can't find a partial match in current inbox, try the others:
	my $ibx = shift @{$ctx->{again}} or return finalize_partial($ctx);
	partial_ibx_start $ctx, $ibx;
}

sub finalize_exact {
	my ($ctx) = @_;
	if ($ctx->{found}) {
		delete($ctx->{-wcb})->(exact($ctx));
	} else { # no exact matches? fall back to partial msgid matching
		$ctx->{again} = [ $ctx->{ibx} ];
		partial_enter $ctx;
	}
}

sub _url_pfx ($$;$) {
	my ($ctx, $ibx, $env) = @_;
	my $u = $ibx->base_url(@{$env // $ctx->{-url_env}}) // return;
	(index($u, '://') < 0 && index($u, '/') != 0) ?
		"$ctx->{-upfx}../$u" : $u;
}

sub partial_response ($) {
	my ($ctx) = @_;
	my $mid = $ctx->{mid};
	my $code = 404;
	my $href = mid_href($mid);
	my $html = ascii_html($mid);
	my $title = "&lt;$html&gt; not found";
	my $s = "<pre>Message-ID &lt;$html&gt;\nnot found\n";
	$ctx->{-upfx} //= '../';
	if (my $n_partial = $ctx->{n_partial}) {
		$code = 300;
		my $es = $n_partial == 1 ? '' : 'es';
		$n_partial .= '+' if ($n_partial == PARTIAL_MAX);
		$s .= "\n$n_partial partial match$es found:\n\n";
		my $cur_name = $ctx->{ibx}->{name};
		my $e = [ $ctx->{env} ];
		foreach my $pair (@{$ctx->{partial}}) {
			my ($ibx, $res) = @$pair;
			my $u = _url_pfx($ctx, $ibx,
				$ibx->{name} eq $cur_name ? $e : undef) // next;
			foreach my $m (@$res) {
				my $href = mid_href($m);
				my $html = ascii_html($m);
				$s .= qq{<a\nhref="$u$href/">$u$html/</a>\n};
			}
		}
	}
	my $ext = ext_urls($ctx, $mid, $href, $html);
	if ($ext ne '') {
		$s .= $ext;
		$code = 300;
	}
	if (my $nr = delete $ctx->{ext_msg_partial_fail}) {
		$s .= <<EOM

$nr internal search queries failed (likely due to server overload)
EOM
	}
	chop $s; # omit trailing \n
	$ctx->{-html_tip} = $s .= '</pre>';
	$ctx->{-title_html} = $title;
	html_oneshot($ctx, $code);
}

sub finalize_partial ($) { delete($_[0]->{-wcb})->(partial_response($_[0])) }

sub ext_urls {
	my ($ctx, $mid, $href, $html) = @_;

	# Fall back to external repos if configured
	if (@EXT_URL && index($mid, '@') >= 0) {
		my $env = $ctx->{env};
		my $e = "\nPerhaps try an external site:\n\n";
		foreach my $url (@EXT_URL) {
			my $u = prurl($env, $url);
			my $r = sprintf($u, $href);
			my $t = sprintf($u, $html);
			$e .= qq{<a\nhref="$r">$t</a>\n};
		}
		return $e;
	}
	''
}

sub exact {
	my ($ctx) = @_;
	my $mid = $ctx->{mid};
	my $found = $ctx->{found};
	my $href = mid_href($mid);
	my $html = ascii_html($mid);
	my $title = "&lt;$html&gt; found in ";
	my $end = @$found == 1 ? 'another inbox' : 'other inboxes';
	$ctx->{-title_html} = $title . $end;
	$ctx->{-upfx} //= '../';
	my $ext_urls = ext_urls($ctx, $mid, $href, $html);
	my $code = (@$found == 1 && $ext_urls eq '') ? 200 : 300;
	$ctx->{-html_tip} = join('',
			"<pre>Message-ID: &lt;$html&gt;\nfound in $end:\n\n",
				(map {
					my $u = _url_pfx($ctx, $_);
					qq(<a\nhref="$u$href/">$u$html/</a>\n)
				} @$found),
			$ext_urls, '</pre>');
	html_oneshot($ctx, $code);
}

1;
