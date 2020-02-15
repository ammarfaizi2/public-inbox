# Copyright (C) 2015-2020 all contributors <meta@public-inbox.org>
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
use PublicInbox::WwwStream;
our $MIN_PARTIAL_LEN = 16;

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

sub mids_from_mset { # Search::retry_reopen callback
	[ map { PublicInbox::SearchMsg::from_mitem($_)->mid } $_[0]->items ];
}

sub search_partial ($$) {
	my ($srch, $mid) = @_;
	return if length($mid) < $MIN_PARTIAL_LEN;
	my $opt = { limit => PARTIAL_MAX, mset => 2 };
	my @try = ("m:$mid*");
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
		my @long = ($mid =~ m!(\w{3,})!g);
		push(@try, join(' ', map { "m:$_" } @long));

		# is the last element long enough to not trigger excessive
		# wildcard matches?
		if (length($long[-1]) > 8) {
			$long[-1] .= '*';
			push(@try, join(' ', map { "m:$_" } @long));
		}
	}

	foreach my $m (@try) {
		# If Xapian can't handle the wildcard since it
		# has too many results.  $@ can be
		# Search::Xapian::QueryParserError or even:
		# "something terrible happened at ../Search/Xapian/Enquire.pm"
		my $mset = eval { $srch->query($m, $opt) } or next;
		my $mids = $srch->retry_reopen(\&mids_from_mset, $mset);
		return $mids if scalar(@$mids);
	}
}

sub ext_msg_i {
	my ($other, $arg) = @_;
	my ($cur, $mid, $ibxs, $found) = @$arg;

	return if $other->{name} eq $cur->{name} || !$other->base_url;

	my $mm = $other->mm or return;

	# try to find the URL with Msgmap to avoid forking
	my $num = $mm->num_for($mid);
	if (defined $num) {
		push @$found, $other;
	} else {
		# no point in trying the fork fallback if we
		# know Xapian is up-to-date but missing the
		# message in the current repo
		push @$ibxs, $other;
	}
}

sub ext_msg {
	my ($ctx) = @_;
	my $cur = $ctx->{-inbox};
	my $mid = $ctx->{mid};

	eval { require PublicInbox::Msgmap };
	my $ibxs = [];
	my $found = [];
	my $arg = [ $cur, $mid, $ibxs, $found ];

	$ctx->{www}->{pi_config}->each_inbox(\&ext_msg_i, $arg);

	return exact($ctx, $found, $mid) if @$found;

	# fall back to partial MID matching
	my @partial;
	my $n_partial = 0;
	my $srch = $cur->search;
	my $mids = search_partial($srch, $mid) if $srch;
	if ($mids) {
		$n_partial = scalar(@$mids);
		push @partial, [ $cur, $mids ];
	}

	# can't find a partial match in current inbox, try the others:
	if (!$n_partial && length($mid) >= $MIN_PARTIAL_LEN) {
		foreach my $ibx (@$ibxs) {
			$srch = $ibx->search or next;
			$mids = search_partial($srch, $mid) or next;
			$n_partial += scalar(@$mids);
			push @partial, [ $ibx, $mids];
			last if $n_partial >= PARTIAL_MAX;
		}
	}

	my $code = 404;
	my $href = mid_href($mid);
	my $html = ascii_html($mid);
	my $title = "&lt;$html&gt; not found";
	my $s = "<pre>Message-ID &lt;$html&gt;\nnot found\n";
	if ($n_partial) {
		$code = 300;
		my $es = $n_partial == 1 ? '' : 'es';
		$n_partial .= '+' if ($n_partial == PARTIAL_MAX);
		$s .= "\n$n_partial partial match$es found:\n\n";
		my $cur_name = $cur->{name};
		foreach my $pair (@partial) {
			my ($ibx, $res) = @$pair;
			my $env = $ctx->{env} if $ibx->{name} eq $cur_name;
			my $u = $ibx->base_url($env) or next;
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
	$ctx->{-html_tip} = $s .= '</pre>';
	$ctx->{-title_html} = $title;
	$ctx->{-upfx} = '../';
	PublicInbox::WwwStream->response($ctx, $code);
}

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
	my ($ctx, $found, $mid) = @_;
	my $href = mid_href($mid);
	my $html = ascii_html($mid);
	my $title = "&lt;$html&gt; found in ";
	my $end = @$found == 1 ? 'another inbox' : 'other inboxes';
	$ctx->{-title_html} = $title . $end;
	$ctx->{-upfx} = '../';
	my $ext_urls = ext_urls($ctx, $mid, $href, $html);
	my $code = (@$found == 1 && $ext_urls eq '') ? 200 : 300;
	$ctx->{-html_tip} = join('',
			"<pre>Message-ID: &lt;$html&gt;\nfound in $end:\n\n",
				(map {
					my $u = $_->base_url;
					qq(<a\nhref="$u$href/">$u$html/</a>\n)
				} @$found),
			$ext_urls, '</pre>');
	PublicInbox::WwwStream->response($ctx, $code);
}

1;
