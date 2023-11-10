# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::WwwTopics;
use v5.12;
use PublicInbox::Hval qw(ascii_html mid_href fmt_ts);

sub add_topic_html ($$) {
	my (undef, $smsg) = @_;
	my $s = ascii_html($smsg->{subject});
	$s = '(no subject)' if $s eq '';
	$_[0] .= "\n".fmt_ts($smsg->{'MAX(ds)'} // $smsg->{ds}) .
		qq{ <a\nhref="}.mid_href($smsg->{mid}).qq{/#r">$s</a>};
	my $nr = $smsg->{'COUNT(num)'};
	$_[0] .= " $nr+ messages" if $nr > 1;
}

# n.b. the `SELECT DISTINCT(tid)' subquery is critical for performance
# with giant inboxes and extindices
sub topics_new ($) {
        $_[0]->do_get(<<EOS);
SELECT ds,ddd,COUNT(num) FROM over WHERE tid IN
(SELECT DISTINCT(tid) FROM over WHERE tid > 0 ORDER BY ts DESC LIMIT 200)
AND +num > 0
GROUP BY tid
ORDER BY ds ASC
EOS
}

sub topics_active ($) {
        $_[0]->do_get(<<EOS);
SELECT ddd,MAX(ds),COUNT(num) FROM over WHERE tid IN
(SELECT DISTINCT(tid) FROM over WHERE tid > 0 ORDER BY ts DESC LIMIT 200)
AND +num > 0
GROUP BY tid
ORDER BY ds ASC
EOS
}

sub topics_i { pop @{$_[0]->{msgs}} }

sub topics_atom { # GET /$INBOX_NAME/topics_(new|active).atom
	my ($ctx) = @_;
	require PublicInbox::WwwAtomStream;
	my ($hdr, $smsg, $val);
	$_->{ds} //= $_->{'MAX(ds)'} // 0 for @{$ctx->{msgs}};
	PublicInbox::WwwAtomStream->response($ctx, \&topics_i);
}

sub topics_html { # GET /$INBOX_NAME/topics_(new|active).html
	my ($ctx) = @_;
	require PublicInbox::WwwStream;
	my $buf = '<pre>';
	$ctx->{-html_more_links} = qq{\n- recent:[<a
href="./">subjects (threaded)</a>|};

	if ($ctx->{topic_category} eq 'new') {
		$ctx->{-html_more_links} .= qq{<b>topics (new)</b>|<a
href="./topics_active.html">topics (active)</a>]};
	} else { # topic_category eq "active" - topics with recent replies
		$ctx->{-html_more_links} .= qq{<a
href="./topics_new.html">topics (new)</a>|<b>topics (active)</b>]};
	}
	# can't use SQL to filter references since our schema wasn't designed
	# for it, but our SQL sorts by ascending time to favor top-level
	# messages while our final result (post-references filter) favors
	# recent messages
	my $msgs = delete $ctx->{msgs};
	add_topic_html($buf, pop @$msgs) while scalar(@$msgs);
	$buf .= '</pre>';
	PublicInbox::WwwStream::html_oneshot($ctx, 200, $buf);
}

sub response {
	my ($ctx, $ibx_name, $category, $type) = @_;
	my ($ret, $over);
	$ret = PublicInbox::WWW::invalid_inbox($ctx, $ibx_name) and return $ret;
	$over = $ctx->{ibx}->over or
		return PublicInbox::WWW::need($ctx, 'Overview', './');
	$ctx->{msgs} = $category eq 'new' ? topics_new($over) :
			topics_active($over);
	$ctx->{topic_category} = $category;
	$type eq 'atom' ? topics_atom($ctx) : topics_html($ctx);
}

1;
