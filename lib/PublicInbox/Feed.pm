# Copyright (C) 2013-2020 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Used for generating Atom feeds for web-accessible mailing list archives.
package PublicInbox::Feed;
use strict;
use warnings;
use PublicInbox::MIME;
use PublicInbox::View;
use PublicInbox::WwwAtomStream;
use PublicInbox::Smsg; # this loads w/o Search::Xapian

sub generate_i {
	my ($ctx) = @_;
	while (my $smsg = shift @{$ctx->{msgs}}) {
		$ctx->{-inbox}->smsg_mime($smsg) and return $smsg;
	}
}

# main function
sub generate {
	my ($ctx) = @_;
	my $msgs = $ctx->{msgs} = recent_msgs($ctx);
	return _no_thread() unless @$msgs;
	PublicInbox::WwwAtomStream->response($ctx, 200, \&generate_i);
}

sub generate_thread_atom {
	my ($ctx) = @_;
	my $msgs = $ctx->{msgs} = $ctx->{-inbox}->over->get_thread($ctx->{mid});
	return _no_thread() unless @$msgs;
	PublicInbox::WwwAtomStream->response($ctx, 200, \&generate_i);
}

sub generate_html_index {
	my ($ctx) = @_;
	# if the 'r' query parameter is given, it is a legacy permalink
	# which we must continue supporting:
	my $qp = $ctx->{qp};
	my $ibx = $ctx->{-inbox};
	if ($qp && !$qp->{r} && $ibx->over) {
		return PublicInbox::View::index_topics($ctx);
	}

	my $env = $ctx->{env};
	my $url = $ibx->base_url($env) . 'new.html';
	my $qs = $env->{QUERY_STRING};
	$url .= "?$qs" if $qs ne '';
	[302, [ 'Location', $url, 'Content-Type', 'text/plain'],
		[ "Redirecting to $url\n" ] ];
}

sub new_html_i {
	my ($nr, $ctx) = @_;
	my $msgs = $ctx->{msgs};
	while (my $smsg = shift @$msgs) {
		my $m = $ctx->{-inbox}->smsg_mime($smsg) or next;
		my $more = scalar @$msgs;
		return PublicInbox::View::index_entry($m, $ctx, $more);
	}
	PublicInbox::View::pagination_footer($ctx, './new.html');
}

sub new_html {
	my ($ctx) = @_;
	my $msgs = $ctx->{msgs} = recent_msgs($ctx);
	if (!@$msgs) {
		return [404, ['Content-Type', 'text/plain'],
			["No messages, yet\n"] ];
	}
	$ctx->{-html_tip} = '<pre>';
	$ctx->{-upfx} = '';
	$ctx->{-hr} = 1;
	PublicInbox::WwwStream->response($ctx, 200, \&new_html_i);
}

# private subs

sub _no_thread () {
	[404, ['Content-Type', 'text/plain'], ["No feed found for thread\n"]];
}

sub recent_msgs {
	my ($ctx) = @_;
	my $ibx = $ctx->{-inbox};
	my $max = $ibx->{feedmax};
	my $qp = $ctx->{qp};
	my $v = $ibx->version;
	if ($v > 2) {
		die "BUG: unsupported inbox version: $v\n";
	}
	if ($ibx->over) {
		return PublicInbox::View::paginate_recent($ctx, $max);
	}

	my $hex = '[a-f0-9]';
	my $addmsg = qr!^:000000 100644 \S+ (\S+) A\t${hex}{2}/${hex}{38}$!;
	my $delmsg = qr!^:100644 000000 (\S+) \S+ D\t(${hex}{2}/${hex}{38})$!;
	my $refhex = qr/(?:HEAD|${hex}{4,40})(?:~[0-9]+)?/;

	# revision ranges may be specified
	my $range = 'HEAD';
	my $r = $qp->{r} if $qp;
	if ($r && ($r =~ /\A(?:$refhex\.\.)?$refhex\z/o)) {
		$range = $r;
	}

	# get recent messages
	# we could use git log -z, but, we already know ssoma will not
	# leave us with filenames with spaces in them..
	my $log = $ibx->git->popen(qw/log
				--no-notes --no-color --raw -r
				--no-abbrev --abbrev-commit/,
				"--format=%H", $range);
	my %deleted; # only an optimization at this point
	my $last;
	my $last_commit;
	local $/ = "\n";
	my @oids;
	while (defined(my $line = <$log>)) {
		if ($line =~ /$addmsg/o) {
			my $add = $1;
			next if $deleted{$add}; # optimization-only
			push @oids, $add;
			if (scalar(@oids) >= $max) {
				$last = 1;
				last;
			}
		} elsif ($line =~ /$delmsg/o) {
			$deleted{$1} = 1;
		}
	}

	if ($last) {
		local $/ = "\n";
		while (my $line = <$log>) {
			if ($line =~ /^(${hex}{7,40})/) {
				$last_commit = $1;
				last;
			}
		}
	}

	$ctx->{next_page} = "r=$last_commit" if $last_commit;
	[ map { bless {blob => $_ }, 'PublicInbox::Smsg' } @oids ];
}

1;
