# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::RepoBrowseGitLog;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepoBrowseBase);
use PublicInbox::RepoBrowseGit qw(git_dec_links);

# enable if we can speed it up..., over 100ms is unnacceptable
my @graph; # = qw(--graph);

# cannot rely on --date=format-local:... yet, it is too new (September 2015)
my $LOG_FMT = '--pretty=tformat:'.
		join('%x00', (@graph ? '' : '%n'), qw(%h s%s D%D));
my $MSG_FMT = join('%x00', '', qw(%ai a%an b%b));

sub call_git_log {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $max = $repo_info->{max_commit_count} || 50;
	$max = int($max);
	$max = 50 if $max == 0;

	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $h = $q->{h};
	$h eq '' and $h = 'HEAD';

	my $fmt = $LOG_FMT;
	$fmt .= $MSG_FMT if $q->{showmsg};
	$fmt .= '%x00%x00';

	my $git = $repo_info->{git};
	my $log = $git->popen(qw(log --no-notes --no-color
				--abbrev-commit --abbrev=10),
				@graph, $fmt, "-$max", $h);
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type'=>'text/html']]);
		git_log_stream($req, $q, $log, $fh);
		$fh->close;
	}
}

sub git_log_stream {
	my ($req, $q, $log, $fh) = @_;
	my $desc = $req->{repo_info}->{desc_html};
	my ($x, $author);
	my $showmsg = $q->{showmsg};

	if ($showmsg) {
		my $qs = $q->qs(showmsg => '');
		$qs = $req->{cgi}->path_info if ($qs eq '');
		$x = qq{<a\nhref="$qs">collapse</a>};
	} else {
		my $qs = $q->qs(showmsg => 1);
		$x = qq{<a\nhref="$qs">expand</a>};
	}

	$fh->write("<html><head><title>$desc" .
		"</title></head><body><pre><b>$desc</b>\n\n".
		"<b>Commit Log</b> ($x)\n");
	$fh->write(@graph && $showmsg ? '</pre>' : "\n");
	my %ac;
	local $/ = "\0\0\n";
	my $rel = $req->{relcmd};
	while (defined(my $line = <$log>)) {
		my @x;
		my ($gr, $id, $s, $D, $ai, $an, $b) = @x = split("\0", $line);

		# --graph may output graph-only lines without a commit
		unless (defined $id) {
			$fh->write($gr . "\n");
			next;
		}

		$s =~ s/\As//;
		$s = utf8_html($s);
		$s = qq(<a\nhref="${rel}commit?id=$id">$s</a>);

		if ($D =~ /\AD(.+)/) {
			$s .= ' ('. join(', ', git_dec_links($rel, $1)) . ')';
		}

		if (defined $b) {
			$an =~ s/\Aa//;
			$b =~ s/\Ab//;
			$b =~ s/\s*\z//s;

			my $ah = $ac{$an} ||= utf8_html($an);

			if (@graph) {
				# duplicate the last line of graph as many
				# times at it takes to match the number of
				# lines in the body:
				my $nl = ($b =~ tr/\n/\n/) + 4;
				$nl -= ($gr =~ tr/\n/\n/);
				$gr =~ s/([^\n]+)\z/($1."\n") x $nl/es;
			}
			$b = utf8_html($b);
			$b = "$s\n- $ah @ $ai\n\n$b";
			if (@graph) {
				$fh->write('<table><tr><td><pre>'. $gr .
					'</pre></td><td><pre>' . $b .
					'</pre></td></tr></table>');
			} else {
				$fh->write($b. "\n\n");
			}
		} else {
			$fh->write((@graph ? $gr : '') . $s . "\n");
		}
	}

	$fh->write((@graph && $showmsg ? '</pre>' : '') . '</body></html>');
}

1;
