# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::RepoBrowseLog;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);
require POSIX; # strftime
use PublicInbox::Git;

sub call_git {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $path = $repo_info->{path};
	my $max = $repo_info->{max_commit_count} || 50;
	$max = int($max);
	$max = 50 if $max == 0;

	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $h = $q->{h};
	$h eq '' and $h = 'HEAD';

	my $fmt = '%h%x00%ct%x00%s%x00%D';
	$fmt .= '%x00%an%x00%b' if $q->{showmsg};
	$fmt .= '%x00%n';

	my $ofs = $q->{ofs};
	$h .= "~$ofs" if $ofs =~ /\A\d+\z/;

	my $git = $repo_info->{git} ||= PublicInbox::Git->new($path);
	my $log = $git->popen(qw(log --no-notes --no-color
				--abbrev-commit --abbrev=16),
				"--format=$fmt", "-$max", $h);
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
		$x = qq{(<a\nhref="./$qs">collapse</a></th><th>Author};
	} else {
		my $qs = $q->qs(showmsg => 1);
		$x = qq{(<a\nhref="./$qs">expand</a>)};
	}

	$fh->write("<html><head><title>$desc" .
		"</title></head><body><p>$desc</p><table><tr>" .
		"<th>Date</th><th>Commit message $x</th></tr>");
	my %ac;
	local $/ = "\0\n\n";
	my $rel = $req->{relcmd};
	while (defined(my $line = <$log>)) {
		my ($id, $ct, $s, $D, $an, $b) = split("\0", $line);
		$line = undef;
		$s = PublicInbox::Hval->new_oneline($s)->as_html;

		# cannot rely on --date=format-local:... yet,
		# it is too new (September 2015)
		$ct = POSIX::strftime('%Y-%m-%d', gmtime($ct));

		# TODO: handle $D (decorate)
		$s = "<tr><td>$ct</td>" .
			qq(<td><a\nhref="${rel}commit?id=$id">$s</a></td>);
		if (defined $b) {
			my $ah = $ac{$an} ||=
				PublicInbox::Hval->new_oneline($an)->as_html;
			$b = PublicInbox::Hval->new($b)->as_html;
			$s .= "<td>$ah</td></tr>" .
				"<tr><td colspan=3><pre\n" .
				"style='white-space:pre-wrap'>$b</pre>".
				'</td></tr>';
		} else {
			$s .= '</tr>';
		}
		$fh->write($s);
	}

	$fh->write('</table></body></html>');
}

1;
