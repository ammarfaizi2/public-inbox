# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

package PublicInbox::RepobrowseGitLog;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::RepobrowseGit qw(git_dec_links git_commit_title);
# cannot rely on --date=format-local:... yet, it is too new (September 2015)
my $LOG_FMT = '--pretty=tformat:'.
		join('%x00', qw(%h %p %s D%D));
my $MSG_FMT = join('%x00', '', qw(%ai a%an b%b));

sub call_git_log {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $max = $repo_info->{max_commit_count} || 50;
	$max = int($max);
	$max = 50 if $max == 0;

	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $h = $q->{h};
	$h eq '' and $h = 'HEAD';

	my $fmt = $LOG_FMT;
	$fmt .= $MSG_FMT if $q->{showmsg};
	$fmt .= '%x00%x00';

	my $git = $repo_info->{git};
	my $log = $git->popen(qw(log --no-notes --no-color
				--abbrev-commit --abbrev=12),
				$fmt, "-$max", $h);
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type'=>'text/html']]);
		git_log_stream($req, $q, $log, $fh, $git);
		$fh->close;
	}
}

sub git_log_stream {
	my ($req, $q, $log, $fh, $git) = @_;
	my $desc = $req->{repo_info}->{desc_html};
	my $showmsg = $q->{showmsg};

	my $x = 'commit log ';
	if ($showmsg) {
		$showmsg = "&showmsg=1";
		my $qs = $q->qs(showmsg => '');
		$qs = $req->{cgi}->path_info if ($qs eq '');
		$x .= qq{[<a\nhref="$qs">oneline</a>|<b>expand</b>]};
	} else {
		my $qs = $q->qs(showmsg => 1);
		$x .= qq{[<b>oneline</b>|<a\nhref="$qs">expand</a>]};
	}

	my $rel = $req->{relcmd};
	$fh->write('<html><head>' . PublicInbox::Hval::STYLE .
		"<title>$desc</title></head><body><pre><b>$desc</b>\n\n".
		qq!commit\t\t$x\n!);
	$fh->write($showmsg ? '</pre>' : "\n");
	my %acache;
	local $/ = "\0\0\n";
	my $nr = 0;
	my (@parents, %seen);
	while (defined(my $line = <$log>)) {
		my ($id, $p, $s, $D, $ai, $an, $b) = split("\0", $line);
		$seen{$id} = 1;
		my @p = split(' ', $p);
		push @parents, @p;

		$s = utf8_html($s);
		$s = qq(<a\nhref="${rel}commit?id=$id">$s</a>);
		if ($D =~ /\AD(.+)/) {
			$s .= ' ('. join(', ', git_dec_links($rel, $1)) . ')';
		}

		if (defined $b) {
			$an =~ s/\Aa//;
			$b =~ s/\Ab//;
			$b =~ s/\s*\z//s;

			my $ah = $acache{$an} ||= utf8_html($an);
			my $x = "<table><tr><td\nvalign=top><pre>$id";
			my $nl = $b eq '' ? '' : "\n"; # empty bodies :<
			$b = $x . '  </pre></td><td><pre>' .
				"<b>$s</b>\n- $ah @ $ai\n$nl" .
				utf8_html($b) . '</pre></td></tr></table>';
		} else {
			$b = qq($id\t$s\n);
		}
		$fh->write($b);
		++$nr;
	}

	my $m = '';
	my $np = 0;
	foreach my $p (@parents) {
		next if $seen{$p};
		$seen{$p} = ++$np;
		my $s = git_commit_title($git, $p);
		$m .= qq(\n<a\nhref="?h=$p$showmsg">$p</a>\t);
		$s = defined($s) ? utf8_html($s) : '';
		$m .= qq(<a\nhref="${rel}commit?id=$p">$s</a>);
	}
	my $foot = $showmsg ? "<pre>\t\t$x\n\n" : "\n\t\t$x\n\n";
	if ($np == 0) {
		$foot .= "No commits follow";
	} elsif ($np > 1) {
		$foot .= "Unseen parent commits to follow (multiple choice):\n";
	} else {
		$foot .= "Next parent to follow:\n";
	}
	$fh->write($foot .= $m . '</pre></body></html>');
}

1;
