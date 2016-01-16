# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# show the log view
package PublicInbox::RepobrowseGitLog;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::RepobrowseGit qw(git_dec_links git_commit_title);
# cannot rely on --date=format-local:... yet, it is too new (September 2015)
my $LOG_FMT = '--pretty=tformat:'.
		join('%x00', qw(%h %p %s D%D %ai a%an b%b), '', '');

sub call_git_log {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $max = $repo_info->{max_commit_count} || 50;
	$max = int($max);
	$max = 50 if $max == 0;

	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $h = $q->{h};
	$h eq '' and $h = 'HEAD';

	my $git = $repo_info->{git};
	my $log = $git->popen(qw(log --no-notes --no-color --abbrev-commit),
				$git->abbrev, $LOG_FMT, "-$max", $h, '--');
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([200, ['Content-Type'=>'text/html']]);
		my $title = "log: $repo_info->{repo} ".utf8_html("($h)");
		$fh->write($self->html_start($req, $title));
		git_log_stream($req, $q, $log, $fh, $git);
		$fh->close;
	}
}

sub git_log_stream {
	my ($req, $q, $log, $fh, $git) = @_;

	my $rel = $req->{relcmd};
	my %acache;
	local $/ = "\0\0\n";
	my $nr = 0;
	my (@parents, %seen);
	while (defined(my $line = <$log>)) {
		my ($id, $p, $s, $D, $ai, $an, $b) = split("\0", $line);
		$seen{$id} = 1;
		my @p = split(' ', $p);
		push @parents, @p;
		my $plinks;
		if (@p == 1) { # typical, single-parent commit
			$plinks = qq( / parent <a\nhref="#p$p[0]">$p[0]</a>);
		} elsif (@p > 0) { # merge commit
			$plinks = ' / parents ' . join(' ', map {
				qq(<a\nhref="#p$_">$_</a>);
				} @p);
		} else {
			$plinks = ''; # root commit
		}

		$s = utf8_html($s);
		$s = qq(<a\nid=p$id\nhref="${rel}commit?id=$id"><b>$s</b></a>);
		if ($D =~ /\AD(.+)/) {
			$s .= ' ('. join(', ', git_dec_links($rel, $1)) . ')';
		}

		$an =~ s/\Aa//;
		$b =~ s/\Ab//;
		$b =~ s/\s*\z//s;

		my $ah = $acache{$an} ||= utf8_html($an);
		my $nl = $b eq '' ? '' : "\n"; # empty bodies :<
		$b = "$s\n- $ah @ $ai\n  commit $id$plinks\n$nl" .
			utf8_html($b);
		$fh->write("\n\n" .$b);
		++$nr;
	}

	my $m = '';
	my $np = 0;
	foreach my $p (@parents) {
		next if $seen{$p};
		$seen{$p} = ++$np;
		my $s = git_commit_title($git, $p);
		$m .= qq(\n<a\nid=p$p\nhref="?h=$p">$p</a>\t);
		$s = defined($s) ? utf8_html($s) : '';
		$m .= qq(<a\nhref="${rel}commit?id=$p">$s</a>);
	}
	my $foot = "</pre><hr /><pre>";
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
