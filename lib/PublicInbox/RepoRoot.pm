# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# displays the root '/' where all the projects lie
package PublicInbox::RepoRoot;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Hval qw(utf8_html);

sub call {
	my ($self, $rconfig) = @_;
	sub {
		my ($res) = @_; # PSGI callback
		my @h = ('Content-Type', 'text/html; charset=UTF-8');
		my $fh = $res->([200, \@h]);
		repobrowse_index($fh, $rconfig);
		$fh->close;
	}
}

sub repobrowse_index {
	my ($fh, $rconfig) = @_;
	my $title = 'repobrowse index';
	$fh->write("<html><head><title>$title</title>" .
			PublicInbox::Hval::STYLE .
			"</head><body><pre><b>$title</b>");

	# preload all groups
	foreach my $k (sort keys %$rconfig) {
		$k =~ /\Arepo\.(.+)\.path\z/ or next;
		my $repo_path = $1;
		$rconfig->lookup($repo_path); # insert into groups
	}

	my $groups = $rconfig->{-groups};
	if (scalar(keys %$groups) > 2) { # default has '-none' + '-hidden'
		$fh->write("\n\n<b>uncategorized</b></pre>".
			"<table\nsummary=repoindex>");
	} else {
		$fh->write("</pre><table\nsummary=repoindex>");
	}
	foreach my $repo_path (sort @{$groups->{-none}}) {
		my $r = $rconfig->lookup($repo_path);
		my $p = PublicInbox::Hval->utf8($r->{repo});
		my $l = $p->as_html;
		$p = $p->as_path;
		$fh->write(qq(<tr><td><tt><a\nhref="$p">$l</a></tt></td>) .
			'<td><tt> '.$r->desc_html.'</tt></td></tr>');
	}

	foreach my $group (keys %$groups) {
		next if $group =~ /\A-(?:none|hidden)\z/;
		my $g = utf8_html($group);
		$fh->write("<tr><td><pre> </pre></td></tr>".
			"<tr><td><pre><b>$g</b></pre></tr>");
		foreach my $repo_path (sort @{$groups->{$group}}) {
			my $r = $rconfig->lookup($repo_path);
			my $p = PublicInbox::Hval->utf8($r->{repo});
			my $l = $p->as_html;
			$p = $p->as_path;
			$fh->write('<tr><td><tt> ' .
				qq(<a\nhref="$p">$l</a></tt></td>) .
				'<td><tt> '.$r->desc_html.'</tt></td></tr>');
		}
	}

	$fh->write('</table></body></html>');
}

1;
