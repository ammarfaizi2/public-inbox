# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /tag/ endpoint for git repositories
package PublicInbox::RepoGitTag;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use POSIX qw(strftime);
use PublicInbox::Hval qw(utf8_html);
use PublicInbox::Qspawn;

my %cmd_map = ( # type => action
	commit => 'commit',
	tag => 'tag',
	# tree/blob fall back to 'show'
);

sub call_git_tag {
	my ($self, $req) = @_;

	my $tip = $req->{tip};
	defined $tip or return git_tag_list($self, $req);
	sub {
		my ($res) = @_;
		git_tag_show($self, $req, $tip, $res);
	}
}

sub read_err {
	my ($fh, $type, $hex) = @_;

	$fh->write("</pre><hr /><pre><b>error reading $type $hex</b>");
}

sub git_show_tag_as_tag {
	my ($self, $fh, $req, $h, $cat, $left, $type, $hex) = @_;
	my $buf = '';
	my $offset = 0;
	while ($$left > 0) {
		my $r = read($cat, $buf, $$left, $offset);
		unless (defined $r) {
			read_err($fh, $type, $hex);
			last;
		}
		$offset += $r;
		$$left -= $r;
	}
	my $head;
	($head, $buf) = split(/\r?\n\r?\n/, $buf, 2);

	my %h = map { split(/[ \t]/, $_, 2) } split(/\r?\n/, $head);
	my $tag = utf8_html($h{tag});
	$type = $h{type} || '(unknown)';
	my $obj = $h{object};
	$h = $self->html_start($req, 'tag: ' . $tag);
	my $label = "$type $obj";
	my $cmd = $cmd_map{$type} || 'show';
	my $rel = $req->{relcmd};
	my $obj_link = qq(<a\nhref="$rel$cmd/$obj">$label</a>);
	$head = $h . "\n\n   tag <b>$tag</b>\nobject $obj_link\n";
	if (my $tagger = $h{tagger}) {
		$head .= 'tagger ' . join("\t", creator_split($tagger)) . "\n";
	}
	$fh->write($head . "\n");

	# n.b. tag subjects may not have a blank line after them,
	# but we bold the first line anyways
	my @buf = split(/\r?\n/s, $buf);
	if (defined(my $subj = shift @buf)) {
		$fh->write('<b>' . utf8_html($subj) . "</b>\n");

		$fh->write(utf8_html($_) . "\n") foreach @buf;
	}
}

sub git_tag_show {
	my ($self, $req, $h, $res) = @_;
	my $git = $req->{-repo}->{git};
	my $fh;
	my $hdr = ['Content-Type', 'text/html; charset=UTF-8'];

	# yes, this could still theoretically show anything,
	# but a tag could also point to anything:
	$git->cat_file("refs/tags/$h", sub {
		my ($cat, $left, $type, $hex) = @_;
		$fh = $res->([200, $hdr]);
		$h = PublicInbox::Hval->utf8($h);
		my $m = "git_show_${type}_as_tag";

		# git_show_tag_as_tag, git_show_commit_as_tag,
		# git_show_tree_as_tag, git_show_blob_as_tag
		if ($self->can($m)) {
			$self->$m($fh, $req, $h, $cat, $left, $type, $hex);
		} else {
			$self->unknown_tag_type($fh, $req, $h, $type, $hex);
		}
	});
	unless ($fh) {
		$fh = $res->([404, $hdr]);
		$fh->write(invalid_tag_start($req, $h));
	}
	$fh->write('</pre></body></html>');
	$fh->close;
}

sub invalid_tag_start {
	my ($self, $req, $h) = @_;
	my $rel = $req->{relcmd};
	$h = 'missing tag: ' . utf8_html($h);
	$self->html_start($req, $h) . "\n\n\t$h\n\n" .
		qq(see <a\nhref="${rel}tag">tag list</a> for valid tags.);
}

sub git_each_tag_sed ($$) {
	my ($self, $req) = @_;
	my $repo = $req->{-repo};
	my $buf = '';
	my $nr = 0;
	$req->{thtml} = $self->html_start($req, "$repo->{repo}: tag list") .
		'</pre><table><tr>' .
		join('', map { "<th><tt>$_</tt></th>" } qw(tag date subject)).
		'</tr>';
	sub {
		my $dst = delete $req->{thtml} || '';
		my $end = '';
		my @lines;
		if (defined $_[0]) {
			@lines = split(/\n/, $buf .= $_[0]);
			$buf = pop @lines if @lines;
		} else { # for-each-ref EOF
			@lines = split(/\n/, $buf);
			$buf = undef;
			if ($nr == $req->{-tag_count}) {
				$end = "<pre>Showing the latest $nr tags</pre>";
			} elsif ($nr == 0) {
				$end = '<pre>no tags to show</pre>';
			}
			$end = "</table>$end</body></html>";
		}
		for (@lines) {
			my ($ref, $date, $s) = split(' ', $_, 3);
			++$nr;
			$ref =~ s!\Arefs/tags/!!;
			$ref = PublicInbox::Hval->utf8($ref);
			my $h = $ref->as_html;
			$ref = $ref->as_href;
			$dst .= qq(<tr><td><tt>) .
				qq(<a\nhref="tag/$ref"><b>$h</b></a>) .
				qq(</tt></td><td><tt>$date</tt></td><td><tt>) .
				utf8_html($s) . '</tt></td></tr>';
		}
		$dst .= $end;
	}
}

sub git_tag_list {
	my ($self, $req) = @_;
	my $git = $req->{-repo}->{git};

	# TODO: use Xapian so we can more easily handle offsets/limits
	# for pagination instead of limiting
	my $count = $req->{-tag_count} = 50;
	my $cmd = $git->cmd(qw(for-each-ref --sort=-creatordate),
		'--format=%(refname) %(creatordate:short) %(subject)',
		"--count=$count", 'refs/tags/');
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	my $env = $req->{env};
	$env->{'qspawn.quiet'} = 1;
	$qsp->psgi_return($env, undef, sub { # parse output
		my ($r) = @_;
		if (!defined $r) {
			my $errmsg = $git->err;
			[ 500, [ 'Content-Type', 'text/html; charset=UTF-8'],
				[ $errmsg ] ];
		} else {
			$env->{'qspawn.filter'} = git_each_tag_sed($self, $req);
			[ 200, [ 'Content-Type', 'text/html; charset=UTF-8' ]];
		}
	});
}

sub unknown_tag_type {
	my ($self, $fh, $req, $h, $type, $hex) = @_;
	my $repo = $req->{-repo};
	$h = $h->as_html;
	my $rel = $req->{relcmd};
	my $label = "$type $hex";
	my $cmd = $cmd_map{$type} || 'show';
	my $obj_link = qq(<a\nhref="$rel$cmd/$hex">$label</a>\n);

	$fh->write($self->html_start($req,
				"$repo->{repo}: ref: $h") .
		"\n\n       <b>$h</b> (lightweight tag)\nobject $obj_link\n");
}

sub creator_split {
	my ($tagger) = @_;
	$tagger =~ s/\s*(\d+)(?:\s+([\+\-])?([ \d]{1,2})(\d\d))\z// or
		return ($tagger, 0);
	my ($tz_sign, $tz_H, $tz_M) = ($2, $3, $4);
	my $sec = $1;
	my $off = $tz_H * 3600 + $tz_M * 60;
	$off *= -1 if $tz_sign eq '-';
	my @time = gmtime($sec + $off);
	my $time = strftime('%Y-%m-%d %H:%M:%S', @time)." $tz_sign$tz_H$tz_M";

	(utf8_html($tagger), $time);
}

1;
