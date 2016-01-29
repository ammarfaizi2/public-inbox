# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# show log as an Atom feed
package PublicInbox::RepobrowseGitAtom;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepobrowseBase);
my $ATOM_FMT = '--pretty=tformat:'.
		join('%x00', qw(%s %ct %an %ae %at %h %H %b), '', '');

use constant DATEFMT => '%Y-%m-%dT%H:%M:%SZ';
use POSIX qw(strftime);

sub call_git_atom {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $max = $repo_info->{max_commit_count} || 10;
	$max = int($max);
	$max = 50 if $max == 0;

	my $git = $repo_info->{git};
	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $h = $q->{h};
	$h eq '' and chomp($h = $git->qx(qw(symbolic-ref --short HEAD)));

	my @cmd = (qw(log --no-notes --no-color --abbrev-commit),
			$git->abbrev, $ATOM_FMT, "-$max", $h, '--');
	push @cmd, $req->{expath} if length($req->{expath});
	my $log = $git->popen(@cmd);

	sub {
		my ($res) = @_; # Plack callback
		my @h = ( 'Content-Type' => 'application/atom+xml' );
		my $fh = $res->([200, \@h]);
		$self->git_atom_stream($req, $q, $log, $fh, $h);
		$fh->close;
	}
}

sub repo_root_url {
	my ($self, $req) = @_;
	my $cgi = $req->{cgi};
	my $uri = $cgi->request_uri;
	$uri =~ s/\?.+\z//; # no query string
	my @uri = split(m!/+!, $uri);
	shift @uri; # leading slash
	my @extra = @{$req->{extra}};
	while (@uri && @extra && $uri[-1] eq $extra[-1]) {
		pop @uri;
		pop @extra;
	}
	pop @uri if $uri[-1] eq 'atom'; # warn if not equal?
	$cgi->base . join('/', @uri);
}

sub git_atom_stream {
	my ($self, $req, $q, $log, $fh, $h) = @_;
	my $repo_info = $req->{repo_info};
	my $title = join('/', $repo_info->{repo}, @{$req->{extra}});
	$title = utf8_html("$title, branch $h");

	my $url = $self->repo_root_url($req);
	$fh->write(qq(<?xml version="1.0"?>\n) .
		qq(<feed\nxmlns="http://www.w3.org/2005/Atom">) .
		qq(<title>$title</title>) .
		qq(<subtitle>$repo_info->{desc_html}</subtitle>) .
		qq(<link\nrel="alternate"\ntype="text/html"\nhref="$url"\n/>));
	my $rel = $req->{relcmd};
	my %acache;
	local $/ = "\0";
	while (defined(my $s = <$log>)) {
		chomp $s;
		my $entry = '<entry><title>';
		$entry .= utf8_html($s); # commit subject
		$entry .= '</title><updated>';

		chomp($s = <$log>); # commit time
		$entry .= strftime(DATEFMT, gmtime($s));
		$entry .= '</updated><author><name>';

		chomp($s = <$log>); # author name
		$entry .= $acache{$s} ||= utf8_html($s);
		$entry .= '</name><email>';

		chomp($s = <$log>); # author email
		$entry .= $acache{$s} ||= utf8_html($s);
		$entry .= '</email></author><published>';

		chomp($s = <$log>); # author time
		$entry .= strftime(DATEFMT, gmtime($s));
		$entry .= '</published>';

		$entry .= qq(<link\nrel="alternate"\ntype="text/html"\nhref=");
		$entry .= $url;
		chomp($s = <$log>); # abbreviated commit hash for URL
		$entry .= qq(/commit?id=$s"\n/><id>);
		chomp($s = <$log>); # unabbreviated commit hash
		$entry .= $s;
		$entry .= qq(</id>);

		$entry .= qq(<content\ntype="xhtml"><div\nxmlns=");
		$entry .= qq(http://www.w3.org/1999/xhtml">);
		$entry .= qq(<pre\nstyle="white-space:pre-wrap">\n);
		chomp($s = <$log>);
		$entry .= utf8_html($s);  # body
		$fh->write($entry .= qq(</pre></div></content></entry>));
		eval {
			local $/ = "\0\n";
			$s = <$log>;
		};
	}
	$fh->write('</feed>');
}

1;
