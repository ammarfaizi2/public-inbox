# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# show log as an Atom feed
package PublicInbox::RepoGitAtom;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepoBase);
use PublicInbox::Qspawn;

use constant DATEFMT => '%Y-%m-%dT%H:%M:%SZ';
use constant STATES => qw(H ct an ae at s b);
use constant STATE_BODY => (scalar(STATES) - 1);
my $ATOM_FMT = '--pretty=tformat:'.
		join('%n', map { "%$_" } STATES).'%x00';
use POSIX qw(strftime);

sub repo_root_url {
	my ($self, $req) = @_;
	my $env = $req->{env};
	my $uri = $env->{REQUEST_URI};
	$uri =~ s/\?.+\z//; # no query string
	my @uri = split(m!/+!, $uri);
	my @extra = @{$req->{extra}};
	while (@uri && @extra && $uri[-1] eq $extra[-1]) {
		pop @uri;
		pop @extra;
	}
	pop @uri if $uri[-1] eq 'atom'; # warn if not equal?
	PublicInbox::Repobrowse::base_url($env) . join('/', @uri);
}

sub flush_hdr ($$$) {
	my ($dst, $hdr, $url) = @_;
	$$dst .= '<entry><title>';
	$$dst .= utf8_html($hdr->{'s'}); # commit subject
	$$dst .= '</title><updated>';
	$$dst .= strftime(DATEFMT, gmtime($hdr->{ct}));
	$$dst .= '</updated><author><name>';
	$$dst .= utf8_html($hdr->{an});
	$$dst .= '</name><email>';
	$$dst .= utf8_html($hdr->{ae});
	$$dst .= '</email></author><published>';
	$$dst .= strftime(DATEFMT, gmtime($hdr->{at}));
	$$dst .= '</published>';
	$$dst .= qq(<link\nrel="alternate"\ntype="text/html"\nhref=");
	$$dst .= $url;
	$$dst .= '/commit/';

	my $H = $hdr->{H};
	$$dst .= $H;
	$$dst .= qq("\n/><id>);
	$$dst .= $H;
	$$dst .= qq(</id>);

	$$dst .= qq(<content\ntype="xhtml"><div\nxmlns=");
	$$dst .= qq(http://www.w3.org/1999/xhtml">);
	$$dst .= qq(<pre\nstyle="white-space:pre-wrap">);
	undef
}

sub git_atom_sed ($$) {
	my ($self, $req) = @_;
	my $buf = '';
	my $state = 0;
	my $rel = $req->{relcmd};
	my $repo = $req->{-repo};
	my $tip = $repo->tip;
	my $title = join('/', $repo->{repo}, @{$req->{extra}});
	$title = utf8_html("$title, $tip");
	my $url = repo_root_url($self, $req);
	my $hdr = {};
	my $subtitle = $repo->desc_html;
	$req->{axml} = qq(<?xml version="1.0"?>\n) .
		qq(<feed\nxmlns="http://www.w3.org/2005/Atom">) .
		qq(<title>$title</title>) .
		qq(<subtitle>$subtitle</subtitle>) .
		qq(<link\nrel="alternate"\ntype="text/html"\nhref="$url"\n/>);
	my ($plinks, $ai);
	my $end = '';
	my $blines;
	sub {
		my $dst;
		# $_[0] == scalar buffer, undef means EOF from "git log"
		$dst = delete $req->{axml} || '';
		my @tmp;
		if (defined $_[0]) {
			$buf .= $_[0];
			@tmp = split(/\n/, $buf, -1);
			$buf = @tmp ? pop(@tmp) : '';
		} else {
			@tmp = split(/\n/, $buf, -1);
			$buf = '';
			$end = '</feed>';
		}

		foreach my $l (@tmp) {
			if ($state != STATE_BODY) {
				$hdr->{((STATES)[$state])} = $l;
				if (++$state == STATE_BODY) {
					flush_hdr(\$dst, $hdr, $url);
					$hdr = {};
					$blines = 0;
				}
				next;
			}
			if ($l eq "\0") {
				$dst .= qq(</pre></div></content></entry>);
				$state = 0;
			} else {
				$dst .= "\n" if $blines++;
				$dst .= utf8_html($l);
			}
		}
		$dst .= $end;
	}
}

sub git_atom_cb {
	my ($self, $req) = @_;
	sub {
		my ($r) = @_;
		my $env = $req->{env};
		if (!defined $r) {
			my $git = $req->{-repo}->{git};
			return [ 400, [ 'Content-Type', 'text/plain' ],
				[ $git->err ] ];
		}
		$env->{'qspawn.filter'} = git_atom_sed($self, $req);
		[ 200, [ 'Content-Type', 'application/atom+xml' ] ];
	}
}

sub call_git_atom {
	my ($self, $req) = @_;
	my $repo = $req->{-repo};
	my $max = $repo->{max_commit_count} || 10;
	$max = int($max);
	$max = 50 if $max == 0;

	my $git = $repo->{git};
	my $env = $req->{env};
	my $tip = $req->{tip} || $repo->tip;
	my $read_log = sub {
		my $cmd = $git->cmd(qw(log --no-notes --no-color
					--abbrev-commit), $git->abbrev,
					$ATOM_FMT, "-$max", $tip, '--');
		my $expath = $req->{expath};
		push @$cmd, $expath if $expath ne '';
		my $rdr = { 2 => $git->err_begin };
		my $qsp = PublicInbox::Qspawn->new($cmd, undef, undef, $rdr);
		$qsp->psgi_return($env, undef, git_atom_cb($self, $req));
	};

	sub {
		$env->{'qspawn.response'} = $_[0];
		$read_log->();
	}
}

1;
