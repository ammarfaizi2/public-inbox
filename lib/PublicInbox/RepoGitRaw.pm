# Copyright (C) 2015-2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoGitRaw;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Hval qw(utf8_html);
use PublicInbox::Qspawn;
my $MAX_ASYNC = 65536;
my $BIN_DETECT = 8000;

sub git_raw_check_res ($$$) {
	my ($self, $req, $res) = @_;
	sub {
		my ($info) = @_;
		my ($hex, $type, $size) = @$info;
		if (!defined $type || $type eq 'missing') {
			return $res->($self->rt(404, 'plain', 'Not Found'));
		}
		my $ct;
		if ($type eq 'blob') {
			my $base = $req->{extra}->[-1];
			$ct = $self->mime_type($base) if defined $base;
			$ct ||= 'text/plain; charset=UTF-8' if !$size;
		} elsif ($type eq 'commit' || $type eq 'tag' ||
				$type eq 'tree') {
			return git_tree_raw($self, $req, $res, $hex);
		} else { # hmm..., just in case
			$ct = 'application/octet-stream';
		}

		$size > $MAX_ASYNC and
			return show_big($self, $req, $res, $ct, $info);

		# buffer small files in full
		my $buf = '';
		$req->{-repo}->{git}->cat_async($req->{env}, $hex, sub {
			my ($r) = @_;
			if (ref($r) eq 'SCALAR') {
				$buf .= $$r;
			} elsif ($r == 0) {
				return if bytes::length($buf) < $size;
				$ct ||= index($buf, "\0") >= 0 ?
						'application/octet-stream' :
						'text/plain; charset=UTF-8';
				$res->([200, ['Content-Type', $ct,
						'Content-Length', $size ],
					[ $buf ]]);
			}
		});
	}
}

sub call_git_raw {
	my ($self, $req) = @_;
	my $repo = $req->{-repo};
	my $obj = $req->{tip} || $repo->tip;
	my $expath = $req->{expath};
	$obj .= ":$expath" if $expath ne '';
	sub {
		my ($res) = @_;
		$repo->{git}->check_async($req->{env}, $obj,
			git_raw_check_res($self, $req, $res));
	}
}

sub git_tree_sed ($) {
	my ($req) = @_;
	my $buf = '';
	my $end = '';
	my $pfx = $req->{tpfx};
	sub { # $_[0] = buffer or undef
		my $dst = delete $req->{tstart} || '';
		my @files;
		if (defined $_[0]) {
			@files = split(/\0/, $buf .= $_[0]);
			$buf = pop @files if scalar @files;
		} else {
			@files = split(/\0/, $buf);
			$end = '</ul></body></html>';
		}
		foreach my $n (@files) {
			$n = PublicInbox::Hval->utf8($n);
			my $ref = $n->as_path;
			$dst .= qq(<li><a\nhref="$pfx$ref">);
			$dst .= $n->as_html;
			$dst .= '</a></li>';
		}
		$dst .= $end;
	}
}

sub git_tree_raw {
	my ($self, $req, $res, $hex) = @_;

	my @ex = @{$req->{extra}};
	my $rel = $req->{relcmd};
	my $title = utf8_html(join('/', '', @ex, ''));
	my $repo = $req->{-repo};
	my $pfx = ($req->{tip} || $repo->tip) . '/';
	my $t = "<h2>$title</h2><ul>";
	if (@ex) {
		$t .= qq(<li><a\nhref="./">../</a></li>);
		my $last = PublicInbox::Hval->utf8($ex[-1])->as_href;
		$pfx = "$last/";
	}

	$req->{tpfx} = $pfx;
	$req->{tstart} = "<html><head><title>$title</title></head><body>".$t;
	my $git = $repo->{git};
	my $cmd = $git->cmd(qw(ls-tree --name-only -z), $hex);
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	my $env = $req->{env};
	$env->{'qspawn.response'} = $res;
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		if (!defined $r) {
			$self->rt(500, 'plain', $git->err);
		} else {
			$env->{'qspawn.filter'} = git_tree_sed($req);
			$self->rt(200, 'html');
		}
	});
}

sub show_big {
	my ($self, $req, $res, $ct, $info) = @_;
	my ($hex, $type, $size) = @$info;
	my $env = $req->{env};
	my $git = $req->{-repo}->{git};
	my $rdr = { 2 => $git->err_begin };
	my $cmd = $git->cmd('cat-file', $type, $hex);
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	$env->{'qspawn.response'} = $res;
	my @cl = ('Content-Length', $size);
	$qsp->psgi_return($env, undef, sub {
		my ($r, $bref) = @_;
		if (!defined $r) {
			$self->rt(500, 'plain', $git->err);
		} elsif (defined $ct) {
			[ 200, [ 'Content-Type', $ct, @cl ] ];
		} else {
			return $self->rt(200, 'plain');
			if (index($$bref, "\0") >= 0) {
				$ct = 'application/octet-stream';
				return [200, ['Content-Type', $ct, @cl ] ];
			}
			my $n = bytes::length($$bref);
			if ($n >= $BIN_DETECT || $n == $size) {
				$ct ||= 'text/plain; charset=UTF-8';
				return [200, ['Content-Type', $ct, @cl] ];
			}
			# else: bref will keep growing...
		}
	});
}

1;
