# Copyright (C) 2015-2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoGitRaw;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use PublicInbox::Hval qw(utf8_html);
use PublicInbox::Qspawn;

sub call_git_raw {
	my ($self, $req) = @_;
	my $repo = $req->{-repo};
	my $git = $repo->{git};
	my $tip = $req->{tip} || $repo->tip;
	my $id = $tip . ':' . $req->{expath};
	my ($cat, $hex, $type, $size) = $git->cat_file_begin($id);
	return unless defined $cat;

	my ($r, $buf);
	my $left = $size;
	if ($type eq 'blob') {
		$type = git_blob_mime_type($self, $req, $cat, \$buf, \$left);
	} elsif ($type eq 'commit' || $type eq 'tag') {
		$type = 'text/plain; charset=UTF-8';
	} elsif ($type eq 'tree') {
		$git->cat_file_finish($left);
		return git_tree_raw($self, $req, $git, $hex);
	} else {
		$type = 'application/octet-stream';
	}
	git_blob_stream_response($git, $cat, $size, $type, $buf, $left);
}

sub git_tree_sed ($) {
	my ($req) = @_;
	my $buf = '';
	my $end;
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
		$end ? $dst .= $end : $dst;
	}
}

# This should follow the cgit DOM structure in case anybody depends on it,
# not using <pre> here as we don't expect people to actually view it much
sub git_tree_raw {
	my ($self, $req, $git, $hex) = @_;

	my @ex = @{$req->{extra}};
	my $rel = $req->{relcmd};
	my $title = utf8_html(join('/', '', @ex, ''));
	my $pfx = 'raw/';
	my $t = "<h2>$title</h2><ul>";
	if (@ex) {
		$t .= qq(<li><a\nhref="./">../</a></li>);
		my $last = PublicInbox::Hval->utf8($ex[-1])->as_href;
		$pfx = "$last/";
	}

	$req->{tpfx} = $pfx;
	$req->{tstart} = "<html><head><title>$title</title></head><body>".$t;
	my $cmd = $git->cmd(qw(ls-tree --name-only -z), $git->abbrev, $hex);
	my $rdr = { 2 => $git->err_begin };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	my $env = $req->{env};
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

sub git_blob_mime_type {
	my ($self, $req, $cat, $buf, $left) = @_;
	my $base = $req->{extra}->[-1];
	my $type = $self->mime_type($base) if defined $base;
	return $type if $type;

	my $to_read = 8000; # git uses this size to detect binary files
	$to_read = $$left if $to_read > $$left;
	my $r = read($cat, $$buf, $to_read);
	if (!defined $r || $r <= 0) {
		my $git = $req->{-repo}->{git};
		$git->cat_file_finish($$left);
		return;
	}
	$$left -= $r;
	(index($buf, "\0") < 0) ? 'text/plain; charset=UTF-8'
				: 'application/octet-stream';
}

sub git_blob_stream_response {
	my ($git, $cat, $size, $type, $buf, $left) = @_;

	sub {
		my ($res) = @_;
		my $to_read = 8192;
		eval {
			my $fh = $res->([ 200, ['Content-Length', $size,
						'Content-Type', $type]]);
			$fh->write($buf) if defined $buf;
			while ($left > 0) {
				$to_read = $left if $to_read > $left;
				my $r = read($cat, $buf, $to_read);
				last if (!defined $r || $r <= 0);
				$left -= $r;
				$fh->write($buf);
			}
			$fh->close;
		};
		$git->cat_file_finish($left);
	}
}


1;
