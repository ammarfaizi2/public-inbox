# Copyright (C) 2015-2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Show a blob as-is
package PublicInbox::RepoGitBlob;
use strict;
use warnings;
use base qw(PublicInbox::RepoBase);
use base qw(Exporter);
our @EXPORT = qw(git_blob_mime_type git_blob_stream_response);

sub call_git_blob {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $id = $req->{-tip} . ':' . $req->{expath};

	my ($cat, $hex, $type, $size) = $git->cat_file_begin($id);
	return unless defined $cat;

	my ($r, $buf);
	my $left = $size;
	if ($type eq 'blob') {
		$type = git_blob_mime_type($self, $req, $cat, \$buf, \$left);
	} elsif ($type eq 'commit' || $type eq 'tag') {
		$type = 'text/plain; charset=UTF-8';
	} else {
		$type = 'application/octet-stream';
	}
	git_blob_stream_response($git, $cat, $size, $type, $buf, $left);
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
		my $git = $req->{repo_info}->{git};
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
			my $fh = $res->([ 200, ['Content-Length' => $size,
						'Content-Type' => $type]]);
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
