# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Show a blob as-is
package PublicInbox::RepoBrowseGitBlob;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);

sub call_git_blob {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $to_read = 8000; # git uses this size to detect binary files
	my $q = PublicInbox::RepoBrowseQuery->new($req->{cgi});
	my $id = $q->{id};
	$id eq '' and $id = 'HEAD';

	if (length(my $expath = $req->{expath})) {
		$id .= ":$expath";
	}
	my ($cat, $hex, $type, $size) = $git->cat_file_begin($id);
	return unless defined $cat;

	my ($r, $buf);
	my $left = $size;
	if ($type eq 'blob') {
		my $base = $req->{extra}->[-1];
		$type = $self->mime_type($base) if defined $base;
		unless ($type) {
			$to_read = $left if $to_read > $left;
			$r = read($cat, $buf, $to_read);
			if (!defined $r || $r <= 0) {
				$git->cat_file_finish($left);
				return;
			}
			$left -= $r;
			$type = (index($buf, "\0") < 0) ?
				'text/plain' :
				'application/octet-stream';
		}
	} elsif ($type eq 'commit' || $type eq 'tag') {
		$type = 'text/plain';
	} else {
		$type = 'application/octet-stream';
	}

	sub {
		my ($res) = @_;
		eval {
			my $fh = $res->([ 200, ['Content-Length' => $size,
						'Content-Type' => $type]]);
			$fh->write($buf) if defined $buf;
			while ($left > 0) {
				$to_read = $left if $to_read > $left;
				$r = read($cat, $buf, $to_read);
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
