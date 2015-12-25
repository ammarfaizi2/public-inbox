# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)

# when no endpoints match, fallback to this and serve a static file
# This can serve Smart HTTP in the future.
package PublicInbox::RepoBrowseGitFallback;
use strict;
use warnings;
use base qw(PublicInbox::RepoBrowseBase);
use Fcntl qw(:seek);

# overrides PublicInbox::RepoBrowseBase::call
sub call {
	my ($self, undef, $req) = @_;
	my $expath = $req->{expath};
	return if index($expath, '..') >= 0; # prevent path traversal

	my $git = $req->{repo_info}->{git};
	my $f = "$git->{git_dir}/$expath";
	return unless -f $f && -r _;
	my @st = stat(_);
	my ($size, $mtime) = ($st[7], $st[9]);
	# TODO: if-modified-since and last-modified...
	open my $in, '<', $f or return;
	my $code = 200;
	my $len = $size;
	my @h;

	# FIXME: this is Plack-only
	my $range = eval { $req->{cgi}->{env}->{HTTP_RANGE} };
	if (defined $range && $range =~ /\bbytes=(\d*)-(\d*)\z/) {
		($code, $len) = prepare_range($req, $in, \@h, $1, $2, $size);
	}

	# we use the unsafe variant since we assume the server admin
	# would not place untrusted HTML/JS/CSS in the git directory
	my $type = $self->mime_type_unsafe($expath) || 'text/plain';
	push @h, 'Content-Type', $type, 'Content-Length', $len;
	sub {
		my ($res) = @_; # Plack callback
		my $fh = $res->([ $code, \@h ]);
		my $buf;
		my $n = 8192;
		while ($size > 0) {
			$n = $size if $size < $n;
			my $r = read($in, $buf, $n);
			last if (!defined($r) || $r <= 0);
			$fh->write($buf);
		}
		$fh->close;
	}
}

sub bad_range { [ 416, [], [] ] }

sub prepare_range {
	my ($req, $in, $h, $beg, $end, $size) = @_;
	my $code = 200;
	my $len = $size;
	if ($beg eq '') {
		if ($end ne '') { # last N bytes
			$beg = $size - $end;
			$beg = 0 if $beg < 0;
			$end = $size - 1;
			$code = 206;
		}
	} else {
		if ($end eq '' || $end >= $size) {
			$end = $size - 1;
			$code = 206;
		} elsif ($end < $size) {
			$code = 206;
		}
	}
	if ($code == 206) {
		$len = $end - $beg + 1;
		seek($in, $beg, SEEK_SET) or return [ 500, [], [] ];
		push @$h, qw(Accept-Ranges bytes),
				'Content-Range', "bytes $beg-$end/$size";

		# FIXME: Plack::Middleware::Deflater bug?
		$req->{cgi}->{env}->{'psgix.no-compress'} = 1;
	}
	($code, $len);
}

1;
