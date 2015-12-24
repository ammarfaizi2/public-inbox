# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoBrowseBase;
use strict;
use warnings;
require PublicInbox::RepoBrowseQuery;
require PublicInbox::Hval;

sub new { bless {}, shift }

sub call {
	my ($self, $cmd, $req) = @_;
	my $vcs = $req->{repo_info}->{vcs};
	my $rv = eval {
		no strict 'refs';
		my $sub = "call_${vcs}_$cmd";
		$self->$sub($req);
	};
	$@ ? [ 500, ['Content-Type'=>'text/plain'], [] ] : $rv;
}

sub mime_load {
	my ($self, $file) = @_;
	my %rv;
	open my $fh, '<', $file or return \%rv;
	foreach (<$fh>) {
		next if /^#/; # no comments
		my ($type, @ext) = split(/\s+/);

		# XSS protection.  Assume the browser knows what to do
		# with images/audio/video...
		if (defined $type && $type =~ m!\A(?:image|audio|video)/!) {
			$rv{$_} = $type foreach @ext;
		}
	}
	\%rv;
}

# returns undef if missing, so users can scan the blob if needed
sub mime_type {
	my ($self, $fn) = @_;
	$fn =~ /\.([^\.]+)\z/ or return;
	my $ext = $1;
	my $m = $self->{mime_types} ||= $self->mime_load('/etc/mime.types');

	$m->{$ext};
}

1;
