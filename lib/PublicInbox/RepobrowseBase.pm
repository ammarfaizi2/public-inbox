# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepobrowseBase;
use strict;
use warnings;
require PublicInbox::RepobrowseQuery;
use PublicInbox::Hval;

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

		if (defined $type) {
			$rv{$_} = $type foreach @ext;
		}
	}
	\%rv;
}

# returns undef if missing, so users can scan the blob if needed
sub mime_type_unsafe {
	my ($self, $fn) = @_;
	$fn =~ /\.([^\.]+)\z/ or return;
	my $ext = $1;
	my $m = $self->{mime_types} ||= $self->mime_load('/etc/mime.types');
	$m->{$ext};
}

sub mime_type {
	my ($self, $fn) = @_;
	my $ct = $self->mime_type_unsafe($fn);

	# XSS protection.  Assume the browser knows what to do
	# with images/audio/video; but don't allow random HTML from
	# a repository to be served
	(defined($ct) && $ct =~ m!\A(?:image|audio|video)/!) ? $ct : undef;
}

# starts an HTML page for Repobrowse in a consistent way
sub html_start {
	my ($self, $req, $title_html) = @_;
	my $desc = $req->{repo_info}->{desc_html};

	"<html><head><title>$title_html</title>" .
		PublicInbox::Hval::STYLE .
		"</head><body><pre><b>$desc</b>";
}

1;
