# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoBase;
use strict;
use warnings;
require PublicInbox::RepoGitQuery;
use PublicInbox::Hval;
our %MIME_TYPE_WHITELIST = ('application/pdf' => 1);

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
	while (<$fh>) {
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
	return unless defined $ct;

	# XSS protection.  Assume the browser knows what to do
	# with images/audio/video; but don't allow random HTML from
	# a repository to be served
	($ct =~ m!\A(?:image|audio|video)/! || $MIME_TYPE_WHITELIST{$ct}) ?
		$ct : undef;
}

# starts an HTML page for Repobrowse in a consistent way
sub html_start {
	my ($self, $req, $title_html, $opts) = @_;
	my $desc = $req->{repo_info}->desc_html;
	my $meta = '';

	if ($opts) {
		my @robots;
		foreach (qw(nofollow noindex)) {
			push @robots, $_ if $opts->{$_};
		}
		$meta = qq(<meta\nname=robots\ncontent=") .
			join(',', @robots) . '" />';
	}

	"<html><head><title>$title_html</title>" .
		PublicInbox::Hval::STYLE . $meta .
		"</head><body><pre><b>$desc</b>";
}

sub r {
	my ($self, $status, $req, @extra) = @_;
	my @h;

	my $body = '';
	if ($status == 301 || $status == 302) {
		# The goal is to be able to make redirects like we make
		# <a href=> tags with '../'
		my $env = $req->{env};
		my $base = PublicInbox::Repobrowse::base_url($env);
		my ($redir) = @extra;
		if ($redir =~ m!\A\.\./!) { # relative redirect
			my @orig = split(m!/+!, $env->{PATH_INFO});
			my @dest = split(m!/+!, $redir);

			while ($dest[0] eq '..') {
				pop @orig;
				shift @dest;
			}
			my $end = '';
			$end = pop @dest if $dest[-1] =~ /\A[#\?]/;
			$redir = $base . join('/', @orig, @dest) . $end;
		} else {
			$redir = $base . '/' . $redir;
		}
		push @h, qw(Content-Type text/plain Location), $redir;

		# mainly for curl (no-'-L') users:
		$body = "Redirecting to $redir\n";
	} else {
		push @h, qw(Content-Type text/plain);
	}

	[ $status, \@h, [ $body ] ]
}

1;
