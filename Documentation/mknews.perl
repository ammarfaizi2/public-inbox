#!/usr/bin/perl -w
# Copyright (C) 2019 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Generates NEWS, NEWS.atom, and NEWS.html files using release emails
# this uses unstable internal APIs of public-inbox, and this script
# needs to be updated if they change.
use strict;
use PublicInbox::MIME;
use PublicInbox::View;
use Plack::Util;
use PublicInbox::MsgTime qw(msg_datestamp);
use PublicInbox::MID qw(mids mid_escape);
my $dst = shift @ARGV or die "Usage: $0 <NEWS|NEWS.atom|NEWS.html>";

# newest to oldest
my @releases = @ARGV;
my $dir = 'Documentation/RelNotes';
my $base_url = 'https://public-inbox.org/meta';
my $html_url = 'https://public-inbox.org/NEWS.html';
my $atom_url = 'https://public-inbox.org/NEWS.atom';
my $addr = 'meta@public-inbox.org';

my $latest = shift(@releases) or die 'no releases?';
my $mime_latest = release2mime($latest);
my $mtime = msg_datestamp($mime_latest->header_obj);
my $tmp = "$dst+";
my $out;
if ($dst eq 'NEWS') {
	open $out, '>', $tmp or die;
	mime2txt($out, $mime_latest);
	for my $v (@releases) {
		print $out "\n" or die;
		mime2txt($out, release2mime($v));
	}
} elsif ($dst eq 'NEWS.atom' || $dst eq 'NEWS.html') {
	open $out, '>', $tmp or die;
	my $ibx = Plack::Util::inline_object(
		description => sub { 'public-inbox releases' },
		over => sub { undef },
		search => sub { 1 }, # for WwwStream:_html_top
		base_url => sub { "$base_url/" },
	);
	$ibx->{-primary_address} = $addr;
	my $ctx = {
		-inbox => $ibx,
		-upfx => "$base_url/",
		-hr => 1,
	};
	if ($dst eq 'NEWS.html') {
		html_start($out, $ctx);
		mime2html($out, $mime_latest, $ctx);
		while (defined(my $v = shift(@releases))) {
			mime2html($out, release2mime($v), $ctx);
		}
		html_end($out, $ctx);
	} elsif ($dst eq 'NEWS.atom') {
		my $astream = atom_start($out, $ctx, $mtime);
		for my $v (reverse(@releases)) {
			mime2atom($out, $astream, release2mime($v), $ctx);
		}
		mime2atom($out, $astream, $mime_latest, $ctx);
		print $out '</feed>' or die;
	} else {
		die "BUG: Unrecognized $dst\n";
	}
} else {
	die "Unrecognized $dst\n";
}

close($out) or die;
utime($mtime, $mtime, $tmp) or die;
rename($tmp, $dst) or die;
exit 0;

sub release2mime {
	my $f = "$dir/$_[0].eml";
	open(my $fh, '<', $f) or die "open($f): $!";
	PublicInbox::MIME->new(do { local $/; <$fh> });
}

sub mime2txt {
	my ($out, $mime) = @_;
	my $title = $mime->header_str('Subject');
	$title =~ s/^\s*\[\w+\]\s*//g; # [ANNOUNCE] or [ANN]
	my $dtime = msg_datestamp($mime->header_obj);
	$title .= ' - ' . PublicInbox::View::fmt_ts($dtime) . ' UTC';
	print $out $title, "\n" or die;
	my $uline = '=' x length($title);
	print $out $uline, "\n\n" or die;

	my $mid = mids($mime)->[0];
	print $out 'Link: ', $base_url, '/', mid_escape($mid), "/\n\n" or die;
	print $out $mime->body_str or die;
}

sub mime2html {
	my ($out, $mime, $ctx) = @_;
	my $smsg = bless { mime => $mime }, 'PublicInbox::SearchMsg';
	print $out PublicInbox::View::index_entry($smsg, $ctx, 1) or die;
}

sub html_start {
	my ($out, $ctx) = @_;
	require PublicInbox::WwwStream;
	$ctx->{www} = Plack::Util::inline_object(style => sub { '' });
	my $www_stream = PublicInbox::WwwStream->new($ctx);
	print $out $www_stream->_html_top, '<pre>' or die;
}

sub html_end {
	print $out <<EOF or die;
	git clone $PublicInbox::WwwStream::CODE_URL
</pre></body></html>
EOF
}

sub atom_start {
	my ($out, $ctx, $mtime) = @_;
	require PublicInbox::WwwAtomStream;
	# WwwAtomStream stats this dir for mtime
	my $astream = PublicInbox::WwwAtomStream->new($ctx);
	delete $ctx->{emit_header};
	my $ibx = $ctx->{-inbox};
	my $title = PublicInbox::WwwAtomStream::title_tag($ibx->description);
	my $updated = PublicInbox::WwwAtomStream::feed_updated(gmtime($mtime));
	print $out <<EOF or die;
<?xml version="1.0" encoding="us-ascii"?>
<feed
xmlns="http://www.w3.org/2005/Atom"
xmlns:thr="http://purl.org/syndication/thread/1.0">$title<link
rel="alternate"
type="text/html"
href="$html_url"/><link
rel="self"
href="$atom_url"/><id>$atom_url</id>$updated
EOF
	$astream;
}

sub mime2atom  {
	my ($out, $astream, $mime, $ctx) = @_;
	my $smsg = bless { mime => $mime }, 'PublicInbox::SearchMsg';
	if (defined(my $str = $astream->feed_entry($smsg))) {
		print $out $str or die;
	}
}
