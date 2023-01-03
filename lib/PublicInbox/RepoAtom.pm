# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# git log => Atom feed (cgit-compatible: $REPO/atom/[PATH]?h=$tip
package PublicInbox::RepoAtom;
use v5.12;
use parent qw(PublicInbox::GzipFilter);
use POSIX qw(strftime);
use URI::Escape qw(uri_escape);
use Scalar::Util ();
use PublicInbox::Hval qw(ascii_html);

my $ATOM_FMT = '--pretty=tformat:'.join('%n',
				map { "%$_" } qw(H ct an ae at s b)).'%x00';

sub log2atom_ok { # parse_hdr for qspawn
	my ($r, $bref, $ctx) = @_;
	return [ 404, [], [ "Not Found\n"] ] if $r == 0;
	bless $ctx, __PACKAGE__;
	my $h = [ 'Content-Type' => 'application/atom+xml; charset=UTF-8' ];
	$ctx->{gz} = $ctx->can('gz_or_noop')->($h, $ctx->{env});
	my $title = ascii_html(delete $ctx->{-feed_title});
	my $desc = ascii_html($ctx->{git}->description);
	my $url = ascii_html($ctx->{git}->base_url($ctx->{env}));
	$ctx->{-base_url} = $url;
	$ctx->zmore(<<EOM);
<?xml version="1.0"?>
<feed xmlns="http://www.w3.org/2005/Atom">
<title>$title</title><subtitle>$desc</subtitle><link
rel="alternate" type="text/html" href="$url"/>
EOM
	[ 200, $h, $ctx ]; # [2] is qspawn.filter
}

# called by GzipFilter->close
sub zflush { $_[0]->SUPER::zflush('</feed>') }

# called by GzipFilter->write or GetlineBody->getline
sub translate {
	my $self = shift;
	my $rec = $_[0] // return $self->zflush; # getline
	my @out;
	my $lbuf = delete($self->{lbuf}) // shift;
	$lbuf .= shift if @_;
	while ($lbuf =~ s/\A([^\0]+)\0\n//s) {
		my $ent = $1;
		utf8::decode($ent);
		$ent = ascii_html($ent);
		my ($H, $ct, $an, $ae, $at, $s, $bdy) = split(/\n/, $ent, 7);
		undef $ent;
		$bdy //= '';
		$_ = strftime('%Y-%m-%dT%H:%M:%SZ', gmtime($_)) for ($ct, $at);

		push @out, <<"", $bdy, '</pre></div></content></entry>'
<entry><title>$s</title><updated>$ct</updated><author><name>$an</name>
<email>$ae</email></author><published>$at</published><link
rel="alternate" type="text/html" href="$self->{-base_url}$H/s/"
/><id>$H</id><content type="xhtml"><div
xmlns="http://www.w3.org/1999/xhtml"><pre style="white-space:pre-wrap">

	}
	$self->{lbuf} = $lbuf;
	chomp @out;
	$self->SUPER::translate(@out);
}

sub srv_atom {
	my ($ctx, $path) = @_;
	return if index($path, '//') >= 0 || index($path, '/') == 0;
	my $max = 50; # TODO configurable
	my @cmd = ('git', "--git-dir=$ctx->{git}->{git_dir}",
			qw(log --no-notes --no-color --no-abbrev),
			$ATOM_FMT, "-$max");
	my $tip = $ctx->{qp}->{h}; # same as cgit
	$ctx->{-feed_title} = $ctx->{git}->{nick};
	if (defined($tip)) {
		push @cmd, $tip;
		$ctx->{-feed_title} .= ", $tip";
	}
	# else: let git decide based on HEAD if $tip isn't defined
	push @cmd, '--';
	push @cmd, $path if $path ne '';
	my $qsp = PublicInbox::Qspawn->new(\@cmd);
	$qsp->psgi_return($ctx->{env}, undef, \&log2atom_ok, $ctx);
}

1;
