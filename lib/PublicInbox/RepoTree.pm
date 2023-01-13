# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# cgit-compatible $REPO/tree/[PATH]?h=$tip redirector
package PublicInbox::RepoTree;
use v5.12;
use PublicInbox::ViewDiff qw(uri_escape_path);
use PublicInbox::WwwStatic qw(r);
use PublicInbox::Qspawn;
use PublicInbox::WwwStream qw(html_oneshot);
use PublicInbox::Hval qw(ascii_html);

sub rd_404_log {
	my ($bref, $ctx) = @_;
	my $path = $ctx->{-q_value_html} = ascii_html($ctx->{-path});
	my $tip = 'HEAD';
	$tip = ascii_html($ctx->{qp}->{h}) if defined($ctx->{qp}->{h});
	PublicInbox::WwwStream::html_init($ctx);
	my $zfh = $ctx->{zfh};
	print $zfh "<pre>\$ git log -1 $tip -- $path\n";
	if ($$bref eq '') {
		say $zfh "found no record of `$path' in git history";
		$ctx->{-has_srch} and
			say $zfh 'perhaps try searching mail (above)';
	} else {
		my ($H, $h, $s_as) = split(/ /, $$bref, 3);
		utf8::decode($s_as);
		my $x = uri_escape_path($ctx->{-path});
		$s_as = ascii_html($s_as);
		print $zfh <<EOM;
found last record of `$path' in the following commit:

<a href="$ctx->{-upfx}$H/s/?b=$x">$h</a> $s_as
EOM
	}
	delete($ctx->{-wcb})->($ctx->html_done);
}

sub find_missing {
	my ($ctx) = @_;
	my $cmd = ['git', "--git-dir=$ctx->{git}->{git_dir}",
		qw(log --no-color -1), '--pretty=%H %h %s (%as)' ];
	push @$cmd, $ctx->{qp}->{h} if defined($ctx->{qp}->{h});
	push @$cmd, '--';
	push @$cmd, $ctx->{-path} if $ctx->{-path} ne '';
	my $qsp = PublicInbox::Qspawn->new($cmd);
	$qsp->psgi_qx($ctx->{env}, undef, \&rd_404_log, $ctx);
}

sub tree_show { # git check_async callback
	my ($oid, $type, $size, $ctx) = @_;
	return find_missing($ctx) if $type eq 'missing';

	open $ctx->{lh}, '<', \(my $dbg_log = '') or die "open(scalar): $!";
	my $res = [ $ctx->{git}, $oid, $type, $size ];
	my ($bn) = ($ctx->{-path} =~ m!/?([^/]+)\z!);
	if ($type eq 'blob') {
		my $obj = ascii_html($ctx->{-obj});
		$ctx->{-paths} = [ $bn, qq[(<a
href="$ctx->{-upfx}$oid/s/$bn">raw</a>)
\$ git show $obj\t# shows this blob on the CLI] ];
	}
	PublicInbox::ViewVCS::solve_result($res, $ctx);
}

sub srv_tree {
	my ($ctx, $path) = @_;
	return if index($path, '//') >= 0 || index($path, '/') == 0;
	my $tip = $ctx->{qp}->{h} // 'HEAD';
	$ctx->{-upfx} = '../' x (($path =~ tr!/!/!) + 1);
	$path =~ s!/\z!!;
	my $obj = $ctx->{-obj} = "$tip:$path";
	$ctx->{-path} = $path;

	# "\n" breaks with `git cat-file --batch-check', and there's no
	# legitimate use of "\n" in filenames anyways.
	return if index($obj, "\n") >= 0;
	sub {
		$ctx->{-wcb} = $_[0]; # HTTP::{Chunked,Identity}
		PublicInbox::ViewVCS::do_check_async($ctx, \&tree_show, $obj);
	};
}

1;
