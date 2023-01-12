# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# cgit-compatible $REPO/tree/[PATH]?h=$tip redirector
package PublicInbox::RepoTree;
use v5.12;
use PublicInbox::ViewDiff qw(uri_escape_path);
use PublicInbox::GitAsyncCat;
use PublicInbox::WwwStatic qw(r);

sub tree_30x { # git check_async callback
	my ($oid, $type, $size, $ctx) = @_;
	my $wcb = delete $ctx->{-wcb};
	return $wcb->(r(404)) if $type eq 'missing';
	my $u = $ctx->{git}->base_url($ctx->{env});
	my $path = uri_escape_path(delete $ctx->{-path});
	$u .= "$oid/s/?b=$path";
	$wcb->([ 302, [ Location => $u, 'Content-Type' => 'text/plain' ],
		[ "Redirecting to $u\n" ] ])
}

sub srv_tree {
	my ($ctx, $path) = @_;
	return if index($path, '//') >= 0 || index($path, '/') == 0;
	my $tip = $ctx->{qp}->{h} // 'HEAD';
	$path =~ s!/\z!!;
	my $obj = $ctx->{-obj} = "$tip:$path";
	$ctx->{-path} = $path;

	# "\n" breaks with `git cat-file --batch-check', and there's no
	# legitimate use of "\n" in filenames anyways.
	return if index($obj, "\n") >= 0;
	sub {
		$ctx->{-wcb} = $_[0]; # HTTP::{Chunked,Identity}
		if ($ctx->{env}->{'pi-httpd.async'}) {
			async_check($ctx, $obj, \&tree_30x, $ctx);
		} else {
			$ctx->{git}->check_async($obj, \&tree_30x, $ctx);
			$ctx->{git}->async_wait_all;
		}
	};
}

1;
