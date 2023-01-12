# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# HTML body stream for which yields getline+close methods for
# generic PSGI servers and callbacks for public-inbox-httpd.
#
# See PublicInbox::GzipFilter parent class for more info.
package PublicInbox::WwwStream;
use strict;
use v5.10.1;
use parent qw(Exporter PublicInbox::GzipFilter);
our @EXPORT_OK = qw(html_oneshot);
use PublicInbox::Hval qw(ascii_html prurl ts2str);

our $CODE_URL = [ qw(
http://7fh6tueqddpjyxjmgtdiueylzoqt6pt7hec3pukyptlmohoowvhde4yd.onion/public-inbox.git
https://public-inbox.org/public-inbox.git) ];

sub base_url ($) {
	my $ctx = shift;
	my $base_url = ($ctx->{ibx} // $ctx->{git})->base_url($ctx->{env});
	chop $base_url; # no trailing slash for clone
	$base_url;
}

sub init {
	my ($ctx, $cb) = @_;
	$ctx->{cb} = $cb;
	$ctx->{base_url} = base_url($ctx);
	$ctx->{-res_hdr} = [ 'Content-Type' => 'text/html; charset=UTF-8' ];
	$ctx->{gz} = PublicInbox::GzipFilter::gz_or_noop($ctx->{-res_hdr},
							$ctx->{env});
	bless $ctx, __PACKAGE__;
}

sub async_eml { # for async_blob_cb
	my ($ctx, $eml) = @_;
	$ctx->write($ctx->{cb}->($ctx, $eml));
}

sub html_repo_top ($) {
	my ($ctx) = @_;
	my $git = $ctx->{git};
	my $desc = ascii_html($git->description);
	my $title = delete($ctx->{-title_html}) // $desc;
	my $upfx = $ctx->{-upfx} // '';
	my $atom = $ctx->{-atom} // (substr($upfx, -1) eq '/' ?
					"${upfx}atom/" : "$upfx/atom/");
	my $top = ascii_html($git->{nick});
	$top = qq(<a\nhref="$upfx">$top</a>) if length($upfx);
	$top .= <<EOM;
  <a href='$upfx#readme'>about</a> / <a
href='$upfx#heads'>heads</a> / <a
href='$upfx#tags'>tags</a>
<b>$desc</b>
EOM
	my @url = PublicInbox::ViewVCS::ibx_url_for($ctx);
	if (@url) {
		$ctx->{-has_srch} = 1;
		my $base_url = base_url($ctx);
		my ($pfx, $sfx) = ($base_url =~ m!\A(https?://[^/]+/)(.*)\z!i);
		my $iupfx = '../' x (($sfx =~ tr!/!/!) + 1);
		$pfx = ascii_html($pfx);
		$pfx = qr/\A\Q$pfx\E/i;
		my $tmp = $top;
		$top = '';
		my ($s, $u, $same_host);
		my $q_val = delete($ctx->{-q_value_html}) // '';
		$q_val = qq(\nvalue="$q_val") if $q_val ne '';
		for (@url) {
			$u = $s = ascii_html($_);
			substr($u, 0, 0, $iupfx) if $u !~ m!://!;
			$s =~ s!$pfx!!;
			$s =~ s!/\z!!;
			$top .= qq{<form\naction="$u"><pre>$tmp} .
				qq{<input\nname=q type=text$q_val />} .
				qq{<input type=submit\n} .
				qq{value="search mail in `$s&#39;"/>} .
				q{</pre></form>};
			$tmp = '';
		}
	} else {
		$top = "<pre>$top</pre>";
	}
	"<html><head><title>$title</title>" .
		qq(<link\nrel=alternate\ntitle="Atom feed"\n).
		qq(href="$atom"\ntype="application/atom+xml"/>) .
		$ctx->{www}->style($upfx) .
		'</head><body>'.$top;
}

sub html_top ($) {
	my ($ctx) = @_;
	my $ibx = $ctx->{ibx} // return html_repo_top($ctx);
	my $desc = ascii_html($ibx->description);
	my $title = delete($ctx->{-title_html}) // $desc;
	my $upfx = $ctx->{-upfx} || '';
	my $atom = $ctx->{-atom} || $upfx.'new.atom';
	my $top = "<b>$desc</b>";
	if (my $t_max = $ctx->{-t_max}) {
		$t_max = ts2str($t_max);
		$top = qq(<a\nhref="$upfx?t=$t_max">$top</a>);
	# we had some kind of query, link to /$INBOX/?t=YYYYMMDDhhmmss
	} elsif ($ctx->{qp}->{t}) {
		$top = qq(<a\nhref="./">$top</a>);
	} elsif (length($upfx)) {
		$top = qq(<a\nhref="$upfx">$top</a>);
	}
	my $code = $ibx->{coderepo} ? qq( / <a\nhref=#code>code</a>) : '';
	# id=mirror must exist for legacy bookmarks
	my $links = qq(<a\nhref="${upfx}_/text/help/">help</a> / ).
			qq(<a\nhref="${upfx}_/text/color/">color</a> / ).
			qq(<a\nid=mirror) .
			qq(\nhref="${upfx}_/text/mirror/">mirror</a>$code / ).
			qq(<a\nhref="$atom">Atom feed</a>);
	if ($ibx->isrch) {
		my $q_val = delete($ctx->{-q_value_html}) // '';
		$q_val = qq(\nvalue="$q_val") if $q_val ne '';
		# XXX gross, for SearchView.pm
		my $extra = delete($ctx->{-extra_form_html}) // '';
		my $action = $upfx eq '' ? './' : $upfx;
		$top = qq{<form\naction="$action"><pre>$top} .
			  qq{\n<input\nname=q\ntype=text$q_val />} .
			  $extra .
			  qq{<input\ntype=submit\nvalue=search />} .
			  ' ' . $links .
			  q{</pre></form>}
	} else {
		$top = '<pre>' . $top . "\n" . $links . '</pre>';
	}
	"<html><head><title>$title</title>" .
		qq(<link\nrel=alternate\ntitle="Atom feed"\n).
		qq(href="$atom"\ntype="application/atom+xml"/>) .
	        $ctx->{www}->style($upfx) .
		'</head><body>'. $top . (delete($ctx->{-html_tip}) // '');
}

sub inboxes { () } # TODO

sub coderepos ($) {
	my ($ctx) = @_;
	$ctx->{ibx} // return inboxes($ctx);
	my $cr = $ctx->{ibx}->{coderepo} // return ();
	my $upfx = ($ctx->{-upfx} // ''). '../';
	my $pfx = $ctx->{base_url} //= $ctx->base_url;
	my $up = $upfx =~ tr!/!/!;
	$pfx =~ s!/[^/]+\z!/! for (1..$up);
	$pfx .= '/' if substr($pfx, -1, 1) ne '/';
	my $buf = '<a id=code>' .
		'Code repositories for project(s) associated with this '.
		$ctx->{ibx}->thing_type . "\n";
	for my $git (@{$ctx->{www}->{pi_cfg}->repo_objs($ctx->{ibx})}) {
		for ($git->pub_urls($ctx->{env})) {
			my $u = m!\A(?:[a-z\+]+:)?//!i ? $_ : $pfx.$_;
			$u = ascii_html(prurl($ctx->{env}, $u));
			$buf .= qq(\n\t<a\nhref="$u">$u</a>);
		}
	}
	($buf);
}

sub _html_end {
	my ($ctx) = @_;
	my $upfx = $ctx->{-upfx} || '';
	my $m = "${upfx}_/text/mirror/";
	my $x = '';
	if ($ctx->{ibx} && $ctx->{ibx}->can('cloneurl')) {
		$x = <<EOF;
This is a public inbox, see <a
href="$m">mirroring instructions</a>
for how to clone and mirror all data and code used for this inbox
EOF
		my $has_nntp = @{$ctx->{ibx}->nntp_url($ctx)};
		my $has_imap = @{$ctx->{ibx}->imap_url($ctx)};
		if ($has_nntp || $has_imap) {
			substr($x, -1, 1) = ";\n"; # s/\n/;\n
			if ($has_nntp && $has_imap) {
				$x .= <<EOM;
as well as URLs for read-only IMAP folder(s) and NNTP newsgroup(s).
EOM
			} elsif ($has_nntp) {
				$x .= <<EOM;
as well as URLs for NNTP newsgroup(s).
EOM
			} else {
				$x .= <<EOM;
as well as URLs for IMAP folder(s).
EOM
			}
		}
	} elsif ($ctx->{ibx}) { # extindex
		$x = <<EOF;
This is an external index of several public inboxes,
see <a href="$m">mirroring instructions</a> on how to clone and mirror
all data and code used by this external index.
EOF
	} elsif ($ctx->{git}) { # coderepo
		$x = join('', map { "git clone $_\n" }
			@{$ctx->{git}->cloneurl($ctx->{env})});
	}
	chomp $x;
	'<hr><pre>'.join("\n\n", coderepos($ctx), $x).'</pre></body></html>'
}

# callback for HTTP.pm (and any other PSGI servers)
sub getline {
	my ($ctx) = @_;
	my $cb = $ctx->{cb} or return;
	while (defined(my $x = $cb->($ctx))) { # x = smsg or scalar non-ref
		if (ref($x)) { # smsg
			my $eml = $ctx->{ibx}->smsg_eml($x) or next;
			$ctx->{smsg} = $x;
			return $ctx->translate($cb->($ctx, $eml));
		} else { # scalar
			return $ctx->translate($x);
		}
	}
	delete $ctx->{cb};
	$ctx->zflush(_html_end($ctx));
}

sub html_done ($;@) {
	my $ctx = $_[0];
	my $bdy = $ctx->zflush(@_[1..$#_], _html_end($ctx));
	my $res_hdr = delete $ctx->{-res_hdr};
	push @$res_hdr, 'Content-Length', length($bdy);
	[ 200, $res_hdr, [ $bdy ] ]
}

sub html_oneshot ($$;@) {
	my ($ctx, $code) = @_[0, 1];
	my $res_hdr = [ 'Content-Type' => 'text/html; charset=UTF-8',
		'Content-Length' => undef ];
	bless $ctx, __PACKAGE__;
	$ctx->{gz} = PublicInbox::GzipFilter::gz_or_noop($res_hdr, $ctx->{env});
	my @top;
	$ctx->{base_url} // do {
		@top = html_top($ctx);
		$ctx->{base_url} = base_url($ctx);
	};
	my $bdy = $ctx->zflush(@top, @_[2..$#_], _html_end($ctx));
	$res_hdr->[3] = length($bdy);
	[ $code, $res_hdr, [ $bdy ] ]
}

sub async_next ($) {
	my ($http) = @_; # PublicInbox::HTTP
	my $ctx = $http->{forward} or return;
	eval {
		if (my $smsg = $ctx->{smsg} = $ctx->{cb}->($ctx)) {
			$ctx->smsg_blob($smsg);
		} else {
			$ctx->write(_html_end($ctx));
			$ctx->close; # GzipFilter->close
		}
	};
	warn "E: $@" if $@;
}

sub aresponse {
	my ($ctx, $cb) = @_;
	init($ctx, $cb);
	$ctx->psgi_response(200, delete $ctx->{-res_hdr});
}

sub html_init {
	my ($ctx) = @_;
	$ctx->{base_url} = base_url($ctx);
	my $h = $ctx->{-res_hdr} = ['Content-Type', 'text/html; charset=UTF-8'];
	$ctx->{gz} = PublicInbox::GzipFilter::gz_or_noop($h, $ctx->{env});
	bless $ctx, __PACKAGE__;
	print { $ctx->zfh } html_top($ctx);
}

sub DESTROY {
	my ($ctx) = @_;
	$ctx->{git}->cleanup if $ctx->{git} && $ctx->{git}->{-tmp};
}

1;
