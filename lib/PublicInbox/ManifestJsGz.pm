# Copyright (C) 2020-2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# generates manifest.js.gz for grokmirror(1)
package PublicInbox::ManifestJsGz;
use strict;
use v5.10.1;
use parent qw(PublicInbox::WwwListing);
use PublicInbox::Config;
use IO::Compress::Gzip qw(gzip);
use HTTP::Date qw(time2str);

my $json = PublicInbox::Config::json();

sub url_filter {
	my ($ctx) = @_;
	# grokmirror uses relative paths, so it's domain-dependent
	# SUPER calls PublicInbox::WwwListing::url_filter
	$ctx->SUPER::url_filter('publicInbox.grokManifest', 'match=domain');
}

sub inject_entry ($$$;$) {
	my ($ctx, $url_path, $ent, $git_dir) = @_;
	$ctx->{-abs2urlpath}->{$git_dir // delete $ent->{git_dir}} = $url_path;
	my $modified = $ent->{modified};
	$ctx->{-mtime} = $modified if $modified > ($ctx->{-mtime} // 0);
	$ctx->{manifest}->{$url_path} = $ent;
}

sub manifest_add ($$;$$) { # slow path w/o extindex "all" (or per-inbox)
	my ($ctx, $ibx, $epoch, $default_desc) = @_;
	my $url_path = "/$ibx->{name}";
	my $git;
	if (defined $epoch) {
		$url_path .= "/git/$epoch.git";
		$git = $ibx->git_epoch($epoch) or return;
	} else {
		$git = $ibx->git;
	}
	my $ent = $git->manifest_entry($epoch, $default_desc) or return;
	inject_entry($ctx, $url_path, $ent, $git->{git_dir});
}

sub slow_manifest_add ($$) {
	my ($ctx, $ibx) = @_;
	eval {
		if (defined(my $max = $ibx->max_git_epoch)) {
			my $desc = $ibx->description;
			for my $epoch (0..$max) {
				manifest_add($ctx, $ibx, $epoch, $desc);
			}
		} else {
			manifest_add($ctx, $ibx);
		}
	};
	warn "E: $@" if $@;
}

sub eidx_manifest_add ($$$) {
	my ($ctx, $ALL, $ibx) = @_;
	if (my $data = $ALL->misc->inbox_data($ibx)) {
		$data = $json->decode($data);
		delete $data->{''}; # private
		while (my ($url_path, $ent) = each %$data) {
			inject_entry($ctx, $url_path, $ent);
		}
	} else {
		warn "E: `${\$ibx->eidx_key}' not indexed by $ALL->{topdir}\n";
		# do not use slow path for global manifest since
		# it can become catastrophically slow.  per-inbox manifest
		# is not too bad with dozens of epochs, so never fail that:
		slow_manifest_add($ctx, $ibx) if $ibx == $ctx->{ibx};
	}
}

sub response {
	my ($class, $ctx) = @_;
	bless $ctx, $class;
	my ($re, undef) = $ctx->url_filter;
	$re // return psgi_triple($ctx);
	my $iter = PublicInbox::ConfigIter->new($ctx->{www}->{pi_cfg},
					$ctx->can('list_match_i'), $re, $ctx);
	sub {
		$ctx->{-wcb} = $_[0]; # HTTP server callback
		$ctx->{env}->{'pi-httpd.async'} ?
				$iter->event_step : $iter->each_section;
	}
}

sub ibx_entry {
	my ($ctx, $ibx) = @_;
	my $ALL = $ctx->{www}->{pi_cfg}->ALL;
	$ALL ? eidx_manifest_add($ctx, $ALL, $ibx) :
		slow_manifest_add($ctx, $ibx);
}

sub hide_key { 'manifest' } # for WwwListing->list_match_i

sub psgi_triple {
	my ($ctx) = @_;
	my $abs2urlpath = delete($ctx->{-abs2urlpath}) // {};
	my $manifest = delete($ctx->{manifest}) // {};
	while (my ($url_path, $repo) = each %$manifest) {
		defined(my $abs = $repo->{reference}) or next;
		$repo->{reference} = $abs2urlpath->{$abs};
	}
	$manifest = $json->encode($manifest);
	gzip(\$manifest => \(my $out));
	[ 200, [ qw(Content-Type application/gzip),
		 'Last-Modified', time2str($ctx->{-mtime}),
		 'Content-Length', length($out) ], [ $out ] ]
}

sub per_inbox {
	my ($ctx) = @_;
	ibx_entry($ctx, $ctx->{ibx});
	psgi_triple($ctx);
}

1;
