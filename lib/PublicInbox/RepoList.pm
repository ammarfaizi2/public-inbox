# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::RepoList;
use v5.12;
use parent qw(PublicInbox::WwwStream);
use PublicInbox::Hval qw(ascii_html prurl fmt_ts);
require PublicInbox::CodeSearch;

sub html_top_fallback { # WwwStream->html_repo_top
	my ($ctx) = @_;
	my $title = delete($ctx->{-title_html}) //
		ascii_html("$ctx->{env}->{PATH_INFO}*");
	my $upfx = $ctx->{-upfx} // '';
	"<html><head><title>$title</title>" .
		$ctx->{www}->style($upfx) . '</head><body>';
}

sub html ($$$) {
	my ($wcr, $ctx, $re) = @_;
	my $cr = $wcr->{pi_cfg}->{-coderepos};
	my @nicks = grep(m!$re!, keys %$cr) or return; # 404
	__PACKAGE__->html_init($ctx);
	my $zfh = $ctx->zfh;
	print $zfh "<pre>matching coderepos\n";
	my @recs = PublicInbox::CodeSearch::repos_sorted($wcr->{pi_cfg},
							@$cr{@nicks});
	my $env = $ctx->{env};
	for (@recs) {
		my ($t, $git) = @$_;
		my $nick = ascii_html("$git->{nick}");
		for my $u ($git->pub_urls($env)) {
			$u = prurl($env, $u);
			print $zfh "\n".fmt_ts($t).qq{ <a\nhref="$u">$nick</a>}
		}
	}
	$ctx->html_done('</pre>');
}

1;
