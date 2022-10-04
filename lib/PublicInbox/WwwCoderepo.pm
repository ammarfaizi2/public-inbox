# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Standalone code repository viewer for users w/o cgit
package PublicInbox::WwwCoderepo;
use v5.12;
use File::Temp 0.19 (); # newdir
use PublicInbox::ViewVCS;
use PublicInbox::WwwStatic qw(r);
use PublicInbox::GitHTTPBackend;
use PublicInbox::Git;
use PublicInbox::GitAsyncCat;
use PublicInbox::WwwStream;
use PublicInbox::Hval qw(ascii_html);

my $EACH_REF = "git for-each-ref --sort=-creatordate --format='%(HEAD)%00".
	join('%00', map { "%($_)" }
	qw(objectname refname:short subject creatordate:short))."'";

# shared with PublicInbox::Cgit
sub prepare_coderepos {
	my ($self) = @_;
	my $pi_cfg = $self->{pi_cfg};

	# TODO: support gitweb and other repository viewers?
	if (defined(my $cgitrc = $pi_cfg->{-cgitrc_unparsed})) {
		$pi_cfg->parse_cgitrc($cgitrc, 0);
	}
	my $code_repos = $pi_cfg->{-code_repos};
	for my $k (grep(/\Acoderepo\.(?:.+)\.dir\z/, keys %$pi_cfg)) {
		$k = substr($k, length('coderepo.'), -length('.dir'));
		$code_repos->{$k} //= $pi_cfg->fill_code_repo($k);
	}
	while (my ($nick, $repo) = each %$code_repos) {
		$self->{"\0$nick"} = $repo;
	}
}

sub new {
	my ($cls, $pi_cfg) = @_;
	my $self = bless { pi_cfg => $pi_cfg }, $cls;
	prepare_coderepos($self);
	$self->{$_} = 10 for qw(summary_branches summary_tags);
	$self->{$_} = 10 for qw(summary_log);
	$self;
}

sub summary_finish {
	my ($ctx) = @_;
	my $wcb = delete($ctx->{env}->{'qspawn.wcb'}) or return; # already done
	my @x = split(/\n\n/sm, delete($ctx->{-each_refs}));
	PublicInbox::WwwStream::html_init($ctx);
	my $zfh = $ctx->zfh;

	# git log
	my @r = split(/\n/s, pop(@x) // '');
	my $last = pop(@r) if scalar(@r) > $ctx->{wcr}->{summary_log};
	print $zfh '<pre><a id=log>$</a> '.
		"git log --pretty=format:'%h %s (%cs)%d'\n";
	for (@r) {
		my $d; # decorations
		s/^ \(([^\)]+)\)// and $d = $1;
		substr($_, 0, 1, '');
		my ($H, $h, $cs, $s) = split(/ /, $_, 4);
		print $zfh "<a\nhref=./$H/s/>$h</a> ", ascii_html($s),
			" (", $cs, ")\n";
		print $zfh "\t(", ascii_html($d), ")\n" if $d;
	}
	print $zfh "# no commits, yet\n" if !@r;
	print $zfh "...\n" if $last;

	# README
	my ($bref, $oid, $ref_path) = @{delete $ctx->{-readme}};
	if ($bref) {
		my $l = PublicInbox::Linkify->new;
		$$bref =~ s/\s*\z//sm;
		print $zfh "\n<a id=readme>\$</a> " .
			"git cat-file blob <a href=./$oid/s/>",
			ascii_html($ref_path), "</a>\n",
			$l->to_html($$bref), '</pre><hr><pre>';
	}

	# refs/heads
	print $zfh "<a id=heads># heads (aka `branches'):</a>\n\$ " .
		"git for-each-ref --sort=-creatordate refs/heads" .
		" \\\n\t--format='%(HEAD) ". # no space for %(align:) hint
		"%(refname:short) %(subject) (%(creatordate:short))'\n";
	@r = split(/^/sm, shift(@x) // '');
	$last = pop(@r) if scalar(@r) > $ctx->{wcr}->{summary_branches};
	for (@r) {
		my ($pfx, $oid, $ref, $s, $cd) = split(/\0/);
		utf8::decode($_) for ($ref, $s);
		chomp $cd;
		my $align = length($ref) < 12 ? ' ' x (12 - length($ref)) : '';
		print $zfh "$pfx <a\nhref=./$oid/s/>", ascii_html($ref),
			"</a>$align ", ascii_html($s), " ($cd)\n";
	}
	print $zfh "# no heads (branches) yet...\n" if !@r;
	print $zfh "...\n" if $last;
	print $zfh "\n<a id=tags># tags:</a>\n\$ " .
		"git for-each-ref --sort=-creatordate refs/tags" .
		" \\\n\t--format='". # no space for %(align:) hint
		"%(refname:short) %(subject) (%(creatordate:short))'\n";
	@r = split(/^/sm, shift(@x) // '');
	$last = pop(@r) if scalar(@r) > $ctx->{wcr}->{summary_tags};
	for (@r) {
		my (undef, $oid, $ref, $s, $cd) = split(/\0/);
		utf8::decode($_) for ($ref, $s);
		chomp $cd;
		my $align = length($ref) < 12 ? ' ' x (12 - length($ref)) : '';
		print $zfh "<a\nhref=./$oid/s/>", ascii_html($ref),
			"</a>$align ", ascii_html($s), " ($cd)\n";
	}
	print $zfh "# no tags yet...\n" if !@r;
	print $zfh "...\n" if $last;
	$wcb->($ctx->html_done('</pre>'));
}

sub capture_refs ($$) { # psgi_qx callback to capture git-for-each-ref + git-log
	my ($bref, $ctx) = @_;
	my $qsp_err = delete $ctx->{-qsp_err};
	$ctx->{-each_refs} = $$bref;
	summary_finish($ctx) if $ctx->{-readme};
}

sub set_readme { # git->cat_async callback
	my ($bref, $oid, $type, $size, $ctx) = @_;
	my $ref_path = shift @{$ctx->{-nr_readme_tries}}; # e.g. HEAD:README
	if ($type eq 'blob' && !$ctx->{-readme}) {
		$ctx->{-readme} = [ $bref, $oid, $ref_path ];
	} elsif (scalar @{$ctx->{-nr_readme_tries}} == 0) {
		$ctx->{-readme} //= []; # nothing left to try
	} # or try another README...
	summary_finish($ctx) if $ctx->{-each_refs} && $ctx->{-readme};
}

sub summary {
	my ($self, $ctx) = @_;
	$ctx->{wcr} = $self;
	my $nb = $self->{summary_branches} + 1;
	my $nt = $self->{summary_tags} + 1;
	my $nl = $self->{summary_log} + 1;
	my $qsp = PublicInbox::Qspawn->new([qw(/bin/sh -c),
		"$EACH_REF --count=$nb refs/heads; echo && " .
		"$EACH_REF --count=$nt refs/tags; echo && " .
		"git log -$nl --pretty=format:'%d %H %h %cs %s' --" ],
		{ GIT_DIR => $ctx->{git}->{git_dir} });
	$qsp->{qsp_err} = \($ctx->{-qsp_err} = '');
	my @try = qw(HEAD:README HEAD:README.md); # TODO: configurable
	$ctx->{-nr_readme_tries} = [ @try ];
	$ctx->{git}->cat_async($_, \&set_readme, $ctx) for @try;
	if ($ctx->{env}->{'pi-httpd.async'}) {
		PublicInbox::GitAsyncCat::watch_cat($ctx->{git});
	} else { # synchronous
		$ctx->{git}->cat_async_wait;
	}
	sub { # $_[0] => PublicInbox::HTTP::{Identity,Chunked}
		$ctx->{env}->{'qspawn.wcb'} = $_[0];
		$qsp->psgi_qx($ctx->{env}, undef, \&capture_refs, $ctx);
	}
}

sub srv { # endpoint called by PublicInbox::WWW
	my ($self, $ctx) = @_;
	my $path_info = $ctx->{env}->{PATH_INFO};
	my $git;
	# handle clone requests
	if ($path_info =~ m!\A/(.+?)/($PublicInbox::GitHTTPBackend::ANY)\z!x) {
		$git = $self->{"\0$1"} and return
			PublicInbox::GitHTTPBackend::serve($ctx->{env},$git,$2);
	}
	$path_info =~ m!\A/(.+?)/\z! and
		($ctx->{git} = $self->{"\0$1"}) and return summary($self, $ctx);
	$path_info =~ m!\A/(.+?)/([a-f0-9]+)/s/\z! and
			($ctx->{git} = $self->{"\0$1"}) and
		return PublicInbox::ViewVCS::show($ctx, $2);

	if ($path_info =~ m!\A/(.+?)\z! and ($git = $self->{"\0$1"})) {
		my $qs = $ctx->{env}->{QUERY_STRING};
		my $url = $git->base_url($ctx->{env});
		$url .= "?$qs" if $qs ne '';
		[ 301, [ Location => $url, 'Content-Type' => 'text/plain' ],
			[ "Redirecting to $url\n" ] ];
	} else {
		r(404);
	}
}

1;
