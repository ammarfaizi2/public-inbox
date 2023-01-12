# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Standalone code repository viewer for users w/o cgit.
# This isn't intended to replicate all of cgit, but merely to be a
# "good enough" viewer with search support and some UI hints to encourage
# cloning + command-line usage.
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
use PublicInbox::ViewDiff qw(uri_escape_path);
use PublicInbox::RepoSnapshot;
use PublicInbox::RepoAtom;
use PublicInbox::RepoTree;

my $EACH_REF = "git for-each-ref --sort=-creatordate --format='%(HEAD)%00".
	join('%00', map { "%($_)" }
	qw(objectname refname:short subject creatordate:short))."'";

# shared with PublicInbox::Cgit
sub prepare_coderepos {
	my ($self) = @_;
	my $pi_cfg = $self->{pi_cfg};

	# TODO: support gitweb and other repository viewers?
	$pi_cfg->parse_cgitrc(undef, 0);

	my $code_repos = $pi_cfg->{-code_repos};
	for my $k (grep(/\Acoderepo\.(?:.+)\.dir\z/, keys %$pi_cfg)) {
		$k = substr($k, length('coderepo.'), -length('.dir'));
		$code_repos->{$k} //= $pi_cfg->fill_code_repo($k);
	}

	# associate inboxes and extindices with coderepos for search:
	for my $k (grep(/\Apublicinbox\.(?:.+)\.coderepo\z/, keys %$pi_cfg)) {
		$k = substr($k, length('publicinbox.'), -length('.coderepo'));
		my $ibx = $pi_cfg->lookup_name($k) // next;
		$pi_cfg->repo_objs($ibx);
	}
	for my $k (grep(/\Aextindex\.(?:.+)\.coderepo\z/, keys %$pi_cfg)) {
		$k = substr($k, length('extindex.'), -length('.coderepo'));
		my $eidx = $pi_cfg->lookup_ei($k) // next;
		$pi_cfg->repo_objs($eidx);
	}
}

sub new {
	my ($cls, $pi_cfg) = @_;
	my $self = bless { pi_cfg => $pi_cfg }, $cls;
	prepare_coderepos($self);
	$self->{snapshots} = do {
		my $s = $pi_cfg->{'coderepo.snapshots'} // '';
		$s eq 'all' ? \%PublicInbox::RepoSnapshot::FMT_TYPES :
			+{ map { $_ => 1 } split(/\s+/, $s) };
	};
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
	my $tip_html = '';
	if (defined(my $tip = $ctx->{qp}->{h})) {
		$tip_html .= ' '.ascii_html($tip).' --';
	}
	print $zfh <<EOM;
<pre><a id=log>\$</a> git log --pretty=format:'%h %s (%cs)%d'$tip_html
EOM
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
		my (undef, $path) = split(/:/, $ref_path, 2); # HEAD:README
		print $zfh "\n<a id=readme>\$</a> " .
			qq(git cat-file blob <a href="./$oid/s/?b=) .
			ascii_html(uri_escape_path($path)) . q(">).
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
	my @s = sort keys %{$ctx->{wcr}->{snapshots}};
	my $n;
	if (@s) {
		$n = $ctx->{git}->local_nick // die "BUG: $ctx->{git_dir} nick";
		$n =~ s/\.git\z/-/;
		($n) = ($n =~ m!([^/]+)\z!);
		$n = ascii_html($n);
	}
	for (@r) {
		my (undef, $oid, $ref, $s, $cd) = split(/\0/);
		utf8::decode($_) for ($ref, $s);
		chomp $cd;
		my $align = length($ref) < 12 ? ' ' x (12 - length($ref)) : '';
		print $zfh "<a\nhref=./$oid/s/>", ascii_html($ref),
			"</a>$align ", ascii_html($s), " ($cd)";
		if (@s) {
			my $v = $ref;
			$v =~ s/\A[vV]//;
			print $zfh "\t",  join(' ', map {
				qq{<a href="snapshot/$n$v.$_">$_</a>} } @s);
		}
		print $zfh "\n";
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
	my $tip = $ctx->{qp}->{h}; # same as cgit
	if (defined $tip && $tip eq '') {
		delete $ctx->{qp}->{h};
		undef($tip);
	}
	my $nb = $self->{summary_branches} + 1;
	my $nt = $self->{summary_tags} + 1;
	my $nl = $self->{summary_log} + 1;

	my @cmd = (qw(/bin/sh -c),
		"$EACH_REF --count=$nb refs/heads; echo && " .
		"$EACH_REF --count=$nt refs/tags; echo && " .
		qq(git log -$nl --pretty=format:'%d %H %h %cs %s' "\$@" --));
	push @cmd, '--', $tip if defined($tip);
	my $qsp = PublicInbox::Qspawn->new(\@cmd,
		{ GIT_DIR => $ctx->{git}->{git_dir} });
	$qsp->{qsp_err} = \($ctx->{-qsp_err} = '');
	$tip //= 'HEAD';
	my @try = ("$tip:README", "$tip:README.md"); # TODO: configurable
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
	my $cr = $self->{pi_cfg}->{-code_repos};
	if ($path_info =~ m!\A/(.+?)/($PublicInbox::GitHTTPBackend::ANY)\z!x) {
		$git = $cr->{$1} and return
			PublicInbox::GitHTTPBackend::serve($ctx->{env},$git,$2);
	}
	$path_info =~ m!\A/(.+?)/\z! and
		($ctx->{git} = $cr->{$1}) and return summary($self, $ctx);
	$path_info =~ m!\A/(.+?)/([a-f0-9]+)/s/\z! and
			($ctx->{git} = $cr->{$1}) and
		return PublicInbox::ViewVCS::show($ctx, $2);

	if ($path_info =~ m!\A/(.+?)/tree/(.*)\z! and
			($ctx->{git} = $cr->{$1})) {
		return PublicInbox::RepoTree::srv_tree($ctx, $2) // r(404);
	}

	# snapshots:
	if ($path_info =~ m!\A/(.+?)/snapshot/([^/]+)\z! and
			($ctx->{git} = $cr->{$1})) {
		$ctx->{wcr} = $self;
		return PublicInbox::RepoSnapshot::srv($ctx, $2) // r(404);
	}

	if ($path_info =~ m!\A/(.+?)/atom/(.*)\z! and
			($ctx->{git} = $cr->{$1})) {
		return PublicInbox::RepoAtom::srv_atom($ctx, $2) // r(404);
	}

	# enforce trailing slash:
	if ($path_info =~ m!\A/(.+?)\z! and ($git = $cr->{$1})) {
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
