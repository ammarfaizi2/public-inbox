# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)

# common functions used by other RepoGit* modules
package PublicInbox::RepoGit;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(git_unquote git_commit_title git_dec_links);
use PublicInbox::Hval qw(utf8_html);

my %GIT_ESC = (
	a => "\a",
	b => "\b",
	f => "\f",
	n => "\n",
	r => "\r",
	t => "\t",
	v => "\013",
);

sub git_unquote ($) {
	my ($s) = @_;
	return $s unless ($s =~ /\A"(.*)"\z/);
	$s = $1;
	$s =~ s/\\([abfnrtv])/$GIT_ESC{$1}/g;
	$s =~ s/\\([0-7]{1,3})/chr(oct($1))/ge;
	$s;
}

# Remove, hilariously slow
sub git_commit_title ($$) {
	my ($git, $obj) = @_; # PublicInbox::Git, $sha1hex
	my $rv;
	eval {
		my $buf = $git->cat_file($obj);
		($rv) = ($$buf =~ /\r?\n\r?\n([^\r\n]+)\r?\n?/);
	};
	$rv;
}

# example inputs: "HEAD -> master", "tag: v1.0.0",
sub git_dec_links ($$) {
	my ($rel, $D) = @_;
	my @l;
	foreach (split /, /, $D) {
		if (/\A(\S+) -> (\S+)/) { # 'HEAD -> master'
			my ($s, $h) = ($1, $2);
			$s = utf8_html($s);
			$h = PublicInbox::Hval->utf8($h);
			my $r = $h->as_href;
			$h = $h->as_html;
			push @l, qq($s -&gt; <a\nhref="${rel}log?h=$r">$h</a>);
		} elsif (s/\Atag: //) {
			my $h = PublicInbox::Hval->utf8($_);
			my $r = $h->as_href;
			$h = $h->as_html;
			push @l, qq(<a\nhref="${rel}tag?h=$r"><b>$h</b></a>);
		} else {
			my $h = PublicInbox::Hval->utf8($_);
			my $r = $h->as_href;
			$h = $h->as_html;
			push @l, qq(<a\nhref="${rel}log?h=$r">$h</a>);
		}
	}
	@l;
}

1;
