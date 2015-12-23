# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)

# common functions used by other RepoBrowseGit* modules
package PublicInbox::RepoBrowseGit;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(git_unquote git_commit_title);

my %GIT_ESC = (
	a => "\a",
	b => "\b",
	f => "\f",
	n => "\n",
	r => "\r",
	t => "\t",
	v => "\013",
);

sub git_unquote {
	my ($s) = @_;
	return $s unless ($s =~ /\A"(.*)"\z/);
	$s = $1;
	$s =~ s/\\([abfnrtv])/$GIT_ESC{$1}/g;
	$s =~ s/\\([0-7]{1,3})/chr(oct($1))/ge;
	$s;
}

sub git_commit_title {
	my ($git, $obj) = @_; # PublicInbox::Git, $sha1hex
	my $rv;
	eval {
		my $buf = $git->cat_file($obj);
		($rv) = ($$buf =~ /\r?\n\r?\n([^\r\n]+)\r?\n?/);
	};
	$rv;
}

1;
