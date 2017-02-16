# Copyright (C) 2017 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Represents a code repository, analoguos to the PublicInbox::Inbox
# class for represpenting an inbox git repository.
package PublicInbox::Repo;
use strict;
use warnings;
use PublicInbox::Config;

sub new {
	my ($class, $opts) = @_;
	bless $opts, $class;
}

sub description {
	my ($self) = @_;
	my $desc = $self->{description};
	return $desc if defined $desc;
	return unless $self->{vcs} eq 'git'; # TODO

	$desc = PublicInbox::Config::try_cat("$self->{path}/description");
	local $/ = "\n";
	chomp $desc;
	$desc =~ s/\s+/ /smg;
	$desc = '($GIT_DIR/description missing)' if $desc eq '';
	$self->{description} = $desc;
}

sub desc_html {
	my ($self) = @_;
	$self->{desc_html} ||=
		PublicInbox::Hval->utf8($self->description)->as_html;
}

sub cloneurl {
	my ($self) = @_;
	my $url = $self->{cloneurl};
	return $url if $url;
	if ($self->{vcs} eq 'git') {
		$url = PublicInbox::Config::try_cat("$self->{path}/cloneurl");
		$url = [ split(/\s+/s, $url) ];
		local $/ = "\n";
		chomp @$url;
	}
	$self->{cloneurl} = $url;
}

sub tip {
	my ($self) = @_;
	$self->{-tip} ||= do {
		if ($self->{vcs} eq 'git') {
			my $t = $self->{git}->qx(qw(symbolic-ref --short HEAD));
			chomp $t;
			$t;
		}
	};
}

1;
