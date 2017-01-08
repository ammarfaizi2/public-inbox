# Copyright (C) 2016 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
#
# Represents a public-inbox (which may have multiple mailing addresses)
package PublicInbox::Inbox;
use strict;
use warnings;
use PublicInbox::Git;
use PublicInbox::MID qw(mid2path);

my $cleanup_timer;
eval {
	$cleanup_timer = 'disabled';
	require PublicInbox::EvCleanup;
	$cleanup_timer = undef; # OK if we get here
};

my $CLEANUP = {}; # string(inbox) -> inbox
sub cleanup_task () {
	$cleanup_timer = undef;
	delete $_->{git} for values %$CLEANUP;
	$CLEANUP = {};
}

sub _set_uint ($$$) {
	my ($opts, $field, $default) = @_;
	my $val = $opts->{$field};
	if (defined $val) {
		$val = $val->[-1] if ref($val) eq 'ARRAY';
		$val = undef if $val !~ /\A\d+\z/;
	}
	$opts->{$field} = $val || $default;
}

sub _set_limiter ($$$) {
	my ($self, $pi_config, $pfx) = @_;
	my $lkey = "-${pfx}_limiter";
	$self->{$lkey} ||= eval {
		# full key is: publicinbox.$NAME.httpbackendmax
		my $mkey = $pfx.'max';
		my $val = $self->{$mkey} or return;
		my $lim;
		if ($val =~ /\A\d+\z/) {
			require PublicInbox::Qspawn;
			$lim = PublicInbox::Qspawn::Limiter->new($val);
		} elsif ($val =~ /\A[a-z][a-z0-9]*\z/) {
			$lim = $pi_config->limiter($val);
			warn "$mkey limiter=$val not found\n" if !$lim;
		} else {
			warn "$mkey limiter=$val not understood\n";
		}
		$lim;
	}
}

sub new {
	my ($class, $opts) = @_;
	my $v = $opts->{address} ||= 'public-inbox@example.com';
	my $p = $opts->{-primary_address} = ref($v) eq 'ARRAY' ? $v->[0] : $v;
	$opts->{domain} = ($p =~ /\@(\S+)\z/) ? $1 : 'localhost';
	my $pi_config = delete $opts->{-pi_config};
	_set_limiter($opts, $pi_config, 'httpbackend');
	_set_uint($opts, 'feedmax', 25);
	$opts->{nntpserver} ||= $pi_config->{'publicinbox.nntpserver'};
	bless $opts, $class;
}

sub git {
	my ($self) = @_;
	$self->{git} ||= eval {
		my $g = PublicInbox::Git->new($self->{mainrepo});
		$g->{-httpbackend_limiter} = $self->{-httpbackend_limiter};
		$cleanup_timer ||= PublicInbox::EvCleanup::later(*cleanup_task);
		$CLEANUP->{"$self"} = $self;
		$g;
	};
}

sub mm {
	my ($self) = @_;
	$self->{mm} ||= eval { PublicInbox::Msgmap->new($self->{mainrepo}) };
}

sub search {
	my ($self) = @_;
	$self->{search} ||= eval {
		PublicInbox::Search->new($self->{mainrepo}, $self->{altid});
	};
}

sub try_cat {
	my ($path) = @_;
	my $rv = '';
	if (open(my $fh, '<', $path)) {
		local $/;
		$rv = <$fh>;
	}
	$rv;
}

sub description {
	my ($self) = @_;
	my $desc = $self->{description};
	return $desc if defined $desc;
	$desc = try_cat("$self->{mainrepo}/description");
	local $/ = "\n";
	chomp $desc;
	$desc =~ s/\s+/ /smg;
	$desc = '($GIT_DIR/description missing)' if $desc eq '';
	$self->{description} = $desc;
}

sub cloneurl {
	my ($self) = @_;
	my $url = $self->{cloneurl};
	return $url if $url;
	$url = try_cat("$self->{mainrepo}/cloneurl");
	my @url = split(/\s+/s, $url);
	local $/ = "\n";
	chomp @url;
	$self->{cloneurl} = \@url;
}

sub base_url {
	my ($self, $env) = @_;
	if ($env) { # PSGI env
		my $scheme = $env->{'psgi.url_scheme'};
		my $host_port = $env->{HTTP_HOST} ||
			"$env->{SERVER_NAME}:$env->{SERVER_PORT}";
		my $url = "$scheme://$host_port". ($env->{SCRIPT_NAME} || '/');
		# for mount in Plack::Builder
		$url .= '/' if $url !~ m!/\z!;
		$url .= $self->{name} . '/';
	} else {
		# either called from a non-PSGI environment (e.g. NNTP/POP3)
		$self->{-base_url} ||= do {
			my $url = $self->{url} or return undef;
			# expand protocol-relative URLs to HTTPS if we're
			# not inside a web server
			$url = "https:$url" if $url =~ m!\A//!;
			$url .= '/' if $url !~ m!/\z!;
			$url;
		};
	}
}

sub nntp_url {
	my ($self) = @_;
	$self->{-nntp_url} ||= do {
		# no checking for nntp_usable here, we can point entirely
		# to non-local servers or users run by a different user
		my $ns = $self->{nntpserver};
		my $group = $self->{newsgroup};
		my @urls;
		if ($ns && $group) {
			$ns = [ $ns ] if ref($ns) ne 'ARRAY';
			@urls = map {
				my $u = m!\Anntps?://! ? $_ : "nntp://$_";
				$u .= '/' if $u !~ m!/\z!;
				$u.$group;
			} @$ns;
		}

		my $mirrors = $self->{nntpmirror};
		if ($mirrors) {
			my @m;
			foreach (@$mirrors) {
				my $u = m!\Anntps?://! ? $_ : "nntp://$_";
				if ($u =~ m!\Anntps?://[^/]+/?\z!) {
					if ($group) {
						$u .= '/' if $u !~ m!/\z!;
						$u .= $group;
					} else {
						warn
"publicinbox.$self->{name}.nntpmirror=$_ missing newsgroup name\n";
					}
				}
				# else: allow full URLs like:
				# nntp://news.example.com/alt.example
				push @m, $u;
			}
			my %seen = map { $_ => 1 } @urls;
			foreach my $u (@m) {
				next if $seen{$u};
				$seen{$u} = 1;
				push @urls, $u;
			}
		}
		\@urls;
	};
}

sub nntp_usable {
	my ($self) = @_;
	my $ret = $self->mm && $self->search;
	$self->{mm} = $self->{search} = undef;
	$ret;
}

sub msg_by_path ($$) {
	my ($self, $path, $ref) = @_;
	# TODO: allow other refs:
	my $str = git($self)->cat_file('HEAD:'.$path);
	$$str =~ s/\A[\r\n]*From [^\r\n]*\r?\n//s if $str;
	$str;
}

sub msg_by_smsg ($$) {
	my ($self, $smsg) = @_;

	return unless defined $smsg; # ghost

	# backwards compat to fallback to msg_by_mid
	# TODO: remove if we bump SCHEMA_VERSION in Search.pm:
	defined(my $blob = $smsg->{blob}) or
			return msg_by_mid($self, $smsg->mid);

	my $str = git($self)->cat_file($blob);
	$$str =~ s/\A[\r\n]*From [^\r\n]*\r?\n//s if $str;
	$str;
}

sub path_check {
	my ($self, $path) = @_;
	git($self)->check('HEAD:'.$path);
}

sub msg_by_mid ($$) {
	my ($self, $mid) = @_;
	msg_by_path($self, mid2path($mid));
}

1;
