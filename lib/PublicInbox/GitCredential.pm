# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# git-credential wrapper with built-in .netrc fallback
package PublicInbox::GitCredential;
use v5.12;
use PublicInbox::Spawn qw(popen_rd);
use autodie qw(close pipe);

sub run ($$;$) {
	my ($self, $op, $lei) = @_;
	my ($in_r, $in_w, $out_r);
	my $cmd = [ qw(git credential), $op ];
	pipe($in_r, $in_w);
	if ($lei) { # we'll die if disconnected:
		pipe($out_r, my $out_w);
		$lei->send_exec_cmd([ $in_r, $out_w ], $cmd, {});
	} else {
		$out_r = popen_rd($cmd, undef, { 0 => $in_r });
	}
	close $in_r;

	my $out = '';
	for my $k (qw(url protocol host username password)) {
		my $v = $self->{$k} // next;
		die "`$k' contains `\\n' or `\\0'\n" if $v =~ /[\n\0]/;
		$out .= "$k=$v\n";
	}
	say $in_w $out;
	close $in_w;
	return $out_r if $op eq 'fill';
	<$out_r> and die "unexpected output from `git credential $op'\n";
	$out_r->close or die "`git credential $op' failed: \$!=$! \$?=$?\n";
}

sub check_netrc {
	my ($self, $lei) = @_;

	# n.b. lei doesn't load ~/.netrc by default, public-inbox-watch does,
	# which may've been a mistake, but we have to live with it.
	return if ($lei && !$lei->{opt}->{netrc});

	# part of the standard library, but distributions may split it out
	eval { require Net::Netrc };
	if ($@) {
		warn "W: Net::Netrc missing: $@\n";
		return;
	}
	if (my $x = Net::Netrc->lookup($self->{host}, $self->{username})) {
		$self->{username} //= $x->login;
		$self->{password} = $x->password;
	}
}

sub fill {
	my ($self, $lei) = @_;
	my $out_r = run($self, 'fill', $lei);
	while (<$out_r>) {
		chomp;
		return if $_ eq '';
		/\A([^=]+)=(.*)\z/ or die "bad line: $_\n";
		$self->{$1} = $2;
	}
	$out_r->close or die "git credential fill failed: \$!=$! \$?=$?\n";
	$self->{filled} = 1;
}

1;
