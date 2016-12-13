# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# shows the /patch/ endpoint for git repositories
# usage: /repo.git/patch?id=COMMIT_ID
package PublicInbox::RepobrowseGitPatch;
use strict;
use warnings;
use base qw(PublicInbox::RepobrowseBase);

# try to be educational and show the command-line used in the signature
my @CMD = qw(format-patch -M --stdout);
my $sig = '--signature=git '.join(' ', @CMD);

sub call_git_patch {
	my ($self, $req) = @_;
	my $git = $req->{repo_info}->{git};
	my $q = PublicInbox::RepobrowseGitQuery->new($req->{cgi});
	my $id = $q->{id};
	$id =~ /\A[\w-]+([~\^][~\^\d])*\z/ or $id = 'HEAD';

	# limit scope, don't take extra args to avoid wasting server
	# resources buffering:
	my $range = "$id~1..$id^0";
	my @cmd = (@CMD, $sig." $range", $range, '--');
	if (defined(my $expath = $req->{expath})) {
		push @cmd, $expath;
	}
	my $rpipe = $git->popen(@cmd);
	my $env = $req->{cgi}->env;
	my $err = $env->{'psgi.errors'};
	my ($n, $res, $vin, $fh);
	my $end = sub {
		if ($fh) {
			$fh->close;
			$fh = undef;
		} elsif ($res) {
			$res->($self->r(500));
		}
		if ($rpipe) {
			$rpipe->close; # _may_ be Danga::Socket::close
			$rpipe = undef;
		}
	};
	my $fail = sub {
		if ($!{EAGAIN} || $!{EINTR}) {
			select($vin, undef, undef, undef) if defined $vin;
			# $vin is undef on async, so this is a noop on EAGAIN
			return;
		}
		my $e = $!;
		$end->();
		$err->print("git format-patch ($git->{git_dir}): $e\n");
	};
	my $cb = sub {
		$n = $rpipe->sysread(my $buf, 65536);
		return $fail->() unless defined $n;
		return $end->() if $n == 0;
		if ($res) {
			my $h = ['Content-Type', 'text/plain; charset=UTF-8'];
			$fh = $res->([200, $h]);
			$res = undef;
		}
		$fh->write($buf);
	};

	if (my $async = $env->{'pi-httpd.async'}) {
		$rpipe = $async->($rpipe, $cb);
		sub { ($res) = @_ } # let Danga::Socket handle the rest.
	} else { # synchronous loop for other PSGI servers
		$vin = '';
		vec($vin, fileno($rpipe), 1) = 1;
		sub {
			($res) = @_;
			while ($rpipe) { $cb->() }
		}
	}
}

1;
