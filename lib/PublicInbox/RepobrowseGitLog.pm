# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# show the log view
package PublicInbox::RepobrowseGitLog;
use strict;
use warnings;
use PublicInbox::Hval qw(utf8_html);
use base qw(PublicInbox::RepobrowseBase);
use PublicInbox::RepobrowseGit qw(git_dec_links git_commit_title);
use PublicInbox::Qspawn;
# cannot rely on --date=format-local:... yet, it is too new (September 2015)
my $LOG_FMT = '--pretty=tformat:'.
		join('%x00', qw(%h %p %s D%D %ai a%an b%b), '', '');

sub parent_links {
	if (@_ == 1) { # typical, single-parent commit
		qq( / parent <a\nhref="#p$_[0]">$_[0]</a>);
	} elsif (@_ > 0) { # merge commit
		' / parents ' .
			join(' ', map { qq(<a\nhref="#p$_">$_</a>) } @_);
	} else {
		''; # root commit
	}
}

sub git_log_sed_end ($$) {
	my $req = $_[0];
	my $dst = delete $req->{lhtml} || '';
	$dst .= utf8_html($_[1]); # existing buffer
	$dst .= '</pre><hr /><pre>';
	my $m = '';
	my $np = 0;
	my $seen = $req->{seen};
	my $git = $req->{repo_info}->{git};
	my $rel = $req->{relcmd};
	foreach my $p (@{$req->{parents}}) {
		next if $seen->{$p};
		$seen->{$p} = ++$np;
		my $s = git_commit_title($git, $p);
		$m .= qq(\n<a\nid=p$p\nhref="?h=$p">$p</a>\t);
		$s = defined($s) ? utf8_html($s) : '';
		$m .= qq(<a\nhref="${rel}commit?id=$p">$s</a>);
	}
	if ($np == 0) {
		$dst .= "No commits follow";
	} elsif ($np > 1) {
		$dst .= "Unseen parent commits to follow (multiple choice):\n";
	} else {
		$dst .= "Next parent to follow:\n";
	}
	$dst .= $m;
	$dst .= '</pre></body></html>';
}

sub git_log_sed ($$) {
	my ($self, $req) = @_;
	my $buf = '';
	my $state = 'h';
	my %acache;
	my $rel = $req->{relcmd};
	my $seen = $req->{seen} = {};
	my $parents = $req->{parents} = [];
	my ($plinks, $id, $ai);
	sub {
		my $dst;
		# $_[0] == scalar buffer, undef means EOF from "git log"
		return git_log_sed_end($req, $buf) unless defined $_[0];
		$dst = delete $req->{lhtml} || '';
		my @tmp;
		$buf .= $_[0];
		@tmp = split(/\0/, $buf, -1);
		$buf = @tmp ? pop(@tmp) : '';

		while (@tmp) {
			if ($state eq 'b') {
				my $bb = shift @tmp;
				$state = 'B' if $bb =~ s/\Ab/\n/;
				my @lines = split(/\n/, $bb);
				$bb = utf8_html(pop @lines);
				$dst .= utf8_html($_)."\n" for @lines;
				$dst .= $bb;
			} elsif ($state eq 'B') {
				my $bb = shift @tmp;
				if ($bb eq '') {
					$state = 'BB';
				} else {
					my @lines = split(/\n/, $bb);
					$bb = undef;
					my $last = utf8_html(pop @lines);
					$dst .= utf8_html($_)."\n" for @lines;
					$dst .= $last;
				}
			} elsif ($state eq 'BB') {
				if ($tmp[0] =~ s/\A\n//s) {
					$state = 'h';
				} else {
					@tmp = ();
					warn 'Bad state BB in log parser: ',
						$req->{-debug};
				}
			} elsif ($state eq 'h') {
				if (scalar keys %$seen) {
					$dst .= '</pre><hr /><pre>';
				}
				$id = shift @tmp;
				$seen->{$id} = 1;
				$state = 'p'
			} elsif ($state eq 'p') {
				my @p = split(/ /, shift @tmp);
				push @$parents, @p;
				$plinks = parent_links(@p);
				$state = 's'
			} elsif ($state eq 's') {
				# FIXME: excessively long subjects OOM us
				my $s = shift @tmp;
				$dst .= qq(<a\nid=p$id\n);
				$dst .= qq(href="${rel}commit?id=$id"><b>);
				$dst .= utf8_html($s);
				$dst .= '</b></a>';
				$state = 'D'
			} elsif ($state eq 'D') {
				# FIXME: thousands of decorations may OOM us
				my $D = shift @tmp;
				if ($D =~ /\AD(.+)/) {
					$dst .= ' (';
					$dst .= join(', ',
						git_dec_links($rel, $1));
					$dst .= ')';
				}
				$state = 'ai';
			} elsif ($state eq 'ai') {
				$ai = shift @tmp;
				$state = 'an';
			} elsif ($state eq 'an') {
				my $an = shift @tmp;
				$an =~ s/\Aa// or
					die "missing 'a' from author: $an";
				my $ah = $acache{$an} ||= utf8_html($an);
				$dst .= "\n- $ah @ $ai\n  commit $id$plinks\n";
				$id = $plinks = $ai = '';
				$state = 'b';
			}
		}

		$dst;
	};
}

sub call_git_log {
	my ($self, $req) = @_;
	my $repo_info = $req->{repo_info};
	my $max = $repo_info->{max_commit_count} || 50;
	$max = int($max);
	$max = 50 if $max == 0;
	my $env = $req->{env};
	my $q = $req->{'q'} = PublicInbox::RepobrowseGitQuery->new($env);
	my $h = $q->{h};
	$h eq '' and $h = 'HEAD';
	my $git = $repo_info->{git};
	my $git_dir = $git->{git_dir};

	# n.b. no need to escape $h, this -debug line will never
	# be seen if $h is invalid
	# XXX but we should probably validate refnames before execve...
	$req->{-debug} = "git log --git-dir=$git_dir $h --";
	my $cmd = [ 'git', "--git-dir=$git_dir",
			qw(log --no-notes --no-color --abbrev-commit),
			$git->abbrev, $LOG_FMT, "-$max", $h, '--' ];
	my $rdr = { 2 => $git->err_begin };
	my $title = "log: $repo_info->{repo} (" . utf8_html($h). ')';
	$req->{lhtml} = $self->html_start($req, $title) . "\n\n";
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $rdr);
	$qsp->psgi_return($env, undef, sub {
		my ($r) = @_;
		if (!defined $r) {
			[ 500, [ 'Content-Type', 'text/html' ], [ $git->err ] ];
		} elsif ($r == 0) {
			[ 404, [ 'Content-Type', 'text/html' ], [ $git->err ] ];
		} else {
			$env->{'qspawn.filter'} = git_log_sed($self, $req);
			[ 200, [ 'Content-Type', 'text/html' ] ];
		}
	});
}

1;
