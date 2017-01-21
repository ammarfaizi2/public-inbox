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
use constant STATES => qw(h p D ai an s b);
use constant STATE_BODY => (scalar(STATES) - 1);
my $LOG_FMT = '--pretty=tformat:'.  join('%n', map { "%$_" } STATES).'%x00';

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

sub flush_log_hdr ($$$) {
	my ($req, $dst, $hdr) = @_;
	my $rel = $req->{relcmd};
	my $seen = $req->{seen};
	$$dst .= '<hr /><pre>' if scalar keys %$seen;
	my $id = $hdr->{h};
	$seen->{$id} = 1;
	$$dst .= qq(<a\nid=p$id\n);
	$$dst .= qq(href="${rel}commit?id=$id"><b>);
	$$dst .= utf8_html($hdr->{'s'}); # FIXME may still OOM
	$$dst .= '</b></a>';
	my $D = $hdr->{D}; # FIXME: thousands of decorations may OOM us
	if ($D ne '') {
		$$dst .= ' (' . join(', ', git_dec_links($rel, $D)) . ')';
	}
	my @p = split(/ /, $hdr->{p});
	push @{$req->{parents}}, @p;
	my $plinks = parent_links(@p);
	$$dst .= "\n- ";
	$$dst .= utf8_html($hdr->{an});
	$$dst .= " @ $hdr->{ai}\n  commit $id$plinks\n";
	undef
}

sub git_log_sed_end ($$) {
	my ($req, $dst) = @_;
	$$dst .= '<hr /><pre>';
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
		$$dst .= "No commits follow";
	} elsif ($np > 1) {
		$$dst .= "Unseen parent commits to follow (multiple choice):\n";
	} else {
		$$dst .= "Next parent to follow:\n";
	}
	$$dst .= $m;
	$$dst .= '</pre></body></html>';
}

sub git_log_sed ($$) {
	my ($self, $req) = @_;
	my $buf = '';
	my $state = 0;
	$req->{seen} = {};
	$req->{parents} = [];
	my $hdr = {};
	sub {
		my $dst;
		# $_[0] == scalar buffer, undef means EOF from "git log"
		$dst = delete $req->{lhtml} || '';
		my @tmp;
		if (defined $_[0]) {
			$buf .= $_[0];
			@tmp = split(/\n/, $buf, -1);
			$buf = @tmp ? pop(@tmp) : '';
		} else {
			@tmp = split(/\n/, $buf, -1);
			$buf = undef;
		}

		foreach my $l (@tmp) {
			if ($state != STATE_BODY) {
				$hdr->{((STATES)[$state])} = $l;
				if (++$state == STATE_BODY) {
					flush_log_hdr($req, \$dst, $hdr);
					$hdr = {};
				}
				next;
			}
			if ($l eq "\0") {
				$dst .= qq(</pre>);
				$state = 0;
			} else {
				$dst .= "\n";
				$dst .= utf8_html($l);
			}
		}
		git_log_sed_end($req, \$dst) unless defined $buf;
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
