# Copyright (C) 2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# pretends to be like LeiDedupe and also PublicInbox::Inbox
package PublicInbox::LeiSavedSearch;
use strict;
use v5.10.1;
use parent qw(PublicInbox::Lock);
use PublicInbox::OverIdx;
use PublicInbox::LeiSearch;
use PublicInbox::Config;
use PublicInbox::Spawn qw(run_die);
use PublicInbox::ContentHash qw(git_sha);
use Digest::SHA qw(sha256_hex);

sub lss_dir_for ($$) {
	my ($lei, $dstref) = @_;
	my @n;
	if ($$dstref =~ m,\Aimaps?://,i) { # already canonicalized
		require PublicInbox::URIimap;
		my $uri = PublicInbox::URIimap->new($$dstref)->canonical;
		$$dstref = $$uri;
		@n = ($uri->mailbox);
	} else { # basename
		@n = ($$dstref =~ m{([\w\-\.]+)/*\z});
		$$dstref = $lei->rel2abs($$dstref);
	}
	push @n, sha256_hex($$dstref);
	$lei->share_path . '/saved-searches/' . join('-', @n);
}

sub new {
	my ($cls, $lei, $dir) = @_;
	my $self = bless { ale => $lei->ale }, $cls;
	if (defined $dir) { # updating existing saved search via "lei up"
		my $f = "$dir/lei.saved-search";
		((-f $f && -r _) || output2lssdir($self, $lei, \$dir, \$f)) or
			return $lei->fail("$f non-existent or unreadable");
		$self->{-cfg} //= PublicInbox::Config::git_config_dump($f);
		$self->{'-f'} = $f;
	} else { # new saved search "lei q --save"
		my $dst = $lei->{ovv}->{dst};
		$dir = lss_dir_for($lei, \$dst);
		require File::Path;
		File::Path::make_path($dir); # raises on error
		$self->{-cfg} = {};
		$self->{'-f'} = "$dir/lei.saved-search";
		my $q = $lei->{mset_opt}->{q_raw} // die 'BUG: {q_raw} missing';
		if (ref $q) {
			cfg_set($self, '--add', 'lei.q', $_) for @$q;
		} else {
			cfg_set($self, 'lei.q', $q);
		}
		$dst = "$lei->{ovv}->{fmt}:$dst" if $dst !~ m!\Aimaps?://!i;
		cfg_set($self, 'lei.q.output', $dst);
		for my $k (qw(only include exclude)) {
			my $ary = $lei->{opt}->{$k} // next;
			for my $x (@$ary) {
				cfg_set($self, '--add', "lei.q.$k", $x);
			}
		}
		for my $k (qw(external local remote import-remote
				import-before threads)) {
			my $val = $lei->{opt}->{$k} // next;
			cfg_set($self, "lei.q.$k", $val);
		}
	}
	bless $self->{-cfg}, 'PublicInbox::Config';
	$self->{lock_path} = "$self->{-f}.flock";
	$self->{-ovf} = "$dir/over.sqlite3";
	$self;
}

sub description { $_[0]->{qstr} } # for WWW

sub cfg_set {
	my ($self, @args) = @_;
	my $lk = $self->lock_for_scope; # git-config doesn't wait
	run_die([qw(git config -f), $self->{'-f'}, @args]);
}

# drop-in for LeiDedupe API
sub is_dup {
	my ($self, $eml, $smsg) = @_;
	my $oidx = $self->{oidx} // die 'BUG: no {oidx}';
	my $blob = $smsg ? $smsg->{blob} : undef;
	return 1 if $blob && $oidx->blob_exists($blob);
	my $lk = $self->lock_for_scope_fast;
	if (my $xoids = PublicInbox::LeiSearch::xoids_for($self, $eml, 1)) {
		for my $docid (values %$xoids) {
			$oidx->add_xref3($docid, -1, $blob, '.');
		}
		$oidx->commit_lazy;
		1;
	} else {
		# n.b. above xoids_for fills out eml->{-lei_fake_mid} if needed
		unless ($smsg) {
			$smsg = bless {}, 'PublicInbox::Smsg';
			$smsg->{bytes} = 0;
			$smsg->populate($eml);
		}
		$oidx->begin_lazy;
		$smsg->{num} = $oidx->adj_counter('eidx_docid', '+');
		$smsg->{blob} //= git_sha(1, $eml)->hexdigest;
		$oidx->add_overview($eml, $smsg);
		$oidx->add_xref3($smsg->{num}, -1, $smsg->{blob}, '.');
		$oidx->commit_lazy;
		undef;
	}
}

sub prepare_dedupe {
	my ($self) = @_;
	$self->{oidx} //= do {
		my $creat = !-f $self->{-ovf};
		my $lk = $self->lock_for_scope; # git-config doesn't wait
		my $oidx = PublicInbox::OverIdx->new($self->{-ovf});
		$oidx->{-no_fsync} = 1;
		$oidx->dbh;
		if ($creat) {
			$oidx->{dbh}->do('PRAGMA journal_mode = WAL');
			$oidx->eidx_prep; # for xref3
		}
		$oidx
	};
}

sub over { $_[0]->{oidx} } # for xoids_for

sub git { $_[0]->{ale}->git }

sub pause_dedupe {
	my ($self) = @_;
	$self->{ale}->git->cleanup;
	my $oidx = delete($self->{oidx}) // return;
	$oidx->commit_lazy;
}

sub mm { undef }

sub altid_map { {} }

sub cloneurl { [] }

# find existing directory containing a `lei.saved-search' file based on
# $dir_ref which is an output
sub output2lssdir {
	my ($self, $lei, $dir_ref, $fn_ref) = @_;
	my $dst = $$dir_ref; # imap://$MAILBOX, /path/to/maildir, /path/to/mbox
	my $dir = lss_dir_for($lei, \$dst);
	my $f = "$dir/lei.saved-search";
	if (-f $f && -r _) {
		$self->{-cfg} = PublicInbox::Config::git_config_dump($f);
		$$dir_ref = $dir;
		$$fn_ref = $f;
		return 1;
	}
	undef;
}

no warnings 'once';
*nntp_url = \&cloneurl;
*base_url = \&PublicInbox::Inbox::base_url;
*smsg_eml = \&PublicInbox::Inbox::smsg_eml;
*smsg_by_mid = \&PublicInbox::Inbox::smsg_by_mid;
*msg_by_mid = \&PublicInbox::Inbox::msg_by_mid;
*modified = \&PublicInbox::Inbox::modified;
*recent = \&PublicInbox::Inbox::recent;
*max_git_epoch = *nntp_usable = *msg_by_path = \&mm; # undef
*isrch = *search = \&mm; # TODO
*DESTROY = \&pause_dedupe;

1;
