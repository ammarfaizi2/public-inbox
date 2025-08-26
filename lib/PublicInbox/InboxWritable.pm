# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Extends read-only Inbox for writing
package PublicInbox::InboxWritable;
use strict;
use v5.10.1;
use parent qw(PublicInbox::Inbox PublicInbox::Umask Exporter);
use autodie qw(open);
use PublicInbox::Import;
use PublicInbox::IO qw(read_all);
use PublicInbox::Filter::Base qw(REJECT);
use Errno qw(ENOENT);
our @EXPORT_OK = qw(eml_from_path);
use Fcntl qw(O_RDONLY O_NONBLOCK);

sub new {
	my ($class, $ibx, $creat_opt) = @_;
	my $self = bless $ibx, $class; # idempotent
	$self->{-creat_opt} = $creat_opt if $creat_opt; # for { nproc => $N }
	$self;
}

sub assert_usable_dir {
	my ($self) = @_;
	my $dir = $self->{inboxdir};
	return $dir if defined($dir) && $dir ne '';
	die "no inboxdir defined for $self->{name}\n";
}

sub _init_v1 {
	my ($self) = @_;
	my $opt = $self->{-creat_opt} // {};
	my $skip_artnum = $opt->{'skip-artnum'};
	if (defined($self->{indexlevel}) || defined($skip_artnum) ||
			$opt->{wal}) {
		require PublicInbox::SearchIdx;
		require PublicInbox::Msgmap;
		my $sidx = PublicInbox::SearchIdx->new($self, $opt);
		$sidx->{oidx}->{journal_mode} = 'wal' if $opt->{wal};
		$sidx->begin_txn_lazy;
		my $mm = PublicInbox::Msgmap->new_file($self, $opt);
		if (defined $skip_artnum) {
			$mm->{dbh}->begin_work;
			$mm->skip_artnum($skip_artnum);
			$mm->{dbh}->commit;
		}
		undef $mm; # ->created_at set
		$sidx->commit_txn_lazy;
	} else {
		open my $fh, '>>', "$self->{inboxdir}/ssoma.lock";
	}
}

sub init_inbox {
	my ($self, $shards) = @_;
	if ($self->version == 1) {
		my $dir = assert_usable_dir($self);
		PublicInbox::Import::init_bare($dir);
		$self->with_umask(\&_init_v1, $self);
	} else {
		importer($self)->init_inbox($shards);
	}
}

sub importer {
	my ($self, $parallel) = @_;
	my $v = $self->version;
	if ($v == 2) {
		eval { require PublicInbox::V2Writable };
		die "v2 not supported: $@\n" if $@;
		my $opt = $self->{-creat_opt};
		my $v2w = PublicInbox::V2Writable->new($self, $opt);
		$v2w->{parallel} = $parallel if defined $parallel;
		$v2w;
	} elsif ($v == 1) {
		init_inbox($self) if $self->{-creat_opt};
		PublicInbox::Import->new(undef, undef, undef, $self);
	} else {
		$! = 78; # EX_CONFIG 5.3.5 local configuration error
		die "unsupported inbox version: $v\n";
	}
}

sub filter {
	my ($self, $im) = @_;
	my $f = $self->{filter};
	if ($f && $f =~ /::/) {
		# v2 keeps msgmap open, which causes conflicts for filters
		# such as PublicInbox::Filter::RubyLang which overload msgmap
		# for a predictable serial number.
		if ($im && $self->version >= 2 && $self->{altid}) {
			$im->done;
		}

		my @args = (ibx => $self);
		# basic line splitting, only
		# Perhaps we can have proper quote splitting one day...
		($f, @args) = split(/\s+/, $f) if $f =~ /\s+/;

		eval "require $f";
		if ($@) {
			warn $@;
		} else {
			# e.g: PublicInbox::Filter::Vger->new(@args)
			return $f->new(@args);
		}
	}
	undef;
}

sub eml_from_path ($) {
	my ($path) = @_;
	if (sysopen(my $fh, $path, O_RDONLY|O_NONBLOCK)) {
		return unless -f $fh && -s _; # no FIFOs or directories
		PublicInbox::Eml->new(\(my $str = read_all($fh, -s _)));
	} else { # ENOENT is common with Maildir
		warn "failed to open $path: $!\n" if $! != ENOENT;
		undef;
	}
}

sub _each_maildir_eml {
	my ($fn, $kw, $eml, $im, $self) = @_;
	return if grep(/\Adraft\z/, @$kw);
	if ($self && (my $filter = $self->filter($im))) {
		my $ret = $filter->scrub($eml) or return;
		return if $ret == REJECT();
	}
	$im->add($eml);
}

# XXX does anybody use this?
sub import_maildir {
	my ($self, $dir) = @_;
	foreach my $sub (qw(cur new tmp)) {
		-d "$dir/$sub" or die "$dir is not a Maildir (missing $sub)\n";
	}
	my $im = $self->importer(1);
	my @self = $self->filter($im) ? ($self) : ();
	require PublicInbox::MdirReader;
	PublicInbox::MdirReader->new->maildir_each_eml($dir,
					\&_each_maildir_eml, $im, @self);
	$im->done;
}

sub _mbox_eml_cb { # MboxReader->mbox* callback
	my ($eml, $im, $filter) = @_;
	if ($filter) {
		my $ret = $filter->scrub($eml) or return;
		return if $ret == REJECT();
	}
	$im->add($eml);
}

sub import_mbox {
	my ($self, $fh, $variant) = @_;
	require PublicInbox::MboxReader;
	my $cb = PublicInbox::MboxReader->reads($variant) or
		die "$variant not supported\n";
	my $im = $self->importer(1);
	$cb->(undef, $fh, \&_mbox_eml_cb, $im, $self->filter);
	$im->done;
}

sub cleanup ($) {
	delete @{$_[0]}{qw(over mm git search)};
}

# for unconfigured inboxes
sub detect_indexlevel ($) {
	my ($ibx) = @_;

	my $over = $ibx->over;
	my $srch = $ibx->search;
	delete @$ibx{qw(over search)}; # don't leave open FDs lying around

	# brand new or never before indexed inboxes default to full
	return 'full' unless $over;
	my $l = 'basic';
	return $l unless $srch;
	if (my $xdb = $srch->xdb) {
		$l = 'full';
		my $m = $xdb->get_metadata('indexlevel');
		if ($m eq 'medium') {
			$l = $m;
		} elsif ($m ne '') {
			warn <<"";
$ibx->{inboxdir} has unexpected indexlevel in Xapian: $m

		}
		$ibx->{-skip_docdata} = 1 if $xdb->get_metadata('skip_docdata');
	}
	$l;
}

1;
