# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::MailDiff;
use v5.12;
use File::Temp 0.19 (); # 0.19 for ->newdir
use PublicInbox::ContentHash qw(content_digest);
use PublicInbox::MsgIter qw(msg_part_text);
use PublicInbox::ViewDiff qw(flush_diff);
use PublicInbox::GitAsyncCat;
use PublicInbox::ContentDigestDbg;
use PublicInbox::Qspawn;
use PublicInbox::IO qw(write_file);
use autodie qw(close mkdir);

sub write_part { # Eml->each_part callback
	my ($ary, $self) = @_;
	my ($part, $depth, $idx) = @$ary;
	if ($idx ne '1' || $self->{-raw_hdr}) { # lei mail-diff --raw-header
		write_file '>', "$self->{curdir}/$idx.hdr", ${$part->{hdr}};
	}
	my $ct = $part->content_type || 'text/plain';
	my ($s, $err) = msg_part_text($part, $ct);
	my $sfx = defined($s) ? 'txt' : 'bin';
	$s //= $part->body;
	$s =~ s/\r\n/\n/gs; # TODO: consider \r+\n to match View
	$s =~ s/\s*\z//s;
	write_file '>:utf8', "$self->{curdir}/$idx.$sfx", $s;
}

# public
sub dump_eml ($$$) {
	my ($self, $dir, $eml) = @_;
	local $self->{curdir} = $dir;
	mkdir $dir;
	$eml->each_part(\&write_part, $self);
	my $fh = write_file '>', "$dir/content_digest";
	my $dig = PublicInbox::ContentDigestDbg->new($fh);
	content_digest($eml, $dig);
	say $fh "\n", $dig->hexdigest;
	close $fh;
}

# public
sub prep_a ($$) {
	my ($self, $eml) = @_;
	$self->{tmp} = File::Temp->newdir('mail-diff-XXXX', TMPDIR => 1);
	dump_eml($self, "$self->{tmp}/a", $eml);
}

# WWW-specific stuff below (TODO: split out for non-lei)

sub next_smsg ($) {
	my ($self) = @_;
	my $ctx = $self->{ctx};
	my $over = $ctx->{ibx}->over;
	$self->{smsg} = $over ? $over->next_by_mid(@{$self->{next_arg}})
			: $ctx->gone('over');
	if (!$self->{smsg}) {
		$ctx->write($ctx->_html_end);
		return $ctx->close;
	}
	PublicInbox::DS::requeue($self) if $ctx->{env}->{'pi-httpd.async'};
}

sub emit_msg_diff {
	my ($bref, $self) = @_; # bref is `git diff' output
	require PublicInbox::Hval;

	# will be escaped to `&#8226;' in HTML
	$self->{ctx}->{ibx}->{obfuscate} and
		PublicInbox::Hval::obfuscate_addrs($self->{ctx}->{ibx},
						$$bref, "\x{2022}");
	print { $self->{ctx}->{zfh} } '</pre><hr><pre>' if $self->{nr} > 1;
	flush_diff($self->{ctx}, $bref);
	next_smsg($self);
}

sub do_diff {
	my ($self, $eml) = @_;
	my $n = 'N'.(++$self->{nr});
	my $dir = "$self->{tmp}/$n";
	$self->dump_eml($dir, $eml);
	my $cmd = [ qw(git diff --no-index --no-color -- a), $n ];
	my $opt = { -C => "$self->{tmp}", quiet => 1, 1 => [':utf8', \my $o] };
	my $qsp = PublicInbox::Qspawn->new($cmd, undef, $opt);
	$qsp->psgi_qx($self->{ctx}->{env}, undef, \&emit_msg_diff, $self);
}

sub diff_msg_i {
	my ($self, $eml) = @_;
	if ($eml) {
		if ($self->{tmp}) { # 2nd..last message
			do_diff($self, $eml);
		} else { # first message:
			prep_a($self, $eml);
			next_smsg($self);
		}
	} else {
		warn "W: $self->{smsg}->{blob} missing\n";
		next_smsg($self);
	}
}

sub diff_msg_i_async {
	my ($bref, $oid, $type, $size, $self) = @_;
	diff_msg_i($self, $bref ? PublicInbox::Eml->new($bref) : undef);
}

sub event_step {
	my ($self) = @_;
	eval {
		my $ctx = $self->{ctx};
		if ($ctx->{env}->{'pi-httpd.async'}) {
			ibx_async_cat($ctx->{ibx}, $self->{smsg}->{blob},
					\&diff_msg_i_async, $self);
		} else {
			diff_msg_i($self, $ctx->{ibx}->smsg_eml($self->{smsg}));
		}
	};
	if ($@) {
		warn "E: $@";
		delete $self->{smsg};
		$self->{ctx}->close;
	}
}

sub begin_mail_diff {
	my ($self) = @_;
	if ($self->{ctx}->{env}->{'pi-httpd.async'}) {
		PublicInbox::DS::requeue($self);
	} else {
		event_step($self) while $self->{smsg};
	}
}

1;
