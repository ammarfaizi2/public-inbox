# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::MailDiff;
use v5.12;
use File::Temp 0.19 (); # 0.19 for ->newdir
use PublicInbox::ContentHash qw(content_digest);
use PublicInbox::ContentDigestDbg;
use Data::Dumper ();
use PublicInbox::MsgIter qw(msg_part_text);

sub write_part { # Eml->each_part callback
	my ($ary, $self) = @_;
	my ($part, $depth, $idx) = @$ary;
	if ($idx ne '1' || $self->{-raw_hdr}) {
		open my $fh, '>', "$self->{curdir}/$idx.hdr" or die "open: $!";
		print $fh ${$part->{hdr}} or die "print $!";
		close $fh or die "close $!";
	}
	my $ct = $part->content_type || 'text/plain';
	my ($s, $err) = msg_part_text($part, $ct);
	my $sfx = defined($s) ? 'txt' : 'bin';
	open my $fh, '>', "$self->{curdir}/$idx.$sfx" or die "open: $!";
	print $fh ($s // $part->body) or die "print $!";
	close $fh or die "close $!";
}

# public
sub dump_eml ($$$) {
	my ($self, $dir, $eml) = @_;
	local $self->{curdir} = $dir;
	mkdir $dir or die "mkdir($dir): $!";
	$eml->each_part(\&write_part, $self);

	open my $fh, '>', "$dir/content_digest" or die "open: $!";
	my $dig = PublicInbox::ContentDigestDbg->new($fh);
	local $Data::Dumper::Useqq = 1;
	local $Data::Dumper::Terse = 1;
	content_digest($eml, $dig);
	print $fh "\n", $dig->hexdigest, "\n" or die "print $!";
	close $fh or die "close: $!";
}

# public
sub prep_a ($$) {
	my ($self, $eml) = @_;
	$self->{tmp} = File::Temp->newdir('mail-diff-XXXX', TMPDIR => 1);
	dump_eml($self, "$self->{tmp}/a", $eml);
}

1;
