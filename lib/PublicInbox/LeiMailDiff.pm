# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# The "lei mail-diff" sub-command, diffs input contents against
# the first message of input
package PublicInbox::LeiMailDiff;
use v5.12;
use parent qw(PublicInbox::IPC PublicInbox::LeiInput PublicInbox::MailDiff);
use PublicInbox::Spawn qw(run_wait);
require PublicInbox::LeiRediff;

sub diff_a ($$) {
	my ($self, $eml) = @_;
	my $dir = "$self->{tmp}/N".(++$self->{nr});
	$self->dump_eml($dir, $eml);
	my $cmd = [ qw(git diff --no-index) ];
	my $lei = $self->{lei};
	PublicInbox::LeiRediff::_lei_diff_prepare($lei, $cmd);
	push @$cmd, qw(-- a), "N$self->{nr}";
	my $rdr = { -C => "$self->{tmp}" };
	@$rdr{1, 2} = @$lei{1, 2};
	run_wait($cmd, $lei->{env}, $rdr) and $lei->child_error($?);
}

sub input_eml_cb { # used by PublicInbox::LeiInput::input_fh
	my ($self, $eml) = @_;
	$self->{tmp} ? diff_a($self, $eml) : $self->prep_a($eml);
}

sub lei_mail_diff {
	my ($lei, @argv) = @_;
	my $self = bless {}, __PACKAGE__;
	$self->prepare_inputs($lei, \@argv) or return;
	my $isatty = -t $lei->{1};
	$lei->{opt}->{color} //= $isatty;
	$lei->start_pager if $isatty;
	$lei->{-err_type} = 'non-fatal';
	$self->{-raw_hdr} = $lei->{opt}->{'raw-header'};
	$lei->wq1_start($self);
}

no warnings 'once';
*net_merge_all_done = \&PublicInbox::LeiInput::input_only_net_merge_all_done;
1;
