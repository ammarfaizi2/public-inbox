# Copyright (C) 2021 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# implements the "lei rm" command, you can point this at
# an entire spam mailbox or read a message from stdin
package PublicInbox::LeiRm;
use strict;
use v5.10.1;
use parent qw(PublicInbox::IPC PublicInbox::LeiInput);

sub input_eml_cb { # used by PublicInbox::LeiInput::input_fh
	my ($self, $eml) = @_;
	$self->{lei}->{sto}->ipc_do('remove_eml', $eml);
}

sub input_mbox_cb { # MboxReader callback
	my ($eml, $self) = @_;
	input_eml_cb($self, $eml);
}

sub input_net_cb { # callback for ->imap_each, ->nntp_each
	my (undef, undef, $kw, $eml, $self) = @_; # @_[0,1]: url + uid ignored
	input_eml_cb($self, $eml);
}

sub input_maildir_cb {
	my (undef, $kw, $eml, $self) = @_; # $_[0] $filename ignored
	input_eml_cb($self, $eml);
}

sub lei_rm {
	my ($lei, @inputs) = @_;
	$lei->_lei_store(1)->write_prepare($lei);
	$lei->{opt}->{stdin} = 1 if !@inputs;
	$lei->{opt}->{'in-format'} //= 'eml';
	my $self = bless { -wq_nr_workers => 1 }, __PACKAGE__;
	$self->prepare_inputs($lei, \@inputs) or return;
	my ($op_c, $ops) = $lei->workers_start($self, 1);
	$lei->{wq1} = $self;
	$lei->{-err_type} = 'non-fatal';
	net_merge_all_done($self) unless $lei->{auth};
	$op_c->op_wait_event($ops);
}

no warnings 'once';
*ipc_atfork_child = \&PublicInbox::LeiInput::input_only_atfork_child;
*net_merge_all_done = \&PublicInbox::LeiInput::input_only_net_merge_all_done;
*net_merge_all = \&PublicInbox::LeiAuth::net_merge_all;

1;
