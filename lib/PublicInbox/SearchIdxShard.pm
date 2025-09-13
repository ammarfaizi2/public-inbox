# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# Internal interface for a single Xapian shard in V2 inboxes.
# See L<public-inbox-v2-format(5)> for more info on how we shard Xapian
package PublicInbox::SearchIdxShard;
use v5.12;
use parent qw(PublicInbox::SearchIdx PublicInbox::IPC);
use PublicInbox::OnDestroy;
use PublicInbox::Syscall qw($F_SETPIPE_SZ);

sub new {
	my ($cls, $v2w, $shard) = @_; # v2w may be ExtSearchIdx
	my $self = $v2w->can('eidx_sync') ? $cls->eidx_shard_new($v2w, $shard)
			: $cls->SUPER::new(@$v2w{qw(ibx -opt)}, $shard);
	# create the DB before forking:
	$self->idx_acquire;
	$self->set_metadata_once;
	$self->idx_release;
	if ($v2w->{parallel}) {
		local $self->{-v2w_afc} = $v2w;
		$self->ipc_worker_spawn("shard[$shard]");
		# Increasing the pipe size for requests speeds V2 batch imports
		# across 8 cores by nearly 20%.  Since many of our responses
		# are small, make the response pipe as small as possible
		if ($F_SETPIPE_SZ) {
			fcntl($self->{-ipc_req}, $F_SETPIPE_SZ, 1048576);
			fcntl($self->{-ipc_res}, $F_SETPIPE_SZ, 4096);
		}
	}
	$self;
}

sub _worker_done { # OnDestroy cb
	my ($self) = @_;
	die "BUG: $$ $0 xdb active" if $self->need_xapian && $self->{xdb};
	die "BUG: $$ $0 txn active" if $self->{txn};
}

sub ipc_atfork_child { # called automatically before ipc_worker_loop
	my ($self) = @_;
	my $v2w = delete $self->{-v2w_afc} or die 'BUG: {-v2w_afc} missing';
	$v2w->atfork_child; # calls ipc_sibling_atfork_child on our siblings
	$v2w->{current_info} = "[$self->{shard}]"; # for $SIG{__WARN__}
	$self->begin_txn_lazy;
	# caller (ipc_worker_spawn) must capture this:
	on_destroy \&_worker_done, $self;
}

sub index_eml {
	my ($self, $eml, $smsg) = @_;
	$self->ipc_do('add_xapian', $eml, $smsg);
}

sub add_eidx_info {
	my ($self, $docid, $eidx_key, $eml) = @_;
	my @list_ids = $eml->header_raw('List-Id');
	$self->ipc_do('add_eidx_info_raw', $docid, $eidx_key, @list_ids);
}

sub idx_close {
	my ($self) = @_;
	die "BUG: $$ $0 txn active" if $self->{txn};
	$self->idx_release if $self->{xdb};
}

sub shard_close {
	my ($self) = @_;
	$self->ipc_do('idx_close');
	$self->ipc_worker_stop;
}

1;
