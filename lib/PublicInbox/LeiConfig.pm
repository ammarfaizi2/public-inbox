# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
package PublicInbox::LeiConfig; # subclassed by LeiEditSearch
use v5.12;
use PublicInbox::PktOp;
use Fcntl qw(SEEK_SET);
use autodie qw(open seek);

sub cfg_do_edit ($;$) {
	my ($self, $reason) = @_;
	my $lei = $self->{lei};
	$lei->pgr_err($reason) if defined $reason;
	my $cmd = [ qw(git config --edit -f), $self->{-f} ];
	my $env = { GIT_CONFIG => $self->{-f} };
	$self->cfg_edit_begin if $self->can('cfg_edit_begin');
	# run in script/lei foreground
	my ($op_c, $op_p) = PublicInbox::PktOp->pair;
	# $op_p will EOF when $EDITOR is done
	$op_c->{ops} = { '' => [\&cfg_edit_done, $lei, $self] };
	$lei->send_exec_cmd([ @$lei{qw(0 1 2)}, $op_p->{op_p} ], $cmd, $env);
}

sub cfg_edit_done { # PktOp lei->do_env cb
	my ($lei, $self) = @_;
	open my $fh, '+>', undef or die "open($!)";
	my $cfg = do {
		local $lei->{2} = $fh;
		$lei->cfg_dump($self->{-f});
	} or do {
		seek($fh, 0, SEEK_SET);
		return cfg_do_edit($self, read_all($fh));
	};
	$self->cfg_verify($cfg) if $self->can('cfg_verify');
}

sub lei_config {
	my ($lei, @argv) = @_;
	$lei->{opt}->{'config-file'} and return $lei->fail(
		"config file switches not supported by `lei config'");
	if ($lei->{opt}->{edit}) {
		@argv and return $lei->fail(
'--edit must be used without other arguments');
		$lei->{opt}->{c} and return $lei->fail(
"`-c $lei->{opt}->{c}->[0]' not allowed with --edit");
		my $f = $lei->_lei_cfg(1)->{-f};
		cfg_do_edit(bless { lei => $lei, -f => $f }, __PACKAGE__);
	} elsif (@argv) { # let git-config do error-checking
		$lei->_config(@argv);
	} else {
		$lei->_help('no options given');
	}
}

1;
