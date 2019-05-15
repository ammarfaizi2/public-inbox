# Copyright (C) 2016-2018 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
use strict;
use warnings;
use Test::More;
use PublicInbox::Config;
my $cfgpfx = "publicinbox.test";
{
	my $config = PublicInbox::Config->new({
		"$cfgpfx.address" => 'test@example.com',
		"$cfgpfx.mainrepo" => '/path/to/non/existent',
		"$cfgpfx.httpbackendmax" => 12,
	});
	my $ibx = $config->lookup_name('test');
	my $git = $ibx->git;
	my $old = "$git";
	my $lim = $git->{-httpbackend_limiter};
	ok($lim, 'Limiter exists');
	is($lim->{max}, 12, 'limiter has expected slots');
	$ibx->{git} = undef;
	$git = $ibx->git;
	isnt($old, "$git", 'got new Git object');
	is("$git->{-httpbackend_limiter}", "$lim", 'same limiter');
}

{
	my $config = PublicInbox::Config->new({
		'publicinboxlimiter.named.max' => 3,
		"$cfgpfx.address" => 'test@example.com',
		"$cfgpfx.mainrepo" => '/path/to/non/existent',
		"$cfgpfx.httpbackendmax" => 'named',
	});
	my $ibx = $config->lookup_name('test');
	my $git = $ibx->git;
	ok($git, 'got git object');
	my $old = "$git";
	my $lim = $git->{-httpbackend_limiter};
	ok($lim, 'Limiter exists');
	is($lim->{max}, 3, 'limiter has expected slots');
	$ibx->{git} = undef;
	my $new = $ibx->git;
	isnt($old, "$new", 'got new Git object');
	is("$new->{-httpbackend_limiter}", "$lim", 'same limiter');
	is($lim->{max}, 3, 'limiter has expected slots');
}

done_testing;
