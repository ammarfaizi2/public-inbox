# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ (https://www.gnu.org/licenses/agpl-3.0.txt)
use strict;
use warnings;
use Test::More;
use PublicInbox::Hval qw(to_attr from_attr);

foreach my $s ('Hello/World.pm', 'Zcat', 'hello world.c', 'Eléanor', '$at') {
	my $attr = to_attr($s);
	is(from_attr($attr), $s, "$s => $attr => $s round trips");
}

{
	my $bad = eval { to_attr('foo//bar') };
	my $err = $@;
	ok(!$bad, 'double-slash rejected');
	like($err, qr/invalid filename:/, 'got exception message');
}

done_testing();
