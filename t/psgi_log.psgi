#!/usr/bin/perl -w
# Copyright (C) all contributors <meta@public-inbox.org>
# License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
# Usage: plackup [OPTIONS] /path/to/this/file
use v5.12;
use PublicInbox::WWW;
use Plack::Builder;
my $www = PublicInbox::WWW->new;
$www->preload;
builder {
	enable 'AccessLog::Timed',
		logger => sub { syswrite(STDOUT, $_[0]) },
		format => '%t "%r" %>s %b %D';
	enable 'Head';
	sub { $www->call($_[0]) }
}
