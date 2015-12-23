#!/usr/bin/perl -w
# Copyright (C) 2015 all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Note: this is part of our test suite, update t/plack.t if this changes
# Usage: plackup [OPTIONS] /path/to/this/file
use strict;
use warnings;
use PublicInbox::RepoBrowse;
use Plack::Request;
use Plack::Builder;
my $have_deflater = eval { require Plack::Middleware::Deflater; 1 };
my $repo_browse = PublicInbox::RepoBrowse->new;

builder {
	enable 'Plack::Middleware::Chunked';
	if ($have_deflater) {
		enable 'Deflater',
			content_type => [ 'text/html', 'text/plain',
					  'application/atom+xml' ];
	}
	enable 'Head';
	sub {
		my $req = Plack::Request->new(@_);
		$repo_browse->run($req, $req->method);
	}
}
