# Copyright (C) all contributors <meta@public-inbox.org>
use v5.12;
local $ENV{TEST_IOPOLLER} = 'PublicInbox::Select';
require './t/ds-poll.t';
