# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
# Prints OS-specific package profiles to stdout (one per line) to use
# as command-line args for ci/deps.perl.  Called automatically by ci/run.sh
eval 'exec perl -wS $0 ${1+"$@"}' # no shebang
if 0; # running under some shell
use v5.12;
BEGIN { require './install/os.perl' }
my $TASKS = do {
	if ($ID =~ /\A(?:free|net|open)bsd\z/) { <<EOM
all devtest Xapian-
all devtest IO::KQueue-
all devtest IO::KQueue
all devtest Inline::C-
all devtest Inline::C
EOM
	} elsif ($ID eq 'debian') { <<EOM
all devtest
all devtest Xapian-
all devtest-
v2essential
essential
essential devtest-
EOM
	} elsif ($ID eq 'centos') { <<EOM
v2essential devtest
essential devtest
all Xapian-
EOM
	} else { die "TODO: support ID=$ID VERSION_ID=$VERSION_ID" }
};

# this output is read by ci/run.sh and fed to install/deps.perl:
print $TASKS;
