#!/bin/sh
# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
set -e
SUDO=${SUDO-'sudo'} PERL=${PERL-'perl'} MAKE=${MAKE-'make'}
DO=${DO-''}

set -x
if test -f Makefile
then
	$DO $MAKE clean >/dev/null
fi
NPROC=${NPROC-$({ getconf _NPROCESSORS_ONLN || getconf NPROCESSORS_ONLN ||
	gnproc || nproc || echo 2; } 2>/dev/null)}

$PERL -w ci/profiles.perl | while read args
do
	$DO $SUDO $PERL -w ci/deps.perl $args
	$DO $PERL Makefile.PL
	$DO $MAKE -j${BUILD_JOBS-$NPROC}
	$DO $MAKE -j${TEST_JOBS-1} ${TEST_TARGET-test}
	$DO $MAKE clean >/dev/null
done
