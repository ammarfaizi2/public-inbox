#!/bin/sh
# sample --post-update-hook for t/clone-coderepo.t test
case $CLONE_CODEREPO_TEST_OUT in
'') ;;
*) echo "dos $@" >> "$CLONE_CODEREPO_TEST_OUT" ;;
esac
