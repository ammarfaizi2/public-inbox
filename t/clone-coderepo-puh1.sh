#!/bin/sh
# sample --post-update-hook for t/clone-coderepo.t test
case $CLONE_CODEREPO_TEST_OUT in
'') ;;
*) echo "uno $@" >> "$CLONE_CODEREPO_TEST_OUT" ;;
esac
