#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

cask exec $EMACS -batch -l ert -l line-scraper.el -l line-scraper-test.el -f ert-run-tests-batch-and-exit
