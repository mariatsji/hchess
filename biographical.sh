#!/bin/sh
stack install --profile --ghc-options=-fprof-auto --force-dirty && \
hchess-profiled +RTS -hb -i0.02 && \
hp2pretty hchess-profiled.hp && open hchess-profiled.svg
