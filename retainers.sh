#!/bin/sh
stack install --profile --ghc-options=-fprof-auto --force-dirty && \
hchess-profiled +RTS -hr -i0.001 && \
hp2pretty hchess-profiled.hp && open hchess-profiled.svg
