#!/bin/sh
./stackw install --profile --work-dir=.stackprofile --copy-bins && \
hchess-profiled +RTS -hb -i0.005 && \
hp2pretty hchess-profiled.hp && open hchess-profiled.svg
