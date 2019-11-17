#!/bin/sh
./stackw install --profile --work-dir=.stackprofile --copy-bins && \
time ./stackw -v exec --work-dir=.stackprofile -- hchess-profiled +RTS -N8 -p -s -hr -qg
