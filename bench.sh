#!/bin/sh
./stackw install --profile --work-dir=.stackprofile --copy-bins && \
time ./stackw exec --work-dir=.stackprofile -- hchess-profiled +RTS -N8 -p -s -qg
