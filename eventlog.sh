#!/bin/sh
stack install --profile --work-dir=.stackprofile --copy-bins && \
time stack -v exec --work-dir=.stackprofile -- bin/hchess-profiled +RTS -p -N8 -qg -hy -l-au
eventlog2html hchess-profiled.eventlog
