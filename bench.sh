#!/bin/sh
stack install --profile --work-dir=.stackprofile --copy-bins && \
time stack -v exec --work-dir=.stackprofile -- bin/hchess-profiled +RTS -N8 -p -s -hr -qg
