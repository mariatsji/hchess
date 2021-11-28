#!/bin/sh
stack run --copy-bins hchess-profiled --profile && \
bin/hchess-profiled +RTS -s -p
