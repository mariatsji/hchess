#!/bin/sh
stack install --force-dirty --profile && \
hchess-profiled +RTS -s -p
