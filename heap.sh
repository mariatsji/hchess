#!/bin/sh
stack install --force-dirty --profile && \
hchess-profiled +RTS -hr -i0.005 && \
hp2pretty hchess-profiled.hp && open hchess-profiled.svg
