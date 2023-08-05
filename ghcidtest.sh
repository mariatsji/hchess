#!/usr/bin/env bash
set -euo pipefail

ghcid --command='cabal repl --enable-multi-repl=all' --test 'main' --setup ":set args --hide-successes"