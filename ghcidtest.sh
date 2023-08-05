#!/usr/bin/env bash
set -euo pipefail

ghcid --command='cabal repl --enable-multi-repl lib:hchess exe:hchess test:hchess-test' -T=":!cabal test"