# hChess

![Cover art](img/coverart.png "hChess")

# run tests

    nix-shell
    cabal test

# Start app with settings

Available options:
analysis - prints the calculated score for a position
black -- play as black against computer
bright -- bright color theme for black terminal backgrounds
dark -- dark color theme for white terminal backgrounds (default)
w2 - white search strength 2 (from 0 to 3)
b2 - black search strength 2 (from 0 to 3)
pgn=<absolute-or-relative-path-ending-in.pgn> - use this position as entrypoint

    nix-shell
    cabal run hchess -- analysis bright black w2 b2 pgn=./pgn/position.pgn

# Start app with GUI

on ice for now, wont work yet!

    nix-shell
    cabal run hchess-gui

# start app

    nix-shell
    cabal run hchess
    
# Run profiling

    nix-shell
    ./prof.sh


outputs hchess-profiled.hp and hchess-profiled.prof

# Install binary locally

    nix-env -i -f release.nix