# hChess

![Cover art](img/coverart.png "hChess")

# run tests

    nix-shell
    cabal test

# PGN support

hChess supports a subset of PGN - all games are saved to ./pgn
A PGN can be loaded into the engine at startup via command line options, see _Start app with settings_

An example of the recognized subset of PGN is

```PGN
[Event "Casual game"]
[Site "London ENG"]
[Date "1851.06.21"]
[EventDate "?"]
[Round "?"]
[Result "1-0"]
[White "Adolf Anderssen"]
[Black "Lionel Adalbert Bagration Felix Kieseritzky"]
[ECO "C33"]
[WhiteElo "?"]
[BlackElo "?"]
[Source "The Chess Player, vol.i no.1, 1851.07.19, p.2"]
[PlyCount "45"]

1.e4 e5 2.f4 exf4 3.Bc4 Qh4+ 4.Kf1 b5 5.Bxb5 Nf6 6.Nf3 Qh6
7.d3 Nh5 8.Nh4 Qg5 9.Nf5 c6 10.g4 Nf6 11.Rg1 cxb5 12.h4 Qg6
13.h5 Qg5 14.Qf3 Ng8 15.Bxf4 Qf6 16.Nc3 Bc5 17.Nd5 Qxb2 18.Bd6
Bxg1 {It is from this move that Black's defeat stems. Wilhelm
Steinitz suggested in 1879 that a better move would be
18... Qxa1+; likely moves to follow are 19. Ke2 Qb2 20. Kd2
Bxg1.} 19. e5 Qxa1+ 20. Ke2 Na6 21.Nxg7+ Kd8 22.Qf6+ Nxf6
23.Be7# 1-0
```

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