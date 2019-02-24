# hchess

# run tests continously

    stack test --file-watch

# start app

    stack build
    stack exec hchess
    
# profile performance

    stack install --profile --work-dir=.stackprofile && \
    time stack exec --work-dir=.stackprofile -- hchess-bench-exe +RTS -N8 -hc -p -s -K100M

    less hchess.prof
    hp2ps -e8in -c hchess.hp
    open hchess.ps

# profile with threadscope

    stack build --profile && \
    stack bench +RTS -ls -N2

# run benchmarks

    stack bench
    
# todo
    
    - Writer Monad to append Pgn in Move.hs
    - refactors
    - quickcheck properties to searched trees
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure