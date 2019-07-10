# hchess

# run tests continously

    stack test --file-watch

# start app

    stack build
    stack exec hchess
    
# profile performance

    stack install --profile --work-dir=.stackprofile && \
    time stack exec --work-dir=.stackprofile -- hchess-bench-exe +RTS -N8 -hc -p -s -K100M

    less hchess-profiled.prof
    hp2ps -e8in -c hchess-profiled.hp
    open hchess-profiled.ps

# profile with threadscope

    stack build --profile && \
    stack bench --profile +RTS -h -s

# run benchmarks

    stack bench
    
# todo
    
    - Writer Monad to append Pgn in Move.hs
    - refactors
    - quickcheck properties to searched trees
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure