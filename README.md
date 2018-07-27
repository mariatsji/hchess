# hchess

# run tests continously

    ./stackw test --file-watch

# start app

    ./stackw build --fast --pedantic
    ./stackw exec hchess-exe
    
# profile performance

    ./stackw build --profile && \
    ./stackw exec -- hchess-exe +RTS -p

# profile with threadscope

    ./stackw build --profile && \
    ./stackw exec -- hchess-exe +RTS -ls -N2

# run benchmarks

    ./stackw bench
    
# todo
    
    - Writer Monad to append Pgn in Move.hs
    - refactors
    - quickcheck properties to searched trees
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure