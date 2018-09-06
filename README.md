# hchess

# run tests continously

    stack test --file-watch

# start app

    stack build --fast --pedantic
    stack exec hchess-exe
    
# profile performance

    stackbuild --profile && \
    stack exec -- hchess-exe +RTS -p

# profile with threadscope

    stack build --profile && \
    stack exec -- hchess-exe +RTS -ls -N2

# run benchmarks

    stack bench
    
# todo
    
    - Writer Monad to append Pgn in Move.hs
    - refactors
    - quickcheck properties to searched trees
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure