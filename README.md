# hchess

# run tests continously

    ./stackw test --file-watch

# start app

    ./stackw build --fast --pedantic
    ./stackw exec hchess-exe
    
# profile performance

    ./stackw build --profile && \
    ./stackw exec -- hchess-exe +RTS -p
    
# todo
    
    - refactors
    - quickcheck properties to searched trees
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure