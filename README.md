# hchess

# run tests continously

    ./stackw test --file-watch

# start app

    ./stackw build --fast --pedantic
    ./stackw exec hchess-exe
    
# todo
    
    - quickcheck properties to searched trees
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure