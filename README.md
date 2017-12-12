# hchess

# run tests continously

    ./stackw test --file-watch

# start app

    ./stackw build --fast --pedantic
    ./stackw exec hchess-exe
    
# todo
    
    - check & mate should eval high
    - quickcheck properties to searched trees
    - config level
    - chose the shorter path to same evaluation
    - memoization
    - eval/search data structure