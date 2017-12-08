# hchess

# run tests continously

    ./stackw test --file-watch

# start app

    ./stackw build --fast --pedantic
    ./stackw exec hchess-exe
    
# todo
    
    -- build fast --pedantic
    - current search assumes worst possible moves from opponent. must switch highest and lowest search for each folding of the position tree.
    - quickcheck properties to searched trees
    - todo chose the shorter path to same evaluation
    - 50 move rule?
    - pgn?
    - load/save ?