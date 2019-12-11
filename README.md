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
    hp2pretty hchess-profiled.hp
    open hchess-profiled.svg


## Retainer profiling

    hchess-profiled +RTS -hc -L80 -hbvoid
    hp2pretty hchess-profiled.hp && open hchess-profiled.svg

# profile with threadscope

    stack build --profile && \
    stack bench --profile +RTS -h -s

# run benchmarks

    stack bench
    
