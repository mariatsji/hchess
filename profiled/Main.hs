module Main where

import AI
import Position

main :: IO ()
main = do
    -- let res = edgeGreed startPosition 4 -- 37 GB
    -- let res = dig 4 White (Evaluated startPosition 0.0 WhiteToPlay) -- 6.5 GB
    let res = edgeGreed startPosition 3 -- 84.8GB (depth = 3 broadness = 200)
    print res