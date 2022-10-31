module Main where

import Chess
import AI
import Position
import Evaluation

main :: IO ()
main = do
    -- let res = edgeGreed startPosition 4 -- 37 GB
    -- let res = dig 4 White (Evaluated startPosition 0.0 WhiteToPlay) -- 6.5 GB
    let res = edgeGreed startPosition 5 -- 958M
    print res