module Main where

import AI
import Position
import Printer

main :: IO ()
main = do
    -- let res = edgeGreed startPosition 4 -- 37 GB
    -- let res = dig 4 White (Evaluated startPosition 0.0 WhiteToPlay) -- 6.5 GB
    let Right res = edgeGreed startPosition 2 -- 26.5GB (depth = 3 broadness = 200)   Total   time   16.839s  ( 12.948s elapsed)
    pretty res