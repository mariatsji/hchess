module Main where

import AI
import Position

main :: IO ()
main = do
    let res = edgeGreed startPosition 4
    print res