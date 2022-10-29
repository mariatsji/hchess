module Main where

import AI
import Position

main :: IO ()
main = do
    print $ edgeGreed startPosition 1 