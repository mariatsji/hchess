module Main where

import AI
import Chess
import Position

main :: IO ()
main = do
    print $ positionTree startPosition