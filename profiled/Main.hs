module Main where

import Chess
import Position

main :: IO ()
main = do
    print $ length $ positionTree startPosition