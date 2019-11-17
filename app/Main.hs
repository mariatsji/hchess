{-# LANGUAGE BangPatterns #-}

module Main where

import GameLoop

main :: IO ()
main = do
  print "1 Human vs Human"
  print "2 Human vs Machine"
  print "3 Machine vs Machine"
  print "q Quit"
  l <- getLine
  start l

