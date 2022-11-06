module Main where

import qualified GUI
import Graphics.Gloss ( greyN, play, Display(InWindow) )
import Position ( startPosition )

main :: IO ()
main = do
    
    play
        (InWindow "hChess" (800, 600) (10, 10))
        (greyN 0.8) -- background color
        2 -- number of steps per second
        (startPosition, Nothing) -- initial world with no square mouse-clicked
        GUI.render -- function to convert world to a Picture
        GUI.handleInput -- function to handle input events
        (const id) -- a functino to step the world one iteration. it is passed the period of time *in seconds( needing to be advanced)
