module GUI (render, handleInput, step) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Interact (Event)
import Position ( Position, Square(Square) )

render :: Position -> Picture
render pos = emptyBoard

handleInput :: Event -> Position -> Position
handleInput _ pos = pos

step :: Float -> Position -> Position
step _ pos = pos

emptySquare :: Square -> Picture
emptySquare (Square c r) =
    let x c' = fromIntegral $ 60 * [ (-4) .. ] !! c'
        y r' = fromIntegral $ 60 * [ (-4) .. ] !! r'
        color' =
            if odd (c + r)
                then light aquamarine
                else dark azure
     in Color color' $ Translate (x c) (y r) $ Polygon [(-30,-30),(30, -30),(30,30),(-30, 30)]

emptyBoard :: Picture
emptyBoard =
    let squares = Square <$> [1 .. 8] <*> [1 .. 8]
     in foldMap emptySquare squares