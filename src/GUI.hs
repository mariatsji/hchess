module GUI (render, handleInput, step) where

import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graphics.Gloss.Juicy
import Position (Color (White), Piece (Pawn), Position (m), Square (..), toList')
import System.IO.Unsafe (unsafePerformIO)

render :: Position -> Picture
render pos = Pictures $ drawPiece <$> toList' (m pos)

handleInput :: Event -> Position -> Position
handleInput _ pos = pos

step :: Float -> Position -> Position
step _ pos = pos

emptySquare :: Square -> Picture
emptySquare squ@Square {..} =
    let color' =
            if odd (col + row)
                then light aquamarine
                else dark azure
     in Color color' $ Translate (xCoord squ) (yCoord squ) emptySquareImg

emptySquareImg :: Picture
emptySquareImg = Polygon [(-30, -30), (30, -30), (30, 30), (-30, 30)]

xCoord :: Square -> Float
xCoord (Square c _) = fromIntegral $ 60 * [(-6) ..] !! c

yCoord :: Square -> Float
yCoord (Square _ r) = fromIntegral $ 60 * [(-4) ..] !! r

emptyBoard :: Picture
emptyBoard =
    let squares = Square <$> [1 .. 8] <*> [1 .. 8]
     in foldMap emptySquare squares

drawPiece :: (Square, Maybe Piece) -> Picture
drawPiece (squ@(Square c r), mPiece) =
    let translated = Translate (xCoord squ) (yCoord squ)
     in case mPiece of
            Just (Pawn White) -> translated $ Pictures [emptySquare squ, whitePawn]
            _ -> emptySquare squ

whitePawn :: Picture
whitePawn = fromMaybe (error "unable to load white pawn image") $
    unsafePerformIO $
        loadJuicyPNG "img/PawnWhite.png"