module GUI (render, handleInput, step) where

import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graphics.Gloss.Juicy
import Position (Color (..), Piece (..), Position (m), Square (..), toList')
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
     in Color color' emptySquareImg

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
     in translated $ case mPiece of
            Just piece -> piecePicture piece `over` emptySquare squ
            _ -> emptySquare squ

piecePicture :: Piece -> Picture
piecePicture piece = fromMaybe (error "unable to load piece image") $
    unsafePerformIO $
        loadJuicyPNG $ case piece of
            Pawn White -> "img/PawnWhite.png"
            Pawn Black -> "img/PawnBlack.png"
            Knight White -> "img/KnightWhite.png"
            Knight Black -> "img/KnightBlack.png"
            Bishop White -> "img/BishopWhite.png"
            Bishop Black -> "img/BishopBlack.png"
            Rook White -> "img/RookWhite.png"
            Rook Black -> "img/RookBlack.png"
            Queen White -> "img/QueenWhite.png"
            Queen Black -> "img/QueenBlack.png"
            King White -> "img/KingWhite.png"
            King Black -> "img/KingBlack.png"


over :: Picture -> Picture -> Picture
over foreground background = 
    let xScaleFactor = 0.1
        yScaleFactor = 0.1
        scaled = Scale xScaleFactor yScaleFactor foreground
    in Pictures [background, scaled]