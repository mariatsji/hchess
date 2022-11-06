module GUI (render, handleInput, step) where

import Chess (board, movePiece)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Debug.Trace as Debug
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (..), MouseButton (..))
import Graphics.Gloss.Interface.IO.Interact (Event, KeyState (..))
import Graphics.Gloss.Juicy
import Position (Color (..), Move, Piece (..), Position (m), Square (..), toList')
import System.IO.Unsafe (unsafePerformIO)

type World = (Position, Maybe Square) -- need to store mouse down square (from square)

render :: World -> Picture
render (pos, maybeFrom) = Pictures $ drawPiece <$> toList' (m pos)

handleInput :: Event -> World -> World
handleInput event (pos, mSquare) = Debug.trace (analyze event) $ case event of
    EventKey (MouseButton LeftButton) Down _ xy ->
        let foundSquare = findSquare xy
         in Debug.trace (show foundSquare) (pos, foundSquare)
    EventKey (MouseButton LeftButton) Up _ xy ->
        let mSquareTo = findSquare xy
            newPos = movePiece pos <$> mSquare <*> mSquareTo
         in Debug.trace (show mSquare <> "-" <> show mSquareTo) case newPos of
                Nothing -> (pos, mSquare)
                Just new -> (new, Nothing)
    _ -> (pos, mSquare)

findSquare :: (Float, Float) -> Maybe Square
findSquare (x, y) = listToMaybe [square | square <- board, xCoord square `closeTo` x && yCoord square `closeTo` y]
  where
    closeTo :: Float -> Float -> Bool
    closeTo a b = abs (a - b) < 15

analyze :: Event -> String
analyze =
    show

step :: Float -> World -> World
step _ (pos, mSquare) = (pos, mSquare)

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
    foldMap emptySquare board

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