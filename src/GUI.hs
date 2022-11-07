module GUI where

{--
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
render (pos, maybeFrom) = Pictures $ drawPiece maybeFrom <$> toList' (m pos)

handleInput :: Event -> World -> World
handleInput event (pos, Nothing) = case event of
    EventKey (MouseButton LeftButton) Down _ xy -> (pos, findSquare xy)
    _ -> (pos, Nothing)
handleInput event (pos, Just fromSquare) = case event of
    EventKey (MouseButton LeftButton) Up _ xy ->
        let mSquareTo = findSquare xy
         in if mSquareTo /= Just fromSquare
                then
                    let newPos = movePiece pos fromSquare <$> mSquareTo
                     in case newPos of
                            Nothing -> (pos, Nothing)
                            Just new -> (new, Nothing)
                else (pos, Just fromSquare)
    _ -> (pos, Just fromSquare)

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

emptySquare :: Bool -> Square -> Picture
emptySquare toHighlight squ@Square {..} =
    let color' =
            if odd (col + row)
                then light aquamarine
                else dark azure
        highlighted = if toHighlight then rose else color'
     in Color highlighted emptySquareImg

emptySquareImg :: Picture
emptySquareImg = Polygon [(-30, -30), (30, -30), (30, 30), (-30, 30)]

xCoord :: Square -> Float
xCoord (Square c _) = fromIntegral $ 60 * [(-6) ..] !! c

yCoord :: Square -> Float
yCoord (Square _ r) = fromIntegral $ 60 * [(-4) ..] !! r

emptyBoard :: Picture
emptyBoard =
    foldMap (emptySquare False) board

drawPiece :: Maybe Square -> (Square, Maybe Piece) -> Picture
drawPiece mFrom (squ@(Square c r), mPiece) =
    let translated = Translate (xCoord squ) (yCoord squ)
        bgSquare =
            if mFrom == Just squ
                then emptySquare True squ
                else emptySquare False squ
     in translated $ case mPiece of
            Just piece -> piecePicture piece `over` bgSquare
            _ -> bgSquare

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

     --}