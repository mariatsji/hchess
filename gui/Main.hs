module Main where

import GI.GdkPixbuf.Enums (InterpType (InterpTypeBilinear))
import qualified GI.GdkPixbuf.Objects.Pixbuf as Pixbuf
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Fixed as Gtk.Fixed

-- legacy drag, replace with DragSource!
-- import qualified GI.Gtk.Objects.GestureClick as GestureClick
-- import qualified GI.Gtk.Objects.Widget as Widget

import GI.Gdk.Flags (DragAction (..))
import GI.Gdk.Objects.Drag (Drag (..))
import qualified GI.Gio.Objects.Application as Gio
import qualified GI.Gtk.Objects.DragSource as DragSource
import GI.Gtk.Objects.Image (imageGetPaintable)
import qualified GI.Gtk.Objects.Window as Window

import AI (bestDeepEval)
import Chess (identifyMove, pieceAt, playIfLegal)
import Position

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Int (Int32)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Conc (TVar, newTVarIO, readTVarIO, writeTVar)
import GHC.Conc.Sync (atomically)
import Relude

data World = World
    { world :: Position
    , highlighted :: Maybe Square
    , humanPlayer :: Color
    , tick :: Maybe Word32
    }

main :: IO ()
main = do
    worldVar <- newTVarIO $ World startPosition Nothing White Nothing
    app <- Gtk.applicationNew (Just appId) []

    Gio.onApplicationActivate app (appActivate app worldVar)
    _ <- Gio.applicationRun app Nothing
    pure ()

appId :: Text
appId = "io.grevling.hchess"

sizeScale :: Int32
sizeScale = 100

appActivate :: Gtk.Application -> TVar World -> IO ()
appActivate app worldVar = do
    window <- Gtk.applicationWindowNew app
    Gtk.setWindowTitle window "hChess"
    Gtk.setWindowResizable window True
    Gtk.setWindowDefaultWidth window 1000
    Gtk.setWindowDefaultHeight window 1000

    fixedArea <- Gtk.Fixed.fixedNew

    Window.windowSetChild window (Just fixedArea)

    -- add event listener to drags
    -- replacing this with dragsource
    {--
    clickEvent <- GestureClick.gestureClickNew
    GestureClick.onGestureClickPressed clickEvent $ clickBegin fixedArea worldVar
    GestureClick.onGestureClickReleased clickEvent $ clickEnd fixedArea worldVar
    Widget.widgetAddController
        fixedArea
        clickEvent
    --}

    drawWorld fixedArea worldVar

    Gtk.widgetShow fixedArea
    Gtk.widgetShow window

dragBegin :: Drag -> IO () -- DragSource.DragSourceDragBeginCallback
dragBegin drag = do
    print "drag goin on!"

drawWorld :: Gtk.Fixed.Fixed -> TVar World -> IO ()
drawWorld fixedArea worldVar = do
    World {..} <- readTVarIO worldVar
    traverse_
        (drawSquare fixedArea highlighted)
        (toList' (m world))
    print "I just drew the world!"

drawSquare :: Gtk.Fixed.Fixed -> Maybe Square -> (Square, Maybe Piece) -> IO ()
drawSquare fixed mHighlight (sq@(Square c r), mPiece) = do
    (Just buf) <-
        Pixbuf.pixbufNewFromFile $
            if mHighlight == Just sq
                then "img/highlight.square.png"
                else
                    if even (c + r)
                        then "img/dark.square.png"
                        else "img/bright.square.png"

    bufScaled <- Pixbuf.pixbufScaleSimple buf sizeScale sizeScale InterpTypeBilinear

    finalSquareImage <-
        Gtk.imageNewFromPixbuf bufScaled
    size <- Gtk.getImagePixelSize finalSquareImage
    Gtk.imageSetPixelSize finalSquareImage sizeScale

    -- draw square behind the piece
    Gtk.Fixed.fixedPut
        fixed
        finalSquareImage
        (xCoord c)
        (yCoord r)

    maybe
        (pure ())
        ( \piece -> do
            (Just pieceBuf) <- loadPieceImage piece
            bufScaled <- Pixbuf.pixbufScaleSimple pieceBuf sizeScale sizeScale InterpTypeBilinear
            pieceImage <- Gtk.imageNewFromPixbuf bufScaled
            Gtk.imageSetPixelSize pieceImage sizeScale

            dragSource <- DragSource.dragSourceNew
            DragSource.dragSourceSetActions dragSource [DragActionCopy, DragActionMove, DragActionLink, DragActionAsk, AnotherDragAction 0]
            DragSource.onDragSourceDragBegin dragSource dragBegin
            mPaintable <- imageGetPaintable pieceImage
            DragSource.dragSourceSetIcon
                dragSource
                mPaintable
                0
                0
            Gtk.widgetAddController fixed dragSource

            Gtk.Fixed.fixedPut
                fixed
                pieceImage
                (xCoord c)
                (yCoord r)
        )
        mPiece

clickBegin :: Gtk.Fixed.Fixed -> TVar World -> Int32 -> Double -> Double -> IO ()
clickBegin fixed worldVar nrClicks x y =
    case findSquare x y of
        Just sq -> do
            w <- readTVarIO worldVar
            when (toPlay (world w) == humanPlayer w && fmap colr (pieceAt (world w) sq) == Just (humanPlayer w)) $ do
                atomically $ writeTVar worldVar w {highlighted = Just sq}
            drawWorld fixed worldVar
        Nothing -> pure ()

clickEnd :: Gtk.Fixed.Fixed -> TVar World -> Int32 -> Double -> Double -> IO ()
clickEnd fixed worldVar nrClicks x y =
    case findSquare x y of
        Just toSquare -> do
            print "callback triggered"
            w@World {..} <- readTVarIO worldVar
            case highlighted of
                Just fromSquare -> do
                    let move = identifyMove world fromSquare toSquare Nothing -- todo promotion piece
                    print move
                    case playIfLegal move world of
                        Left s -> print s -- todo
                        Right newPos -> do
                            let newWorld =
                                    w
                                        { world = newPos
                                        , highlighted = Nothing
                                        }
                            atomically $ writeTVar worldVar newWorld
                            drawWorld fixed worldVar
                            void $ Gtk.widgetAddTickCallback fixed (computerThinking worldVar fixed)
                            Gtk.widgetQueueDraw fixed
                            print "clickEnd complete!"
                Nothing -> pure ()
        Nothing -> pure ()

computerThinking :: TVar World -> Gtk.Fixed.Fixed -> Gtk.Widget -> a -> IO Bool
computerThinking worldVar fixed widget frameClock = do
    w@World {..} <- readTVarIO worldVar
    case bestDeepEval world 3 of
        (Nothing, stat) ->
            print stat -- todo
        (Just responsePos, status) -> do
            let responseWorld =
                    w
                        { world = responsePos
                        , highlighted = Nothing
                        }
            atomically $ do
                writeTVar worldVar responseWorld

            drawWorld fixed worldVar
            Gtk.widgetQueueDraw fixed
            print "computer done thinking, repainted"
    pure False

findSquare :: Double -> Double -> Maybe Square
findSquare x y =
    let inside (Square c r) =
            x > xCoord c
                && x < xCoord c + fromIntegral sizeScale
                && y > yCoord r
                && y < yCoord r + fromIntegral sizeScale
     in listToMaybe
            [ Square co ro
            | co <- [1 .. 8]
            , ro <- [1 .. 8]
            , inside (Square co ro)
            ]

-- todo buffer this plz
loadPieceImage :: Piece -> IO (Maybe Pixbuf.Pixbuf)
loadPieceImage piece = do
    let filename = case piece of
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
    Pixbuf.pixbufNewFromFile filename

-- ---> x (more x is more to the right)
xCoord :: Int -> Double
xCoord c = fromIntegral sizeScale * ([0 .. 8] !! c)

{--
  |
  |
  | y (more y is more down)
--}
yCoord :: Int -> Double
yCoord r = let marginTop = 80 in (fromIntegral sizeScale * (reverse [0 .. 8] !! r)) + marginTop
