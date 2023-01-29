{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Position where

import Board
import Control.Parallel.Strategies (NFData)
import Data.Aeson
import Data.Bifunctor (first)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Word (Word8)
import GHC.Generics (Generic)

data Color = White | Black
    deriving stock (Eq, Ord, Enum, Show, Generic)
    deriving anyclass (NFData, FromJSON, ToJSON)

data Piece
    = Pawn Color
    | Knight Color
    | Bishop Color
    | Rook Color
    | Queen Color
    | King Color
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, FromJSON, ToJSON)

type Col = Int
type Row = Int

data Square = Square
    { col :: Col
    , row :: Row
    }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Show Square where
    show (Square col row) = toLetter col : show row

-- todo hack
toLetter :: Int -> Char
toLetter c = ('x' : ['a' ..]) !! c

type Snapshot = Board (Maybe Piece)

data Position = Position
    { m :: Snapshot
    , gamehistory :: [Snapshot]
    , pristineShortWhite :: Bool
    , pristineLongWhite :: Bool
    , pristineShortBlack :: Bool
    , pristineLongBlack :: Bool
    , toPlay :: Color
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (NFData)

data Move
    = MovedPiece Square Square
    | Promotion Square Square Piece
    | CastleShort
    | CastleLong
    | EnPassant Square Square
    deriving stock (Eq, Generic)
    deriving anyclass (NFData)

-- need Color because of castle notation is colorless atm..
movedFrom :: Move -> Color -> Square
movedFrom (MovedPiece from _) _ = from
movedFrom (Promotion from _ _) _ = from
movedFrom (EnPassant from _) _ = from
movedFrom CastleShort White = Square 5 1
movedFrom CastleShort Black = Square 5 8
movedFrom CastleLong White = Square 5 1
movedFrom CastleLong Black = Square 5 8

-- need Color because of castle notation is colorless atm..
movedTo :: Move -> Color -> Square
movedTo (MovedPiece _ to) _ = to
movedTo (Promotion _ to _) _ = to
movedTo CastleShort White = Square 7 1
movedTo CastleShort Black = Square 7 8
movedTo CastleLong White = Square 3 1
movedTo CastleLong Black = Square 3 8
movedTo (EnPassant _ to) _ = to

instance Show Move where
    show (MovedPiece from to) = show from <> "-" <> show to
    show (EnPassant from to) = show from <> "-" <> show to
    show (Promotion from to piece) = show from <> "-" <> show to <> toOneChar piece
      where
        toOneChar :: Piece -> String
        toOneChar (Queen _) = "Q"
        toOneChar (Rook _) = "R"
        toOneChar (Bishop _) = "B"
        toOneChar (Knight _) = "K"
        toOneChar _ = ""
    show CastleShort = "O-O"
    show CastleLong = "O-O-O"

next :: Color -> Color
next White = Black
next Black = White

mkPosition :: Position -> Snapshot -> Position
mkPosition pos@Position {..} snp =
    let changes = m `diff` snp
        piecesChanged = snd <$> changes
        squaresChanged = fst <$> changes
        whiteKingSuddled = Just (King White) `elem` piecesChanged
        whiteShortRookSuddled = hash (Square 8 1) `elem` squaresChanged && Just (Rook White) `elem` piecesChanged
        whiteLongRookSuddled = hash (Square 1 1) `elem` squaresChanged && Just (Rook White) `elem` piecesChanged
        blackKingSuddled = Just (King Black) `elem` piecesChanged
        blackShortRookSuddled = hash (Square 8 8) `elem` squaresChanged && Just (Rook Black) `elem` piecesChanged
        blackLongRookSuddled = hash (Square 8 1) `elem` squaresChanged && Just (Rook Black) `elem` piecesChanged
        newGH = m : gamehistory
     in pos
            { m = snp
            , gamehistory = newGH
            , pristineShortWhite = pristineShortWhite && not whiteKingSuddled && not whiteShortRookSuddled
            , pristineLongWhite = pristineLongWhite && not whiteKingSuddled && not whiteLongRookSuddled
            , pristineShortBlack = pristineShortBlack && not blackKingSuddled && not blackShortRookSuddled
            , pristineLongBlack = pristineLongBlack && not blackKingSuddled && not blackLongRookSuddled
            , toPlay = next toPlay
            }

hash :: Square -> Word8
hash (Square col row) = (fromIntegral row - 1) * 8 + (fromIntegral col - 1)

unHash :: Word8 -> Square
unHash i = Square ((fromIntegral i `rem` 8) + 1) ((fromIntegral i `quot` 8) + 1)

colr :: Piece -> Color
colr (Pawn c) = c
colr (Knight c) = c
colr (Bishop c) = c
colr (Rook c) = c
colr (Queen c) = c
colr (King c) = c

startPosition :: Position
startPosition =
    Position
        { m = startTree
        , gamehistory = []
        , pristineShortWhite = True
        , pristineLongWhite = True
        , pristineShortBlack = True
        , pristineLongBlack = True
        , toPlay = White
        }

startTree :: Snapshot
startTree = fromList' startSquarePieces

startSquarePieces :: [(Square, Piece)]
startSquarePieces = startWhitePieces <> startBlackPieces

startWhitePieces :: [(Square, Piece)]
startWhitePieces =
    [ (Square 1 1, Rook White)
    , (Square 2 1, Knight White)
    , (Square 3 1, Bishop White)
    , (Square 4 1, Queen White)
    , (Square 5 1, King White)
    , (Square 6 1, Bishop White)
    , (Square 7 1, Knight White)
    , (Square 8 1, Rook White)
    , (Square 1 2, Pawn White)
    , (Square 2 2, Pawn White)
    , (Square 3 2, Pawn White)
    , (Square 4 2, Pawn White)
    , (Square 5 2, Pawn White)
    , (Square 6 2, Pawn White)
    , (Square 7 2, Pawn White)
    , (Square 8 2, Pawn White)
    ]

startBlackPieces :: [(Square, Piece)]
startBlackPieces =
    [ (Square 1 7, Pawn Black)
    , (Square 2 7, Pawn Black)
    , (Square 3 7, Pawn Black)
    , (Square 4 7, Pawn Black)
    , (Square 5 7, Pawn Black)
    , (Square 6 7, Pawn Black)
    , (Square 7 7, Pawn Black)
    , (Square 8 7, Pawn Black)
    , (Square 1 8, Rook Black)
    , (Square 2 8, Knight Black)
    , (Square 3 8, Bishop Black)
    , (Square 4 8, Queen Black)
    , (Square 5 8, King Black)
    , (Square 6 8, Bishop Black)
    , (Square 7 8, Knight Black)
    , (Square 8 8, Rook Black)
    ]

movePiece' :: Snapshot -> Square -> Square -> Snapshot
movePiece' snp from to = case snp ?! hash from of
    Nothing ->
        error $ "should be a piece at " <> show from <> " in pos " <> show snp
    (Just piece) ->
        let without = removePieceAt snp from
         in replacePieceAt without to piece

removePieceAt :: Snapshot -> Square -> Snapshot
removePieceAt snp s = set snp (hash s) Nothing

replacePieceAt :: Snapshot -> Square -> Piece -> Snapshot
replacePieceAt snp square piece = set snp (hash square) (Just piece)

pieceAt' :: Snapshot -> Square -> Maybe Piece
pieceAt' snp s = snp ?! hash s

searchForPieces :: Position -> (Square -> Bool) -> (Piece -> Bool) -> [(Square, Piece)]
searchForPieces pos squarePred piecePred = catSndMaybes $ unHash <$.> searchIdx (m pos) (squarePred . unHash) (maybe False piecePred)

fromList' :: [(Square, Piece)] -> Snapshot
fromList' =
    foldr
        ( \(s, p) tree ->
            set tree (hash s) (pure p)
        )
        (empty64 Nothing)

toList' :: Snapshot -> [(Word8, Maybe Piece)]
toList' snp = searchIdx snp (const True) (const True)

toList'' :: Snapshot -> [(Square, Maybe Piece)]
toList'' snp = unHash <$.> searchIdx snp (const True) (const True)

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes = mapMaybe sequenceA

(<$.>) :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
(<$.>) = fmap . first

infixl 9 <$.>

emptyBoard :: Position
emptyBoard = Position (empty64 Nothing) [] True True True True White

-- [(35,Nothing),(36,Nothing),(43,Just (Pawn White))] (d5,e5,d6)
findMove :: Snapshot -> Snapshot -> Move
findMove a b =
    let changedSquaresAndPiece = first unHash <$> a `diff` b
        changedSquares = fst <$> changedSquaresAndPiece
     in case length changedSquares of
            4
                | Square 8 1 `elem` changedSquares -> CastleShort
                | Square 1 1 `elem` changedSquares -> CastleLong
                | Square 8 8 `elem` changedSquares -> CastleShort
                | Square 1 8 `elem` changedSquares -> CastleLong
                | otherwise -> error "could not determine position diff of length 4 that does not seem to be a castle"
            3 -> diffLength3Move b changedSquaresAndPiece
              where
                diffLength3Move _ [(_, Nothing), (from, Nothing), (to, Just (Pawn _))] = EnPassant from to
                diffLength3Move snp changes = MovedPiece (findFrom snp $ fst <$> changes) (findTo snp $ fst <$> changes)
            2
                | pawnMovedIn changedSquaresAndPiece a b -> Promotion (promfromSquare changedSquaresAndPiece) (promtoSquare changedSquaresAndPiece) (promtoPiece changedSquaresAndPiece)
                | otherwise -> MovedPiece (findFrom b changedSquares) (findTo b changedSquares)
            _ -> error $ "could not determine changed position when diff length " <> show (length changedSquares)

findFrom :: Snapshot -> [Square] -> Square
findFrom _ [] = error "Could not find from square in snapshot"
findFrom snp (s : xs) = if isNothing $ snp ?! hash s then s else findFrom snp xs

findTo :: Snapshot -> [Square] -> Square
findTo _ [] = error "Could not find to square in snapshot"
findTo snp (s : xs) = if isJust $ snp ?! hash s then s else findTo snp xs

epfromSquare :: [(Square, Maybe Piece)] -> Square
epfromSquare l =
    let Square r c = eptoSquare l
     in maybe (error "cant find epfromsquare") fst (find (\(Square r' c', _) -> r /= r' && c /= c') l)

eptoSquare :: [(Square, Maybe Piece)] -> Square
eptoSquare l = maybe (error "cant find eptosquare") fst (find (\(_, mp) -> isJust mp) l)

-- todo stupid
pawnMovedIn :: [(Square, Maybe Piece)] -> Snapshot -> Snapshot -> Bool
pawnMovedIn [] _ _ = False
pawnMovedIn ((s@(Square _ r), mp) : xs) from to
    | r == 7 && isNothing mp = from ?! hash s == Just (Pawn White)
    | r == 2 && isNothing mp = from ?! hash s == Just (Pawn Black)
    | otherwise = pawnMovedIn xs from to

-- todo stupid
promfromSquare :: [(Square, Maybe Piece)] -> Square
promfromSquare [] = error "could not determine promfromSquare"
promfromSquare ((Square c r, _) : xs)
    | r == 2 = Square c r
    | r == 7 = Square c r
    | otherwise = promfromSquare xs

-- todo stupid
promtoSquare :: [(Square, Maybe Piece)] -> Square
promtoSquare [] = error "could not determine promtoSquare"
promtoSquare ((Square c r, _) : xs)
    | r == 1 = Square c r
    | r == 8 = Square c r
    | otherwise = promtoSquare xs

-- todo stupid
promtoPiece :: [(Square, Maybe Piece)] -> Piece
promtoPiece [] = error "could not determine promtoSquare"
promtoPiece ((Square _ r, mp) : xs)
    | r == 1 = fromMaybe (error "expected white officer in promtosquare") mp
    | r == 8 = fromMaybe (error "expected black officer in promtosquare") mp
    | otherwise = promtoPiece xs
