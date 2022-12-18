{-# LANGUAGE DeriveGeneric #-}

module Chess where

import Board (diff)
import Control.DeepSeq (force)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (NFData)
import Data.List
import Data.Maybe (isNothing)
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import Position
import Prelude hiding (foldr)
import qualified Data.Set as Set

data Status
    = WhiteToPlay
    | BlackToPlay
    | Remis
    | WhiteIsMate
    | BlackIsMate
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

board :: [Square]
board = Square <$> [1 .. 8] <*> [1 .. 8]

-- col row
squareTo :: Square -> Int -> Int -> Square
squareTo (Square c r) cols rows = Square (c + cols) (r + rows)

{-     let newC = c + cols
        newR = r + rows
    in if newC < 1 || newC > 8 || newR < 1 || newR > 8
        then Debug.trace ("Dbg: " <> show (Square newC newR)) (Square newC newR)
        else Square newC newR -}

-- MovedPiece Square Square | Promotion Square Square Piece | CastleShort | CastleLong
identifyMove :: Position -> Square -> Square -> Maybe Piece -> Move
identifyMove pos from to mPromPiece =
    let kingpos = kingPos (toPlay pos) -- todo reuse these
        shortcastlekingpos = squareTo kingpos 2 0
        longcastlekingpos = squareTo kingpos (-2) 0
        promRow = if toPlay pos == White then 8 else 1
        isProm = pieceAt pos from == Just (Pawn (toPlay pos)) && row to == promRow
     in case (isProm, mPromPiece) of
            (True, Just piece) -> Promotion from to piece
            _ ->
                if from == kingpos && to == shortcastlekingpos
                    then CastleShort
                    else
                        if from == kingpos && to == longcastlekingpos
                            then CastleLong
                            else MovedPiece from to

-- todo express shorter plz
playIfLegal :: Move -> Position -> Either String Position
playIfLegal move pos = do
    case pieceAt pos (movedFrom move (toPlay pos)) of
        Nothing -> Left "No piece at that from-square"
        Just p ->
            if colr p == toPlay pos
                then
                    let moveAttempt = case move of
                            MovedPiece from to -> movePiece pos from to
                            CastleShort -> head $ castle' CastleShort pos -- todo head..
                            CastleLong -> head $ castle' CastleLong pos -- todo head..
                            Promotion from to piece -> movePiecePromote pos from to piece
                        tree = positionTree pos
                        isAmongLegalMoves = any (eqPosition moveAttempt) tree
                     in if isAmongLegalMoves
                            then Right moveAttempt
                            else Left "Move not among legal moves"
                else Left "Sorry, that piece is not for you to move"

-- flips toPlay
movePiece :: Position -> Square -> Square -> Position
movePiece pos@(Position m' _ _ _ tp) from@(Square fc _) to@(Square tc tr)
    | pieceAt pos from == Just (Pawn White) && (fc /= tc) && vacantAt pos to =
        let newSnapshot =
                movePiece' (removePieceAt m' (Square tc (tr - 1))) from to -- todo danger (tr - 1)
         in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos)
    | pieceAt pos from == Just (Pawn Black) && (fc /= tc) && vacantAt pos to =
        let newSnapshot =
                movePiece' (removePieceAt m' (Square tc (tr + 1))) from to -- todo danger (tr + 1)
         in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos)
    | otherwise =
        let newSnapshot = movePiece' m' from to
            newCastleStatusWhite = case tp of
                White -> mkNewCastleStatus pos White from
                Black -> castleStatusWhite pos
            newCastleStatusBlack = case tp of
                Black -> mkNewCastleStatus pos Black from
                White -> castleStatusBlack pos
         in mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack

movePiecePromote :: Position -> Square -> Square -> Piece -> Position
movePiecePromote pos from toSquare promPiece =
    let newPos = movePiece pos from toSquare
        snap = m newPos
     in newPos {m = replacePieceAt snap toSquare promPiece}

mkNewCastleStatus :: Position -> Color -> Square -> CastleStatus
mkNewCastleStatus pos White from = case from of
    Square 1 1 -> case castleStatusWhite pos of
        CanCastleBoth -> CanCastleH
        CanCastleH -> CanCastleH
        _ -> CanCastleNone
    Square 5 1 -> CanCastleNone
    Square 8 1 -> case castleStatusWhite pos of
        CanCastleBoth -> CanCastleA
        CanCastleA -> CanCastleA
        _ -> CanCastleNone
    _ -> castleStatusWhite pos
mkNewCastleStatus pos Black from = case from of
    Square 1 8 -> case castleStatusBlack pos of
        CanCastleBoth -> CanCastleH
        CanCastleH -> CanCastleH
        _ -> CanCastleNone
    Square 5 8 -> CanCastleNone
    Square 8 8 -> case castleStatusBlack pos of
        CanCastleBoth -> CanCastleA
        CanCastleA -> CanCastleA
        _ -> CanCastleNone
    _ -> castleStatusBlack pos

points :: Square -> Square -> [Square]
points (Square c1 r1) (Square c2 r2)
    | c1 == c2 =
        Square c1 <$> [min r1 r2 + 1 .. max r1 r2 - 1]
    | r1 == r2 =
        flip Square r1 <$> [min c1 c2 + 1 .. max c1 c2 - 1]
    | otherwise =
        let cs =
                if c1 < c2
                    then [c1 + 1 .. c2 - 1]
                    else reverse [c2 + 1 .. c1 - 1]
            rs =
                if r1 < r2
                    then [r1 + 1 .. r2 - 1]
                    else reverse [r2 + 1 .. r1 - 1]
         in uncurry Square <$> cs `zip` rs

-- todo par?
canGoThere :: Position -> Square -> Square -> Bool
canGoThere pos from to =
    let vacant = finalDestinationNotOccupiedBySelf pos to
        clear = all isNothing (fmap (pieceAt pos) (points from to))
     in vacant && clear

canGoThere' :: Snapshot -> Color -> Square -> Square -> Bool
canGoThere' snp myColor from to =
    let vacant = finalDestinationNotOccupiedBySelf' snp myColor to
        clear = all isNothing (fmap (pieceAt' snp) (points from to))
     in vacant && clear

finalDestinationNotOccupiedBySelf :: Position -> Square -> Bool
finalDestinationNotOccupiedBySelf Position {..} = finalDestinationNotOccupiedBySelf' m toPlay

finalDestinationNotOccupiedBySelf' :: Snapshot -> Color -> Square -> Bool
finalDestinationNotOccupiedBySelf' snp myColor to =
    fmap colr (pieceAt' snp to) /= Just myColor

enemyAt :: Position -> Square -> Bool
enemyAt pos to =
    let enemyColor = next (toPlay pos)
     in (colr <$> pieceAt pos to) == Just enemyColor

vacantAt :: Position -> Square -> Bool
vacantAt pos t = isNothing $ pieceAt pos t

makeMoves :: Position -> [(Square, Square)] -> Position
makeMoves = foldl (uncurry . movePiece)

pieceAt :: Position -> Square -> Maybe Piece
pieceAt po s@(Square c r) =
    if c < 1 || c > 8 || r < 1 || r > 8
        then Debug.trace ("Dbg: " <> show s) go po s
        else go po s
  where
    go = pieceAt' . m

positionTree :: Position -> [Position]
positionTree pos = filter (notSelfcheck (toPlay pos)) $ positionTreeIgnoreCheck pos

notSelfcheck :: Color -> Position -> Bool
notSelfcheck col pos = not $ isInCheck (pos {toPlay = col})

-- flips toPlay in all positions! but.. does it flip when you are in check? correct.. has a bug in it
positionTreeIgnoreCheck :: Position -> [Position]
positionTreeIgnoreCheck pos =
    let c = toPlay pos
        allPieceSquares = searchForPieces pos (const True) (\p -> colr p == c)
        prPiece = positionsPrPiece pos =<< allPieceSquares -- flipped
        castled = castle pos -- flipped
     in prPiece <> castled

-- flips toPlay, does promotions too
positionsPrPiece :: Position -> (Square, Piece) -> [Position]
positionsPrPiece pos@(Position snp _ _ _ _) (s, p) = case p of
    Pawn _ ->
        let squares = filter (canGoThere pos s . fst) $ toSquaresPawn pos s
            positions =
                squares
                    >>= ( \t ->
                            let fromSquare = s
                                toSquare = fst t
                                promRow = if toPlay pos == White then 8 else 1 -- todo reuse
                             in case snd t of
                                    Nothing ->
                                        if row toSquare == promRow
                                            then
                                                let naive = movePiece pos fromSquare toSquare
                                                    snap = m naive
                                                    color = toPlay pos
                                                    replaced piece = naive {m = replacePieceAt snap toSquare piece}
                                                 in [ replaced (Rook color)
                                                    , replaced (Knight color)
                                                    , replaced (Bishop color)
                                                    , replaced (Queen color)
                                                    ]
                                            else [movePiece pos fromSquare toSquare]
                                    Just s' ->
                                        [movePiece pos {m = removePieceAt snp s'} s toSquare] -- En passant
                        )
         in positions
    Knight _ ->
        fmap
            (movePiece pos s)
            (filter (finalDestinationNotOccupiedBySelf pos) $ toSquaresKnight s)
    Bishop _ ->
        fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresBishop s)
    Rook _ ->
        fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresRook s)
    Queen _ ->
        fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresQueen s)
    King _ ->
        fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresKing s)

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: Position -> Square -> [(Square, Maybe Square)]
toSquaresPawn pos s@(Square c r)
    | toPlay pos == White =
        [(squareTo s 0 2, Nothing) | r == 2, vacantAt pos $ squareTo s 0 2]
            <> [(squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1]
            <> [(squareTo s (-1) 1, Nothing) | c > 1, enemyAt pos $ squareTo s (-1) 1]
            <> [(squareTo s 1 1, Nothing) | c < 8, enemyAt pos $ squareTo s 1 1]
            <> [(squareTo s (-1) 1, Just (squareTo s (-1) 0)) | c > 1, enPassant pos (squareTo s (-1) 0)]
            <> [(squareTo s 1 1, Just (squareTo s 1 0)) | c < 8, enPassant pos (squareTo s 1 0)]
    | otherwise =
        [(squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos $ squareTo s 0 (-2)]
            <> [(squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1)]
            <> [(squareTo s (-1) (-1), Nothing) | c > 1, enemyAt pos $ squareTo s (-1) (-1)]
            <> [(squareTo s 1 (-1), Nothing) | c < 8, enemyAt pos $ squareTo s 1 (-1)]
            <> [(squareTo s (-1) (-1), Just (squareTo s (-1) 0)) | c > 1, enPassant pos (squareTo s (-1) 0)]
            <> [(squareTo s 1 (-1), Just (squareTo s 1 0)) | c < 8, enPassant pos (squareTo s 1 0)]

-- en passant
-- the square arg is the capture square
enPassant :: Position -> Square -> Bool
enPassant Position {..} captureSquare@(Square _ r) =
    let rightRow = r == if toPlay == White then 5 else 4
        di = m `diff` head gamehistory
        fromSquare = squareTo captureSquare 0 (if toPlay == White then 2 else -2)
        opPawn = Just $ Pawn (next toPlay)
        jumpedHereJustNow = Set.fromList di == Set.fromList [(hash fromSquare, opPawn), (hash captureSquare, Nothing)]
     in not (null gamehistory) && rightRow && jumpedHereJustNow

-- knights
toSquaresKnight :: Square -> [Square]
toSquaresKnight s =
    filter
        insideBoard
        [ squareTo s (-1) 2
        , squareTo s (-1) (-2)
        , squareTo s 1 2
        , squareTo s 1 (-2)
        , squareTo s 2 1
        , squareTo s 2 (-1)
        , squareTo s (-2) 1
        , squareTo s (-2) (-1)
        ]

-- bishops
toSquaresBishop :: Square -> [Square]
toSquaresBishop s@(Square c r) =
    let maxDown = r - 1
        maxUp = 8 - r
        maxLeft = c - 1
        maxRight = 8 - c
        a' = fmap (\x -> squareTo s x x) [1 .. min maxUp maxRight]
        b' = fmap (\x -> squareTo s x (-x)) [1 .. min maxDown maxRight]
        c' = fmap (\x -> squareTo s (-x) (-x)) [1 .. min maxDown maxLeft]
        d' = fmap (\x -> squareTo s (-x) x) [1 .. min maxLeft maxUp]
     in a' <> b' <> c' <> d'

-- rooks
toSquaresRook :: Square -> [Square]
toSquaresRook s@(Square c r) =
    let maxDown = 1 - r
        maxUp = 8 - r
        maxLeft = 1 - c
        maxRight = 8 - c
        lane = fmap (squareTo s 0) [maxDown .. maxUp]
        row = fmap (\c' -> squareTo s c' 0) [maxLeft .. maxRight]
     in lane <> row

-- queens
toSquaresQueen :: Square -> [Square]
toSquaresQueen s = toSquaresBishop s <> toSquaresRook s

-- kings
toSquaresKing :: Square -> [Square]
toSquaresKing s =
    [ squareTo s a b
    | a <- [-1, 0, 1]
    , b <- [-1, 0, 1]
    , (a, b) /= (0, 0)
    , insideBoard $ squareTo s a b
    ]

-- flips! todo this is in context of positionTree.. not after a Move
castle :: Position -> [Position]
castle pos =
    let relevantStatus = case toPlay pos of
            White -> castleStatusWhite pos
            _ -> castleStatusBlack pos
     in case relevantStatus of
            CanCastleNone -> []
            CanCastleBoth -> castle' CastleShort pos <> castle' CastleLong pos
            CanCastleA -> castle' CastleLong pos
            CanCastleH -> castle' CastleShort pos

castle' :: Move -> Position -> [Position]
castle' move pos =
    let color = toPlay pos
        kingSquare = kingPos color
        rookSquare = case move of
            CastleShort -> shortRookPos color
            _ -> longRookPos color -- todo illegal state if not castle move
        hasKingAtHome = pieceAt pos kingSquare == Just (King color)
        hasRookAtHome = pieceAt pos rookSquare == Just (Rook color)
        vacantBetweenKingAndRook = vacantBetween pos kingSquare rookSquare
        isNotInCheck = not (isInCheck pos)
        wontPassCheck = willNotPassCheck pos kingSquare rookSquare
        newSnapshot = case move of
            CastleShort -> doCastleShort (m pos) color
            _ -> doCastleLong (m pos) color -- todo illegal state if not castle move
        newCastleStatusWhite = if color == White then CanCastleNone else castleStatusWhite pos
        newCastleStatusBlack = if color == Black then CanCastleNone else castleStatusBlack pos
     in ( [ mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack
          | hasKingAtHome && hasRookAtHome && vacantBetweenKingAndRook && isNotInCheck && wontPassCheck -- todo error state ?
          ]
        )

doCastleShort :: Snapshot -> Color -> Snapshot
doCastleShort pos c =
    replacePieceAt
        ( replacePieceAt
            (removePieceAt (removePieceAt pos (kingPos c)) (shortRookPos c))
            (Square 7 (homeRow c))
            (King c)
        )
        (Square 6 (homeRow c))
        (Rook c)

doCastleLong :: Snapshot -> Color -> Snapshot
doCastleLong pos c =
    replacePieceAt
        ( replacePieceAt
            (removePieceAt (removePieceAt pos (kingPos c)) (longRookPos c))
            (Square 3 (homeRow c))
            (King c)
        )
        (Square 4 (homeRow c))
        (Rook c)

vacantBetween :: Position -> Square -> Square -> Bool
vacantBetween pos from to = all (vacantAt pos) $ points from to

kingPos :: Color -> Square
kingPos c = Square 5 (homeRow c)

shortRookPos :: Color -> Square
shortRookPos c = Square 8 (homeRow c)

longRookPos :: Color -> Square
longRookPos c = Square 1 (homeRow c)

homeRow :: Color -> Int
homeRow White = 1
homeRow Black = 8

willNotPassCheck :: Position -> Square -> Square -> Bool
willNotPassCheck pos kingPos' rookPos =
    let king = King $ toPlay pos
        squares = if col rookPos == 8 then points kingPos' rookPos else points kingPos' (rookPos {col = 7})
        asPos square = pos {m = replacePieceAt (removePieceAt (m pos) kingPos') square king}
     in not $ any isInCheck $ asPos <$> squares

insideBoard :: Square -> Bool
insideBoard (Square c r) = c >= 1 && c <= 8 && r >= 1 && r <= 8

insideBoard' :: (Square, Maybe Square) -> Bool
insideBoard' (s, Nothing) = insideBoard s
insideBoard' (s, Just s2) = insideBoard s && insideBoard s2

isInCheck :: Position -> Bool
isInCheck pos =
    case toPlay pos of
        White ->
            let whiteKingPos = searchForPieces pos (const True) (== King White)
             in case whiteKingPos of
                    [(kingSquare@(Square c r), _)] ->
                        let cangothere = canGoThere pos kingSquare
                            checkByPawn = c > 1 && r < 7 && pieceAt pos (squareTo kingSquare (-1) 1) == Just (Pawn Black) || c < 8 && r < 7 && pieceAt pos (squareTo kingSquare 1 1) == Just (Pawn Black)
                            checkByKnight = any ((Just (Knight Black) ==) . pieceAt pos) $ filter cangothere (toSquaresKnight kingSquare)
                            checkByRookQueen = any ((`elem` [Just (Rook Black), Just (Queen Black)]) . pieceAt pos) $ filter cangothere (toSquaresRook kingSquare)
                            checkByBishopQueen = any ((`elem` [Just (Bishop Black), Just (Queen Black)]) . pieceAt pos) $ filter cangothere (toSquaresBishop kingSquare)
                            checkByKing = any ((Just (King Black) ==) . pieceAt pos) $ filter cangothere (toSquaresKing kingSquare) -- todo move this check to canGoThere.. case I am a King piece -> check if causes check, else ..
                         in checkByPawn || checkByKnight || checkByRookQueen || checkByBishopQueen || checkByKing
                    _ -> False
        Black ->
            let blackKingPos = searchForPieces pos (const True) (== King Black)
             in case blackKingPos of
                    [(kingSquare@(Square c r), _)] ->
                        let cangothere = canGoThere pos kingSquare
                            checkByPawn = c > 1 && r > 2 && pieceAt pos (squareTo kingSquare (-1) (-1)) == Just (Pawn White) || c < 8 && r > 2 && pieceAt pos (squareTo kingSquare 1 (-1)) == Just (Pawn White)
                            checkByKnight = any ((Just (Knight White) ==) . pieceAt pos) $ toSquaresKnight kingSquare
                            checkByRookQueen = any ((`elem` [Just (Rook White), Just (Queen White)]) . pieceAt pos) $ filter cangothere (toSquaresRook kingSquare)
                            checkByBishopQueen = any ((`elem` [Just (Bishop White), Just (Queen White)]) . pieceAt pos) $ filter cangothere (toSquaresBishop kingSquare)
                            checkByKing = any ((Just (King White) ==) . pieceAt pos) $ filter cangothere (toSquaresKing kingSquare)
                         in checkByPawn
                                || checkByKnight
                                || checkByRookQueen
                                || checkByBishopQueen
                                || checkByKing
                    _ -> False

isCheckMate :: Position -> [Position] -> Bool
isCheckMate pos positiontree = null positiontree && isInCheck pos

isDraw :: Position -> [Position] -> Bool
isDraw pos ptree = threefoldrepetition pos || isPatt pos ptree

threefoldrepetition :: Position -> Bool
threefoldrepetition Position {..} =
    let occurrences = length $ filter (== m) gamehistory
     in occurrences > 2

eqPosition :: Position -> Position -> Bool
eqPosition (Position m1 _ _ _ _) (Position m2 _ _ _ _) = m1 == m2

isPatt :: Position -> [Position] -> Bool
isPatt pos positiontree = null positiontree && not (isInCheck pos)

determineStatus :: Position -> Status
determineStatus pos =
    let ptree = positionTree pos
        isMate = isCheckMate pos ptree
     in if toPlay pos == White && isMate
            then WhiteIsMate
            else
                if isMate
                    then BlackIsMate
                    else
                        if isDraw pos ptree
                            then Remis
                            else
                                if toPlay pos == White
                                    then WhiteToPlay
                                    else BlackToPlay

-- par/pseq fmap
paraMap :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
paraMap _ [] = []
paraMap f (x : xs) = do
    let a = force $ f x
        as = paraMap f xs
    a `par` as `pseq` a : as

(<-$->) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
(<-$->) = paraMap
infixl 4 <-$->

(<-&->) :: (NFData a, NFData b) => [a] -> (a -> b) -> [b]
(<-&->) = flip paraMap
infixr 4 <-&->
