{-# LANGUAGE DeriveGeneric, BangPatterns #-}

module Chess where

import Board (diff, searchIdx)
import Control.DeepSeq (force)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (NFData)
import Data.List
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import Position
import Prelude hiding (foldr)

data Status
    = WhiteToPlay
    | BlackToPlay
    | Remis
    | WhiteIsMate
    | BlackIsMate
    | BlackResigns
    | WhiteResigns
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
positionTree = caf
    where caf pos = filter (notSelfcheck (toPlay pos)) $ positionTreeIgnoreCheck pos

notSelfcheck :: Color -> Position -> Bool
notSelfcheck col pos = not $ isInCheck (m pos) col

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
        let squares = toSquaresPawn pos s
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
        fmap (movePiece pos s) (toSquaresKnight' (m pos) (toPlay pos) s)
    Bishop _ ->
        fmap (movePiece pos s) (toSquaresBishop' (m pos) (toPlay pos) s)
    Rook _ ->
        fmap (movePiece pos s) (toSquaresRook' (m pos) (toPlay pos) s)
    Queen _ ->
        fmap (movePiece pos s) (toSquaresQueen' (m pos) (toPlay pos) s)
    King _ ->
        fmap (movePiece pos s) (toSquaresKing' (m pos) (toPlay pos) s)

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: Position -> Square -> [(Square, Maybe Square)]
toSquaresPawn pos s@(Square c r)
    | toPlay pos == White =
        [(squareTo s 0 2, Nothing) | r == 2, vacantAt pos (Square c (r + 1)), vacantAt pos (Square c (r + 2))]
            <> [(squareTo s 0 1, Nothing) | vacantAt pos (Square c (r + 1))]
            <> [(squareTo s (-1) 1, Nothing) | c > 1, enemyAt pos $ squareTo s (-1) 1]
            <> [(squareTo s 1 1, Nothing) | c < 8, enemyAt pos $ squareTo s 1 1]
            <> [(squareTo s (-1) 1, Just (squareTo s (-1) 0)) | c > 1, enPassant pos (squareTo s (-1) 0)]
            <> [(squareTo s 1 1, Just (squareTo s 1 0)) | c < 8, enPassant pos (squareTo s 1 0)]
    | otherwise =
        [(squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos (Square c (r - 1)), vacantAt pos (Square c (r - 2))]
            <> [(squareTo s 0 (-1), Nothing) | vacantAt pos (Square c (r - 1))]
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
toSquaresKnight' :: Snapshot -> Color -> Square -> [Square]
toSquaresKnight' snp myCol (Square c r) =
    let okSquare zq =
            insideBoard zq && case pieceAt' snp zq of
                Nothing -> True
                Just pie -> colr pie == next myCol
     in [ s
        | s <-
            [ Square (c - 1) (r - 2)
            , Square (c - 1) (r + 2)
            , Square (c - 2) (r - 1)
            , Square (c - 2) (r + 1)
            , Square (c + 1) (r - 2)
            , Square (c + 1) (r + 2)
            , Square (c + 2) (r - 1)
            , Square (c + 2) (r + 1)
            ]
        , okSquare s
        ]

toSquaresRook' :: Snapshot -> Color -> Square -> [Square]
toSquaresRook' snp myCol (Square c r) =
    let up = digger id succ [] snp (Square c (succ r)) myCol
        right = digger succ id [] snp (Square (succ c) r) myCol
        down = digger id pred [] snp (Square c (pred r)) myCol
        left = digger pred id [] snp (Square (pred c) r) myCol
     in up <> right <> down <> left

toSquaresBishop' :: Snapshot -> Color -> Square -> [Square]
toSquaresBishop' snp myCol (Square c r) =
    let ne = digger succ succ [] snp (Square (succ c) (succ r)) myCol
        se = digger succ pred [] snp (Square (succ c) (pred r)) myCol
        sw = digger pred pred [] snp (Square (pred c) (pred r)) myCol
        nw = digger pred succ [] snp (Square (pred c) (succ r)) myCol
     in ne <> se <> sw <> nw

toSquaresQueen' :: Snapshot -> Color -> Square -> [Square]
toSquaresQueen' snp myCol s = toSquaresRook' snp myCol s <> toSquaresBishop' snp myCol s

toSquaresKing' :: Snapshot -> Color -> Square -> [Square]
toSquaresKing' snp myCol (Square c r) =
    let okSquare zq = case pieceAt' snp zq of
            Nothing -> True
            Just pie -> colr pie == next myCol
     in [ Square c' r'
        | c' <- [pred c, c, succ c]
        , r' <- [pred r, r, succ r]
        , insideBoard (Square c' r')
        , (c', r') /= (c, r)
        , okSquare (Square c' r')
        ]

digger :: (Int -> Int) -> (Int -> Int) -> [Square] -> Snapshot -> Square -> Color -> [Square]
digger nextCol nextRow !acc snp (Square c r) color
    | insideBoard (Square c r) =
        acc <> case pieceAt' snp (Square c r) of
            Nothing -> Square c r : digger nextCol nextRow acc snp (Square (nextCol c) (nextRow r)) color
            Just p -> [Square c r | colr p == next color]
    | otherwise = acc

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
        relevantSquares = case (color, move) of
            (White, CastleShort) -> [Square 6 1, Square 7 1]
            (White, CastleLong) -> [Square 2 1, Square 3 1, Square 4 1]
            (Black, CastleShort) -> [Square 6 8, Square 7 8]
            (Black, CastleLong) -> [Square 2 8, Square 3 8, Square 4 8]
            _ -> []
        vacantBetweenKingAndRook = all (vacantAt pos) relevantSquares
        fakesnp s = removePieceAt (replacePieceAt (m pos) s (King color)) kingSquare
        isNotInCheck = not (isInCheck (m pos) color)
        wontPassCheck = all (\snp' -> not $ isInCheck snp' color) (fakesnp <$> relevantSquares)
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

kingPos :: Color -> Square
kingPos c = Square 5 (homeRow c)

shortRookPos :: Color -> Square
shortRookPos c = Square 8 (homeRow c)

longRookPos :: Color -> Square
longRookPos c = Square 1 (homeRow c)

homeRow :: Color -> Int
homeRow White = 1
homeRow Black = 8

insideBoard :: Square -> Bool
insideBoard (Square c r) = c >= 1 && c <= 8 && r >= 1 && r <= 8

insideBoard' :: (Square, Maybe Square) -> Bool
insideBoard' (s, Nothing) = insideBoard s
insideBoard' (s, Just s2) = insideBoard s && insideBoard s2

isInCheck :: Snapshot -> Color -> Bool
isInCheck snp myColr =
    let kinPo = searchIdx snp (const True) (\mP -> mP == Just (King myColr))
        hashed (w8, x) = (unHash w8, x)
        opColr = next myColr
        dangerSquares (Square c r) = filter insideBoard case myColr of
            White -> [Square (c - 1) (r + 1), Square (c + 1) (r + 1)]
            Black -> [Square (c - 1) (r - 1), Square (c + 1) (r - 1)]
     in case hashed <$> kinPo of
            [(kingSquare, _)] ->
                let checkByPawn = elem (Just (Pawn opColr)) (pieceAt' snp <$> dangerSquares kingSquare)
                    checkByKnight = any ((Just (Knight opColr) ==) . pieceAt' snp) $ toSquaresKnight' snp myColr kingSquare
                    checkByRookQueen = any ((`elem` [Just (Rook opColr), Just (Queen opColr)]) . pieceAt' snp) $ toSquaresRook' snp myColr kingSquare
                    checkByBishopQueen = any ((`elem` [Just (Bishop opColr), Just (Queen opColr)]) . pieceAt' snp) $ toSquaresBishop' snp myColr kingSquare
                    checkByKing = any ((Just (King opColr) ==) . pieceAt' snp) $ toSquaresKing' snp myColr kingSquare
                 in checkByPawn || checkByKnight || checkByRookQueen || checkByBishopQueen || checkByKing
            _ -> False

isCheckMate :: Position -> [Position] -> Bool
isCheckMate pos positiontree = null positiontree && isInCheck (m pos) (toPlay pos)

isDraw :: Position -> [Position] -> Bool
isDraw pos ptree = threefoldrepetition pos || isPatt pos ptree

threefoldrepetition :: Position -> Bool
threefoldrepetition Position {..} =
    let occurrences = length $ filter (== m) gamehistory
     in occurrences > 2

eqPosition :: Position -> Position -> Bool
eqPosition (Position m1 _ _ _ _) (Position m2 _ _ _ _) = m1 == m2

isPatt :: Position -> [Position] -> Bool
isPatt pos positiontree = null positiontree && not (isInCheck (m pos) (toPlay pos))

determineStatus :: Position -> [Position] -> Status
determineStatus pos ptree=
    let isMate = isCheckMate pos ptree
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
