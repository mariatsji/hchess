{-# LANGUAGE DeriveGeneric #-}

module Chess where

import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import Position
import Prelude hiding (foldr)

data Status
    = WhiteToPlay
    | BlackToPlay
    | Remis
    | WhiteIsMate
    | BlackIsMate
    deriving (Eq, Ord, Show, Generic)

board :: [Square]
board = Square <$> [1 .. 8] <*> [1 .. 8]

squareTo :: Square -> Int -> Int -> Square
squareTo (Square c r) cols rows = Square (c + cols) (r + rows)

-- flips toPlay
movePiece :: Position -> Square -> Square -> Position
movePiece pos@(Position m' _ _ _ wk bk tp) from@(Square fc _) to@(Square tc tr)
    | pieceAt pos from == Just (Pawn White) && (fc /= tc) && vacantAt pos to =
        let newSnapshot =
                movePiece' (removePieceAt m' (Square tc (tr - 1))) from to
         in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos) wk bk
    | pieceAt pos from == Just (Pawn Black) && (fc /= tc) && vacantAt pos to =
        let newSnapshot =
                movePiece' (removePieceAt m' (Square tc (tr + 1))) from to
         in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos) wk bk
    | otherwise =
        let newSnapshot = movePiece' m' from to
            newCastleStatusWhite = case tp of
                White -> mkNewCastleStatus pos White from
                Black -> castleStatusWhite pos
            newCastleStatusBlack = case tp of
                Black -> mkNewCastleStatus pos Black from
                White -> castleStatusBlack pos
            newWhiteKing
                | pieceAt pos to == Just (King White) = Nothing
                | pieceAt pos from == Just (King White) = Just to
                | otherwise = wk
            newBlackKing
                | pieceAt pos to == Just (King Black) = Nothing
                | pieceAt pos from == Just (King Black) = Just to
                | otherwise = bk
         in mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack newWhiteKing newBlackKing

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

canGoThere :: Position -> Square -> Square -> Bool
canGoThere pos from to =
    all isNothing (fmap (pieceAt pos) (points from to))
        && finalDestinationNotOccupiedBySelf pos to

finalDestinationNotOccupiedBySelf :: Position -> Square -> Bool
finalDestinationNotOccupiedBySelf pos to =
    null $ searchForPieces pos (== to) ((== toPlay pos) . colr)

enemyAt :: Position -> Square -> Bool
enemyAt pos to =
    let enemyColor = next (toPlay pos)
     in (colr <$> pieceAt pos to) == Just enemyColor

succ' :: Color -> Color
succ' White = Black
succ' Black = White

vacantAt :: Position -> Square -> Bool
vacantAt pos t = isNothing $ pieceAt pos t

makeMoves :: Position -> [(Square, Square)] -> Position
makeMoves = foldl (uncurry . movePiece)

-- CAF now? would be nice
pieceAt :: Position -> Square -> Maybe Piece
pieceAt = pieceAt' . m

positionTree :: Position -> [Position]
positionTree pos = filter (\p -> not $ isInCheck pos) $ positionTreeIgnoreCheck pos

debugger :: IO ()
debugger = do
    let poses = positionTreeIgnoreCheck startPosition
    print $ toPlay <$> poses

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
positionsPrPiece pos@(Position snp _ _ _ _ _ _) (s, p) = case p of
    Pawn _ ->
        let squares = filter (canGoThere pos s . fst) $ toSquaresPawn pos s
            positions =
                ( \t ->
                    let fromSquare = s
                        toSquare = fst t
                        promRow = if toPlay pos == White then 8 else 1
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
                    =<< squares
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

row :: Square -> Int
row (Square _ r) = r

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: Position -> Square -> [(Square, Maybe Square)]
toSquaresPawn pos s@(Square _ r)
    | toPlay pos == White =
        filter insideBoard' $
            [(squareTo s 0 2, Nothing) | r == 2, vacantAt pos $ squareTo s 0 2]
                <> [(squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1]
                <> [(squareTo s (-1) 1, Nothing) | enemyAt pos $ squareTo s (-1) 1]
                <> [(squareTo s 1 1, Nothing) | enemyAt pos $ squareTo s 1 1]
                <> [(squareTo s (-1) 1, Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0)]
                <> [(squareTo s 1 1, Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0)]
    | otherwise =
        filter insideBoard' $
            [(squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos $ squareTo s 0 (-2)]
                <> [(squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1)]
                <> [(squareTo s (-1) (-1), Nothing) | enemyAt pos $ squareTo s (-1) (-1)]
                <> [(squareTo s 1 (-1), Nothing) | enemyAt pos $ squareTo s 1 (-1)]
                <> [(squareTo s (-1) (-1), Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0)]
                <> [(squareTo s 1 (-1), Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0)]

-- en passant
enPassant :: Position -> Square -> Bool
enPassant (Position _ [] _ _ _ _ _) _ = False
enPassant pos s@(Square c r)
    | toPlay pos == White =
        (r == 5) && pieceAt pos s == Just (Pawn Black) && jumpedHereJustNow pos s
    | otherwise =
        (r == 4) && pieceAt pos s == Just (Pawn White) && jumpedHereJustNow pos s
  where
    piece = Just $ Pawn (toPlay pos)
    fromSquare = if toPlay pos == White then Square c 7 else Square c 2
    jumpedHereJustNow :: Position -> Square -> Bool
    jumpedHereJustNow _ _ =
        not $
            length (gamehistory pos) < 3
                && let prevSnapshot = gamehistory pos !! 1
                    in pieceAt' prevSnapshot fromSquare == piece && pieceAt' (m pos) s == piece && isNothing (pieceAt' prevSnapshot s)

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

-- castles
type KingPos = Color -> Square

type RookPos = Color -> Square

-- flips!
castle :: Position -> [Position]
castle pos =
    let x = case toPlay pos of
            White -> castleStatusWhite pos
            _ -> castleStatusBlack pos
     in case x of
            CanCastleNone -> []
            CanCastleBoth -> castleShort pos <> castleLong pos
            CanCastleA -> castleLong pos
            CanCastleH -> castleShort pos

castleShort :: Position -> [Position]
castleShort pos = castle' pos kingPos shortRookPos doCastleShort

castleLong :: Position -> [Position]
castleLong pos = castle' pos kingPos longRookPos doCastleLong

castle' ::
    Position -> KingPos -> RookPos -> (Snapshot -> Color -> Snapshot) -> [Position]
castle' pos kingPosF rookPosF doCastleF =
    let color = toPlay pos
     in if pieceAt pos (kingPosF color) == Just (King color) -- must have a king at home
            && pieceAt pos (rookPosF color) -- must have a rook at home
                == Just (Rook color)
            && vacantBetween pos (kingPosF color) (rookPosF color) -- must be vacant between king and rook
            && ( not (isInCheck pos) -- must not be in check
                    && willNotPassCheck pos (kingPosF color) (rookPosF color) -- must not move through check
               )
            then
                let newSnapshot = doCastleF (m pos) color
                    newCastleStatusWhite = if color == White then CanCastleNone else castleStatusWhite pos
                    newCastleStatusBlack = if color == Black then CanCastleNone else castleStatusBlack pos
                    newKingPosWhite = case rookPosF color of
                        Square 1 1 -> Just $ Square 3 1
                        Square 8 1 -> Just $ Square 7 1
                        _ -> whiteKing pos
                    newKingPosBlack = case rookPosF color of
                        Square 1 8 -> Just $ Square 3 8
                        Square 8 8 -> Just $ Square 7 8
                        _ -> blackKing pos
                 in [mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack newKingPosWhite newKingPosBlack]
            else []

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

promoteRow :: Color -> Int
promoteRow White = homeRow Black
promoteRow Black = homeRow White

willNotPassCheck :: Position -> Square -> Square -> Bool
willNotPassCheck pos (Square 5 1) (Square 8 1) =
    not $
        any
            isInCheck
            [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 6 1) (King (toPlay pos)), whiteKing = Just (Square 6 1)}
            , pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 7 1) (King (toPlay pos)), whiteKing = Just (Square 7 1)}
            ]
willNotPassCheck pos (Square 5 1) (Square 1 1) =
    not $
        any
            isInCheck
            [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 4 1) (King (toPlay pos)), whiteKing = Just (Square 4 1)}
            , pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 3 1) (King (toPlay pos)), whiteKing = Just (Square 3 1)}
            , pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 2 1) (King (toPlay pos)), whiteKing = Just (Square 2 1)}
            ]
willNotPassCheck pos (Square 5 8) (Square 8 8) =
    not $
        any
            isInCheck
            [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 6 8) (King (toPlay pos)), blackKing = Just (Square 6 8)}
            , pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 7 8) (King (toPlay pos)), blackKing = Just (Square 7 8)}
            ]
willNotPassCheck pos (Square 5 8) (Square 1 8) =
    not $
        any
            isInCheck
            [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 4 8) (King (toPlay pos)), blackKing = Just (Square 4 8)}
            , pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 3 8) (King (toPlay pos)), blackKing = Just (Square 3 8)}
            , pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 2 8) (King (toPlay pos)), blackKing = Just (Square 2 8)}
            ]
willNotPassCheck _ s1 s2 =
    error $
        "cannot use squares "
            <> show s1
            <> " and "
            <> show s2
            <> " as castling squares"

insideBoard :: Square -> Bool
insideBoard (Square c r) = c >= 1 && c <= 8 && r >= 1 && r <= 8

insideBoard' :: (Square, Maybe Square) -> Bool
insideBoard' (s, Nothing) = insideBoard s
insideBoard' (s, Just s2) = insideBoard s && insideBoard s2

isInCheck :: Position -> Bool
isInCheck pos =
    case toPlay pos of
        White ->
            case whiteKing pos of
                Nothing -> False
                Just kingSquare ->
                    let cangothere = canGoThere pos kingSquare
                        checkByPawn = pieceAt pos (squareTo kingSquare (-1) 1) == Just (Pawn Black) || pieceAt pos (squareTo kingSquare 1 1) == Just (Pawn Black)
                        checkByKnight = any ((Just (Knight Black) ==) . pieceAt pos) $ filter cangothere (toSquaresKnight kingSquare)
                        checkByRookQueen = any ((`elem` [Just (Rook Black), Just (Queen Black)]) . pieceAt pos) $ filter cangothere (toSquaresRook kingSquare)
                        checkByBishopQueen = any ((`elem` [Just (Bishop Black), Just (Queen Black)]) . pieceAt pos) $ filter cangothere (toSquaresBishop kingSquare)
                        checkByKing = any ((Just (King Black) ==) . pieceAt pos) $ filter cangothere (toSquaresKing kingSquare) -- todo move this check to canGoThere.. case I am a King piece -> check if causes check, else ..
                     in checkByPawn || checkByKnight || checkByRookQueen || checkByBishopQueen || checkByKing
        Black ->
            case blackKing pos of
                Nothing -> False
                Just kingSquare ->
                    let cangothere = canGoThere pos kingSquare
                        checkByPawn = pieceAt pos (squareTo kingSquare (-1) (-1)) == Just (Pawn White) || pieceAt pos (squareTo kingSquare 1 (-1)) == Just (Pawn White)
                        checkByKnight = any ((Just (Knight White) ==) . pieceAt pos) $ toSquaresKnight kingSquare
                        checkByRookQueen = any ((`elem` [Just (Rook White), Just (Queen White)]) . pieceAt pos) $ filter cangothere (toSquaresRook kingSquare)
                        checkByBishopQueen = any ((`elem` [Just (Bishop White), Just (Queen White)]) . pieceAt pos) $ filter cangothere (toSquaresBishop kingSquare)
                        checkByKing = any ((Just (King White) ==) . pieceAt pos) $ filter cangothere (toSquaresKing kingSquare)
                     in checkByPawn || checkByKnight || checkByRookQueen || checkByBishopQueen || checkByKing

isCheckMate :: Position -> [Position] -> Bool
isCheckMate pos positiontree = isInCheck pos && null positiontree

isDraw :: Position -> [Position] -> Bool
isDraw pos ptree = isPatt pos ptree || threefoldrepetition pos

threefoldrepetition :: Position -> Bool
threefoldrepetition (Position m' gh _ _ _ _ _) = length gh > 5 && length (filter (== m') gh) > 1

eqPosition :: Position -> Position -> Bool
eqPosition (Position m1 _ _ _ _ _ _) (Position m2 _ _ _ _ _ _) = m1 == m2

isPatt :: Position -> [Position] -> Bool
isPatt pos positiontree = not (isInCheck pos) && null positiontree

determineStatus :: Position -> Status
determineStatus pos =
    let ptree = positionTree pos
     in if toPlay pos == White && isCheckMate pos ptree
            then WhiteIsMate
            else
                if isCheckMate pos ptree
                    then BlackIsMate
                    else
                        if isDraw pos ptree
                            then Remis
                            else
                                if toPlay pos == White
                                    then WhiteToPlay
                                    else BlackToPlay