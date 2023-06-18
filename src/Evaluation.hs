module Evaluation (
    Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    terminal,
    deepEval,
    alfaBeta
) where

import Chess (
    Status (..),
    determineStatus,
    positionTree,
    threefoldrepetition,
    toSquaresBishop',
    toSquaresKnight',
    toSquaresQueen',
    toSquaresRook',
    (<-$->),
 )
import Position

import Board (searchIdx)
import Control.Parallel (par)
import Data.List (maximum, minimum)
import GHC.Conc (pseq)
import Relude
import Relude.Extra (Foldable1 (..))

data Evaluated = Evaluated
    { pos :: Position
    , score :: Float
    , status :: Status
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

getPosition :: Evaluated -> Position
getPosition (Evaluated p _ _) = p

alfaBeta :: Int -> (Float, Float) -> Color -> Position -> Float
alfaBeta depth (alfa, beta) perspective pos =
    let candidates = positionTree pos
        status = determineStatus pos candidates
        evaluated = evaluate . m <-$-> candidates
        (newAlfa, newBeta) = case nonEmpty evaluated of
            Nothing -> (alfa, beta)
            Just ne -> (minimum1 ne, maximum1 ne)
     in if terminal status -- tie this with nonEmpty evaluated case, should be forced to correspond
            then score $ evaluate' pos
            else if perspective == White && newBeta <= alfa then newBeta
            else if perspective == Black && newAlfa >= beta then newAlfa
            else
                fromMaybe (error "Not terminal status, so there should be candidates")
                    $ singleBest'
                        perspective
                    $ if depth == 0 then evaluated
                    else alfaBeta (depth - 1) (newAlfa, newBeta) (next perspective) <$> candidates

deepEval :: Int -> Color -> Position -> Float
deepEval depth perspective pos =
    let candidates = positionTree pos
        status = determineStatus pos candidates
        evaluated = evaluate . m <-$-> candidates
     in if terminal status
            then score $ evaluate' pos
            else
                fromMaybe (error "Not terminal status, so there should be candidates")
                    $ singleBest'
                        perspective
                    $ if depth == 0
                        then evaluated
                        else deepEval (depth - 1) (next perspective) <$> candidates

terminal :: Status -> Bool
terminal = flip elem [WhiteIsMate, BlackIsMate, Remis, WhiteResigns, BlackResigns]

singleBest' :: Color -> [Float] -> Maybe Float
singleBest' _ [] = Nothing
singleBest' White (f : fs) = Just $ maximum (f : fs)
singleBest' Black (f : fs) = Just $ minimum (f : fs)

evaluate' :: Position -> Evaluated
evaluate' pos =
    case determineStatus pos (positionTree pos) of
        WhiteIsMate -> Evaluated pos (-10000.0) WhiteIsMate
        BlackIsMate -> Evaluated pos 10000.0 BlackIsMate
        Remis -> Evaluated pos 0 Remis
        playOn ->
            if threefoldrepetition pos
                then Evaluated pos 0.0 Remis
                else Evaluated pos (evaluate (m pos)) playOn

-- ideas:
-- trade when leading
_evaluate :: Snapshot -> Float
_evaluate snp =
    foldr
        ( \(w, mP) acc ->
            case mP of
                Nothing -> acc
                Just pie ->
                    let pieceVal = force $ valueOf pie
                        impactVal = force $ impactArea snp pie (unHash w)
                     in pieceVal `par` impactVal `pseq` acc + pieceVal + impactVal
        )
        (bishopPair snp)
        (toList' snp)

-- much faster evaluate function
evaluate :: Snapshot -> Float
evaluate snp =
    sum $
        fmap
            (maybe 0 valueOf)
            snp

valueOf :: Piece -> Float
valueOf (Pawn c) = colorFactor c * 1.0
valueOf (Knight c) = colorFactor c * 3.0
valueOf (Bishop c) = colorFactor c * 3.0
valueOf (Rook c) = colorFactor c * 5.0
valueOf (Queen c) = colorFactor c * 9.0
valueOf (King c) = colorFactor c * 100.0

colorFactor :: Color -> Float
colorFactor c = if c == Black then (-1) else 1

impactArea :: Snapshot -> Piece -> Square -> Float
impactArea _ (Pawn White) (Square _ r) = 0.005 * fromIntegral r
impactArea _ (Pawn Black) (Square _ r) = (-0.005) * fromIntegral (9 - r)
impactArea _ (King _) _ = 0
impactArea snp (Knight c) s =
    let reachables = fromIntegral . length $ toSquaresKnight' snp c s
     in colorFactor c * reachables * 0.01
impactArea snp (Bishop c) s =
    let reachables = fromIntegral . length $ toSquaresBishop' snp c s
     in colorFactor c * reachables * 0.01
impactArea snp (Rook c) s =
    let reachables = fromIntegral . length $ toSquaresRook' snp c s
     in colorFactor c * reachables * 0.01
impactArea snp (Queen c) s =
    let reachables = fromIntegral . length $ toSquaresQueen' snp c s
     in colorFactor c * reachables * 0.01

bishopPair :: Snapshot -> Float
bishopPair snp =
    let whiteBishops = searchIdx snp (const True) (== Just (Bishop White))
        blackBishops = searchIdx snp (const True) (== Just (Bishop Black))
     in (if length whiteBishops == 2 then 0.25 else 0.00) - (if length blackBishops == 2 then 0.25 else 0.00)