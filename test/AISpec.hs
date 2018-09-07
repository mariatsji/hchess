import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Data.Typeable

import           AI
import           Chess
import           Evaluation
import           Move
import           Printer

main :: IO ()
main =
  hspec $
    describe "AI" $ do
  it "evaluates the start position as reasonably balanced" $ do
    let e = Evaluation.evaluate Chess.startPosition
    e `shouldSatisfy` (> (-1.1))
    e `shouldSatisfy` (< 1.1)
  it "evaluate advanced pawns over home pawns" $ do
    let p = Chess.makeMoves [Chess.startPosition] [((Square 5  2), (Square 5  4))]
    let e = Evaluation.evaluate $ head p
    e `shouldSatisfy` (> 0.0)
