module AISpec where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Data.Typeable

import           AI
import           Chess
import           Evaluation
import           Move
import           Printer

spec :: Spec
spec = describe "AI" $ do
  it "evaluates the start position as reasonably balanced" $ do
    let e = Evaluation.evaluate Chess.startPosition
    e `shouldSatisfy` (> (-1.1))
    e `shouldSatisfy` (< 1.1)
  it "evaluate advanced pawns over home pawns" $ do
    let p = Chess.makeMoves Chess.startPosition [((Square 5 2), (Square 5 4))]
        e = Evaluation.evaluate p
    e `shouldSatisfy` (> 0.0)
  it "expands the horizon of start position" $ do
    length (expandHorizon 1 Chess.startPosition) `shouldBe` 20
    let two = expandHorizon 2 Chess.startPosition
        three = expandHorizon 3 Chess.startPosition
    length two `shouldBe` 400
    length three > length two `shouldBe` True
  it "successfully steps one point in a direction with oneStep with 1 look a head" $ do
    let p1 = Chess.startPosition
    let p2 = head $ positionTree p1
    oneStep p1 p2 `shouldBe` p2
  it "successfully steps one point in a direction with oneStep based on 2 looks ahead" $ do
    let p1 = Chess.startPosition
    let p2 = head $ positionTree p1
    let p3 = head $ positionTree p2
    oneStep p1 p3 `shouldBe` p2
