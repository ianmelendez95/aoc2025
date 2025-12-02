module Main (main) where

import Day0 (moveDial', doMoves, doMoves', doMovesState, doMovesState') 
import Test.Day0 (testDay0)
import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
    xdescribe,
  )
 

main :: IO ()
main = hspec $ do
  testDay0
      


