module Main (main) where

import Day0 (moveDial') 
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
  describe "Day0" $ do 
    it "moveDial" $ do 
      let res = moveDial' 50 5
      res `shouldBe` (45, 0)

