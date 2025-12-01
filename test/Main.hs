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
    it "moveDial - 50 + 5" $ do 
      let res = moveDial' 50 5
      res `shouldBe` (55, 0)
    it "moveDial - 50 + 55" $ do 
      let res = moveDial' 50 55
      res `shouldBe` (5, 1)
    it "moveDial - 50 + 200" $ do 
      let res = moveDial' 50 200
      res `shouldBe` (50, 2)

