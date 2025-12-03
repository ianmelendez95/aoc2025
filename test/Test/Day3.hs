module Test.Day3 (testDay3) where

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    xit,
    shouldBe,
    shouldSatisfy,
    xdescribe,
    expectationFailure
  )
import Day3
import Data.Text qualified as T
import Data.Text qualified as TIO

testDay3 :: SpecWith ()
testDay3 = 
  describe "Day3" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "test/Test/Day3/short.txt"
        res `shouldBe` 0

      xit "full-input.txt" $ do 
        res <- soln "test/Test/Day3/full.txt"
        -- putStrLn $ "Solution: " ++ show res
        res `shouldBe` 50793864718

