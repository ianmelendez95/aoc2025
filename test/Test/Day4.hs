module Test.Day4 (test) where

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
    expectationFailure,
    shouldMatchList
  )
import Day4
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.Maybe (fromJust)

test :: SpecWith ()
test = 
  describe "Day4" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "test/Test/Day4/short.txt"
        res `shouldBe` 0

      xit "full-input.txt" $ do 
        res <- soln "test/Test/Day4/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldBe` 17332



