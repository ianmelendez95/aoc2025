module Test.Day5 (test) where

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    xit,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
    xdescribe,
    expectationFailure,
    shouldMatchList
  )
import Day5
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.Maybe (fromJust)

test :: SpecWith ()
test = 
  describe "Day5" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "test/Test/Day5/short.txt"
        res `shouldBe` 3

      it "full-input.txt" $ do 
        res <- soln "test/Test/Day5/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 172


