module Test.Day7 (test) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text qualified as TIO
import Day7
import Test.Hspec
  ( Expectation,
    SpecWith (..),
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
    shouldMatchList,
    shouldNotBe,
    shouldSatisfy,
    xdescribe,
    xit,
  )
import Test.QuickCheck

newtype Range = Range
  { unRange :: (Int, Int)
  }
  deriving (Show)

test :: SpecWith ()
test =
  describe "Day7" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln "test/Test/Day7/short.txt"
        res `shouldBe` 3263827

      it "full-input.txt" $ do
        res <- soln "test/Test/Day7/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 9194682052782


