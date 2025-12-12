module Test.Day11 (test) where

import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text qualified as TIO
import Day11
import Parse qualified as P
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

shortInput :: FilePath
shortInput = "test/Test/Day11/short.txt"

test :: SpecWith ()
test =
  describe "Day11" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 5

      it "full-input.txt" $ do
        res <- soln "test/Test/Day11/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0

