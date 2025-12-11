module Test.Day10 (test) where

import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.List
import Data.Ord
import Day10
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
shortInput = "test/Test/Day10/short.txt"

test :: SpecWith ()
test =
  describe "Day10" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 50

      xit "full-input.txt" $ do
        res <- soln "test/Test/Day10/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0
