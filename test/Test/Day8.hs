module Test.Day8 (test) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text qualified as TIO
import Day8
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
import Data.Set qualified as S
import Data.Map qualified as M

test :: SpecWith ()
test =
  describe "Day8" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln "test/Test/Day8/short.txt"
        res `shouldBe` 0

      xit "full-input.txt" $ do
        res <- soln "test/Test/Day8/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0


