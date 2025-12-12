module Test.Day12 (test) where

import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Day12
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
shortInput = "test/Test/Day12/short.txt"

test :: SpecWith ()
test =
  describe "Day12" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 5

      -- xit "full-input.txt" $ do
      --   res <- soln "svr" ["dac", "fft"] "test/Test/Day12/full.txt"
      --   putStrLn $ "Solution: " ++ show res
      --   res `shouldNotBe` 0

    describe "pShape" $ do 
      it "parses first" $ do 
        content <- TIO.readFile shortInput
        P.parse pShape content `shouldBe` 7

