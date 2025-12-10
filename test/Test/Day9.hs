module Test.Day9 (test) where

import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.List
import Data.Ord
import Day9
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
shortInput = "test/Test/Day9/short.txt"

test :: SpecWith ()
test =
  describe "Day9" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 50

      it "full-input.txt" $ do
        res <- soln "test/Test/Day9/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0

    describe "bigRects0" $ do 
      it "finds biggest" $ do 
        coords <- readCoordsFile shortInput
        let big_rects = bigRects0 (pairs0 $ sort coords)
        case big_rects of 
          [] -> error $ "no rects"
          (rect1:_) -> pairArea0 rect1 `shouldBe` 50
    
    describe "pairArea0" $ do 
      it "2,5 11,1" $ do 
        pairArea0 ((2,5), (11,1)) `shouldBe` 50
