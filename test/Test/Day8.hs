module Test.Day8 (test) where

import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
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

    describe "coordDist0" $ do
      it "simple" $ do
        -- 162,817,812
        -- 425,690,689
        let v1@(Coord x y z) = Coord 162 817 812
            v2@(Coord x' y' z') = Coord 425 690 689
        coordDist0 v1 v2
          `shouldBe` sqrt
            ( sum
                [ (fromIntegral x - fromIntegral x') ** 2,
                  (fromIntegral y - fromIntegral y') ** 2,
                  (fromIntegral z - fromIntegral z') ** 2
                ]
            )
