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

shortInput :: FilePath
shortInput = "test/Test/Day8/short.txt"

test :: SpecWith ()
test =
  describe "Day8" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 0

      xit "full-input.txt" $ do
        res <- soln "test/Test/Day8/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0
    
    describe "ascPairCoords0" $ do 
      it "short" $ do 
        coords <- readCoordsFile shortInput
        let pairs = ascPairCoords0 coords
            p1 = (Coord 162 817 812, Coord 425 690 689)
            p2 = (Coord 162 817 812, Coord 431 825 988)
            p3 = (Coord 906 360 560, Coord 805 96  715)
        case pairs of 
          (p1' : p2' : p3' : _) -> do 
            p1' `shouldBe` p1
            p2' `shouldBe` p2
            p3' `shouldBe` p3
          _ -> error "ERR"

    describe "pairCoords0" $ do 
      it "two" $ do 
        -- 162,817,812
        -- 425,690,689
        let c1 = Coord 162 817 812
            c2 = Coord 425 690 689
        pairCoords0 [c1, c2] `shouldBe` [(coordDist0 c1 c2, (c1, c2))]

      it "three" $ do 
        let c1 = Coord 162 817 812
            c2 = Coord 425 690 689
            c3 = Coord 431 824 988 -- 431,825,988
            pairs = pairCoords0 [c1, c2, c3]
        -- mapM_ print pairs
        length pairs `shouldBe` 3

    describe "nearCoord0" $ do 
      it "first short.txt" $ do 
        coords <- readCoordsFile shortInput 
        case coords of 
          -- 425,690,689
          (c:cs) -> nearCoord0 c cs `shouldBe` Coord 425 690 689
          _ -> error $ "coords: " ++ show coords

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

    describe "againstEach" $ do 
      it "simple" $ do 
        againstEach (:) "hello" `shouldBe` ["hello", "ehllo", "lehlo", "lleho", "olleh"]
