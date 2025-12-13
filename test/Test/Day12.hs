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

fullInput :: FilePath
fullInput = "test/Test/Day12/full.txt"

test :: SpecWith ()
test =
  describe "Day12" $ do
    describe "soln" $ do
    --   it "short-input.txt" $ do
    --     res <- soln shortInput
    --     res `shouldBe` 5

      it "full-input.txt" $ do
        res <- soln fullInput
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0

    describe "pInput" $ do 
      it "parses short" $ do 
        content <- TIO.readFile shortInput
        let (shapes, regions) = P.parse pInput content
        putStrLn $ "REGIONS: " ++ show regions
        length shapes `shouldBe` 6
        length regions `shouldBe` 3

    describe "pRegion" $ do 
      it "parses first" $ do 
        let (Region a ss) = P.parse pRegion "48x42: 43 59 54 46 51 56"
        a `shouldBe` 48 * 42
        ss `shouldBe` [43, 59, 54, 46, 51, 56]

    describe "pShape" $ do 
      it "parses first" $ do 
        content <- TIO.readFile shortInput
        P.parse pShape content `shouldBe` 7

      it "parses many" $ do 
        content <- TIO.readFile shortInput
        P.parse (P.many (P.try pShape)) content `shouldBe` [7, 7, 7, 7, 7, 7]

      it "parses full" $ do 
        content <- TIO.readFile fullInput
        P.parse (P.many (P.try pShape)) content `shouldBe` [7, 7, 6, 5, 7, 7]


