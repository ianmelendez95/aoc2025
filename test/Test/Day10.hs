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
import Parse qualified as P

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
    
    describe "pressButton" $ do 
      it "simple press" $ do 
        pressButton0 (S.fromList [1, 2]) (S.singleton 3) `shouldBe` S.fromList [1, 2, 3]

    describe "pMachine" $ do 
      it "parses first" $ do 
        let res = P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
        res `shouldBe` Mach (S.fromList [1, 2]) [
            S.singleton 3, 
            S.fromList [1, 3],
            S.singleton 2,
            S.fromList [2, 3],
            S.fromList [0, 2],
            S.fromList [0, 1]
          ]
        
