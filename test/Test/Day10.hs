module Test.Day10 (test) where

import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Day10
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
shortInput = "test/Test/Day10/short.txt"

test :: SpecWith ()
test =
  describe "Day10" $ do
    describe "soln" $ do
      xit "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 7

      xit "full-input.txt" $ do
        res <- soln "test/Test/Day10/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 0

    describe "machToZ3" $ do 
      it "gens first" $ do 
        let mach = P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
            z3_script = machToZ3 mach
        expected_script <- TIO.readFile "test/Test/Day10/solve_first_short.z3"
        z3_script `shouldBe` expected_script

    describe "evalMachine" $ do 
      it "evals first press soln" $ do 
        let res = evalMachine (P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {0,0,0,1}")
        res `shouldBe` 1

      xit "evals first" $ do 
        let res = evalMachine (P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
        res `shouldBe` 10

    describe "pressButton" $ do
      it "simple press" $ do
        pressButton0 [0, 1, 2, 3] (S.singleton 3) `shouldBe` [0, 1, 2, 4]

      it "first three" $ do
        -- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1)
        let result =
              foldl'
                pressButton0
                [0, 0, 0, 0]
                [ S.singleton 3,
                  S.fromList [1, 3],
                  S.singleton 2
                ]
        result `shouldBe` [0, 1, 1, 2]

    describe "pMachine" $ do
      it "parses first" $ do
        let res = P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
        res
          `shouldBe` Mach
            (S.fromList [1, 2])
            [ S.singleton 3,
              S.fromList [1, 3],
              S.singleton 2,
              S.fromList [2, 3],
              S.fromList [0, 2],
              S.fromList [0, 1]
            ] 
            [3, 5, 4, 7]
