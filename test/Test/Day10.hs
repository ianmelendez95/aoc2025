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
import SBV

shortInput :: FilePath
shortInput = "test/Test/Day10/short.txt"

mach1 :: Mach
mach1 = P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"

test :: SpecWith ()
test =
  describe "Day10" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln shortInput
        res `shouldBe` 33

      it "full-input.txt" $ do
        res <- soln "test/Test/Day10/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 17558
    
    describe "solveMach" $ do 
      it "solves first" $ do 
        soln1 <- solveMach mach1
        soln1 `shouldBe` 10

    -- describe "machToZ3" $ do 
    --   it "gens first" $ do 
    --     let mach = P.parse pMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    --     opt_res <- optimizeLexicographic $ machToZ3 mach
    --     print opt_res
    --     True `shouldBe` True
        --     z3_script = machToZ3 mach
        -- expected_script <- TIO.readFile "test/Test/Day10/solve_first_short.z3"
        -- z3_script `shouldBe` expected_script

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
