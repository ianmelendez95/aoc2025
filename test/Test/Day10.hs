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
import Data.SBV

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

      it "solves sample 1" $ do 
        soln1 <- solveMach $ P.parse pMachine "[#...##...#] (0,1,2,6,7,9) (2,4,6,9) (3,7,8,9) (0,1,2,5,6,8,9) (5,8) (5,9) (1) (1,2) (1,4,6) (0,3,4,6,8) (0,1,2,3,4,5,6,7) (1,2,4,7,8) (0,2,3,4,5,6,7,9) {63,82,89,51,64,66,71,77,59,75}"
        soln1 `shouldBe` 0

      it "solves invalid" $ do 
        let mach_txt = "[#..####..] (3,6,8) (1,2,3,4,5,6,7) (5,6) (0,1,3,4) (1,2,3,4,5,7,8) (0,1,3,4,8) \
                       \(0,1,2,3,4,5,7,8) (0,2,4,5,6,8) (4,8) {37,50,37,52,77,41,26,26,54}"
            mach = P.parse pMachine mach_txt
            c_set = machToZ3 mach
        opt_result <- optimizeWith (z3 {verbose = False}) Lexicographic c_set
        let result_btns = getResultBtnPresses (length $ machButtons mach) opt_result
            soln1 = getResult opt_result
        -- print opt_result
        putStrLn $ "Final joltages: " ++ show (simulatePresses mach result_btns) ++ " Expected: " ++ show (machJoltages mach)
        soln1 `shouldBe` 83

    describe "simulatePresses" $ do 
      -- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      -- One way to do this is by pressing (3) once, (1,3) three times, (2,3) three times, (0,2) once, and (0,1) twice.
      it "does example" $ do 
        let joltages = simulatePresses mach1 [1, 3, 0, 3, 1, 2]
        joltages `shouldBe` [3, 5, 4, 7]

      it "does solved" $ do
        let joltages = simulatePresses mach1 [1, 2, 0, 4, 0, 3]
        joltages `shouldBe` [3, 5, 4, 7]

    describe "simPress" $ do 
      it "simulates simple" $ do 
        let joltages = simPress 3 (S.fromList [1, 3]) [0, 0, 0, 0]
        joltages `shouldBe` [0, 3, 0, 3]

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
