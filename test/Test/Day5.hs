module Test.Day5 (test) where

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    xit,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
    xdescribe,
    expectationFailure,
    shouldMatchList
  )
import Day5
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.Maybe (fromJust)

test :: SpecWith ()
test = 
  describe "Day5" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "test/Test/Day5/short.txt"
        res `shouldBe` 14

      it "full-input.txt" $ do 
        res <- soln "test/Test/Day5/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 325400210124724
        res `shouldNotBe` 325030422967918
        res `shouldNotBe` 312999017066962

      xit "full-trial" $ do 
        res <- soln "test/Test/Day5/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 325400210124724
        res `shouldNotBe` 325030422967918
        res `shouldNotBe` 312999017066962

      xit "first n" $ do 
        (ranges, _) <- readDbFile "test/Test/Day5/full.txt"
        let ranges' = take 5 ranges
        totalNums0 ranges' `shouldBe` totalNums1 ranges'

    describe "sumRanges0" $ do 
      it "short" $ do 
        sumRanges0 [(3, 5), (10, 20)] `shouldBe` 3 + 11

    describe "mergeRanges0" $ do 
      it "short" $ do 
        (ranges, _) <- readDbFile "test/Test/Day5/short.txt"
        let ranges' = [(3,5),(10,14),(16,20),(12,18)]
            merged = mergeRanges0 ranges'
        print ranges
        print merged
        merged `shouldBe` [(3, 5), (10, 20)]

    describe "readDbFile" $ do 
      it "reads" $ do 
        (ranges, ingrs) <- readDbFile "test/Test/Day5/full.txt"
        length ranges `shouldBe` 181
        length ingrs `shouldBe` (1182 - 182)


