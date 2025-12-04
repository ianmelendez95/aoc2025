module Test.Day3 (testDay3) where

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    xit,
    shouldBe,
    shouldSatisfy,
    xdescribe,
    expectationFailure,
    shouldMatchList
  )
import Day3
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.Maybe (fromJust)

testDay3 :: SpecWith ()
testDay3 = 
  describe "Day3" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "test/Test/Day3/short.txt"
        res `shouldBe` 357

      xit "full-input.txt" $ do 
        res <- soln "test/Test/Day3/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldBe` 17332

      
      -- 987654321111111
      -- 811111111111119
      -- 234234234234278
      -- 818181911112111
    describe "findMaxNum0" $ do
      it "987654321111111" $ do 
        findMaxNum0 "987654321111111" `shouldBe` 98

      it "811111111111119" $ do 
        findMaxNum0 "811111111111119" `shouldBe` 89

      it "234234234234278" $ do 
        findMaxNum0 "234234234234278" `shouldBe` 78

      it "818181911112111" $ do 
        findMaxNum0 "818181911112111" `shouldBe` 92

    describe "takeAny0" $ do 
      it "2 1234" $ do 
        fromJust (takeAny0 2 "1234") `shouldMatchList` ["12", "23", "34", "13", "24", "34"]
        



