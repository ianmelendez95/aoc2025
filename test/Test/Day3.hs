module Test.Day3 (testDay3) where

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    xit,
    xdescribe,
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
        res `shouldBe` 3121910778619

      xit "full-input.txt" $ do 
        res <- soln "test/Test/Day3/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldBe` 17332

      
    describe "findMaxNum" $ do
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
        takeAny0 2 "1234" `shouldMatchList` ["12", "13", "14", "23", "24", "34"]

    describe "dodecMax" $ do 
      it "987654321111111" $ do 
        dodecMax0 "987654321111111" `shouldBe` 987654321111
        dodecMax1 "987654321111111" `shouldBe` 987654321111

      it "811111111111119" $ do 
        dodecMax0 "811111111111119" `shouldBe` 811111111119
        dodecMax1 "811111111111119" `shouldBe` 811111111119

      it "234234234234278" $ do 
        dodecMax0 "234234234234278" `shouldBe` 434234234278
        dodecMax1 "234234234234278" `shouldBe` 434234234278

      it "818181911112111" $ do 
        dodecMax0 "818181911112111" `shouldBe` 888911112111
        dodecMax1 "818181911112111" `shouldBe` 888911112111

    describe "buildBigNumFinal" $ do 
      it "987654321111111" $ do 
        buildBigNumFinal1 12 (charsWithPos1 "987654321111111") `shouldBe` 987654321111

      it "811111111111119" $ do 
        dodecMax0 "811111111111119" `shouldBe` 811111111119
        dodecMax1 "811111111111119" `shouldBe` 811111111119

      it "234234234234278" $ do 
        dodecMax0 "234234234234278" `shouldBe` 434234234278
        dodecMax1 "234234234234278" `shouldBe` 434234234278

      it "818181911112111" $ do 
        dodecMax0 "818181911112111" `shouldBe` 888911112111
        dodecMax1 "818181911112111" `shouldBe` 888911112111

      it "14" $ do 
        buildBigNumFinal1 2 (charsWithPos1 "14") `shouldBe` 14

    xdescribe "buildBigNum" $ do 
      it "987654321111111" $ do 
        buildBigNum1 12 (charsWithPos1 "987654321111111") `shouldBe` Just "987654321111"

      it "818181911112111" $ do 
        buildBigNum1 12 (charsWithPos1 "818181911112111") `shouldBe` Just "888911112111"

      it "14" $ do 
        buildBigNum1 2 (charsWithPos1 "14") `shouldBe` Just "14"

      it "1" $ do 
        buildBigNum1 1 (charsWithPos1 "1") `shouldBe` Just "1"

    describe "buildBigNum'" $ do 
      it "test1" $ do 
        buildBigNum1' 1 (1, '1') [(0, '4'), (1, '1')] `shouldBe` ["14"]
        {--
         - buildBigNum1' 2 (1, '1') [(0, '4'), (1, '1')]
         - cs_after_c = [(0, '4')]
         - cs_with_enough = 
        --}
      
      it "123" $ do 
        -- [(0,'3'),(1,'2'),(2,'1')]
        buildBigNum1' 2 (2, '1') (charsWithPos1 "123") `shouldBe` ["123"]
        
    describe "charsWithPos1" $ do 
      it "1234" $ do
        charsWithPos1 "1234" `shouldBe` [(0, '4'), (1, '3'), (2, '2'), (3, '1')]

      it "14" $ do
        charsWithPos1 "14" `shouldBe` [(0, '4'), (1, '1')]

    describe "withEach" $ do 
      it "1234" $ do 
        withEach0 (\x xs -> Just (x : xs)) "1234" `shouldBe` ["1234","2134","3214","4321"]



