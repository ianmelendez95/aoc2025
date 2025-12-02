module Test.Day1 (testDay1) where

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    hspec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
    xdescribe,
    expectationFailure
  )
import Day1
import Data.Text qualified as T
import Data.Text qualified as TIO

testDay1 :: SpecWith ()
testDay1 = 
  describe "Day1" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "src/Day1/short.txt"
        res `shouldBe` 1227775554

      it "full-input.txt" $ do 
        res <- soln "src/Day1/full.txt"
        putStrLn $ "Solution: " ++ show res

    describe "isRepeat" $ do 
      it "11" $ do 
        isRepeat 11 `shouldBe` True

      it "12" $ do 
        isRepeat 11 `shouldBe` True

      it "12-21" $ do
        noRepeats [12..21]

      -- 1188511880-1188511890
      -- 1188511885
      it "1188511880-1188511890" $ do 
        noRepeats [1188511880..1188511884]
        isRepeat 1188511885 `shouldBe` True
        noRepeats [1188511886..1188511890]


repeats :: [Int] -> IO ()
repeats = mapM_ (\n -> if not $ isRepeat n then expectationFailure $ "flagged not repeat: " ++ show n else pure ())

noRepeats :: [Int] -> IO ()
noRepeats = mapM_ (\n -> if isRepeat n then expectationFailure $ "flagged repeat: " ++ show n else pure ())
