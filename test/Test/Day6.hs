module Test.Day6 (test) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text qualified as TIO
import Day6
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

newtype Range = Range
  { unRange :: (Int, Int)
  }
  deriving (Show)

test :: SpecWith ()
test =
  describe "Day6" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln "test/Test/Day6/short.txt"
        res `shouldBe` 3263827

      xit "full-input.txt" $ do
        res <- soln "test/Test/Day6/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 325400210124724

    describe "parseInput1" $ do
      it "parses one col" $ do
        let rows :: [T.Text]
            rows = ["123", " 45", "  6", "*   "]
        parseInput1 rows `shouldBe` [([1, 24, 356], Mult)]

      it "parses two col" $ do
        let rows :: [T.Text]
            rows =
              [ "123 328",
                " 45 64 ",
                "  6 98 ",
                "*   +  "
              ]
        parseInput1 rows `shouldBe` [([1, 24, 356], Mult), ([369, 248, 8], Add)]

    describe "parseMathRow0" $ do
      it "col1" $ do
        parseNums0 ["123", " 45", "  6"] `shouldMatchList` [1, 24, 356]

      it "col2" $ do
        parseNums0 ["328", "64 ", "98 "] `shouldMatchList` [369, 248, 8]

      it "col3" $ do
        parseNums0 [" 51", "387", "215"] `shouldMatchList` [32, 581, 175]

      it "col4" $ do
        parseNums0 ["64 ", "23 ", "314"] `shouldMatchList` [623, 431, 4]

