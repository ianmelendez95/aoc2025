module Test.Day6 (test) where

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
import Day6
import Data.Text qualified as T
import Data.Text qualified as TIO
import Data.Maybe (fromJust)
import Test.QuickCheck

newtype Range = Range {
  unRange :: (Int, Int)
} deriving Show

test :: SpecWith ()
test = 
  describe "Day6" $ do 
    describe "soln" $ do 
      it "short-input.txt" $ do 
        res <- soln "test/Test/Day6/short.txt"
        res `shouldBe` 14

      xit "full-input.txt" $ do 
        res <- soln "test/Test/Day6/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 325400210124724
        res `shouldNotBe` 325030422967918
        res `shouldNotBe` 312999017066962


