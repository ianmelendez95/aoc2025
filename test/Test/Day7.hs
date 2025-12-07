module Test.Day7 (test) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text qualified as TIO
import Day7
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
import Data.Set qualified as S

newtype Range = Range
  { unRange :: (Int, Int)
  }
  deriving (Show)

test :: SpecWith ()
test =
  describe "Day7" $ do
    describe "soln" $ do
      it "short-input.txt" $ do
        res <- soln "test/Test/Day7/short.txt"
        res `shouldBe` 3263827

      it "full-input.txt" $ do
        res <- soln "test/Test/Day7/full.txt"
        putStrLn $ "Solution: " ++ show res
        res `shouldNotBe` 9194682052782

    describe "collide" $ do
      it "single" $ do 
        let res = collide (S.singleton 7) (S.singleton 7)
        res `shouldBe` S.fromList [6, 8]

      it "double" $ do 
        let res = collide (S.fromList [6, 8]) (S.fromList [6, 8])
        res `shouldBe` S.fromList [5, 7, 9]

      it "with miss" $ do 
        let res = collide (S.fromList [4, 6, 8, 10]) (S.fromList [4, 6, 10])
        res `shouldBe` S.fromList [3, 5, 7, 8, 9, 11]
        
    


