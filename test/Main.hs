module Main (main) where

import Day0 (moveDial', doMoves, doMoves', doMovesState, doMovesState') 
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
  )
 

main :: IO ()
main = hspec $ do
  describe "Day0" $ do 
    describe "moveDial'" $ do 
      it "50 + 5" $ do 
        let res = moveDial' 50 5
        res `shouldBe` (55, 0)
      it "50 + 55" $ do 
        let res = moveDial' 50 55
        res `shouldBe` (5, 1)
      it "50 + 200" $ do 
        let res = moveDial' 50 200
        res `shouldBe` (50, 2)
      it "50 + 200" $ do 
        let res = moveDial' 50 200
        res `shouldBe` (50, 2)
      it "50 - 55" $ do 
        let res = moveDial' 50 (-55)
        res `shouldBe` (95, 1)
      it "50 - 200" $ do 
        let res = moveDial' 50 (-200)
        res `shouldBe` (50, 2)
      it "50 - 50" $ do 
        let res = moveDial' 50 (-50)
        res `shouldBe` (0, 1)
      it "0 - 5" $ do 
        let res = moveDial' 0 (-5)
        res `shouldBe` (95, 0)

    describe "doMoves" $ do 
      it "-68" $ do 
        let res = doMoves [-68]
        res `shouldBe` (82, 1)
      it "-68 -30 48" $ do 
        let res = doMoves [-68, -30, 48]
        res `shouldBe` (0, 2)
      it "-68 -30 48 -5" $ do 
        let res = doMoves [-68, -30, 48, -5]
        res `shouldBe` (95, 2)
      it "-68 -30 48 -5 60" $ do 
        let res = doMoves [-68, -30, 48, -5, 60]
        res `shouldBe` (55, 3)

      it "0 200" $ do 
        let res = doMoves' 0 [200]
        res `shouldBe` (0, 2)
      it "0 200 -1" $ do 
        let res = doMoves' 0 [200, -1]
        res `shouldBe` (99, 2)
    
    describe "moveDialState" $ do 
      it "-68" $ do 
        let res = doMovesState [-68]
        res `shouldBe` (82, 1)
      it "-68 -30 48" $ do 
        let res = doMovesState [-68, -30, 48]
        res `shouldBe` (0, 2)
      it "-68 -30 48 -5" $ do 
        let res = doMovesState [-68, -30, 48, -5]
        res `shouldBe` (95, 2)
      it "-68 -30 48 -5 60" $ do 
        let res = doMovesState [-68, -30, 48, -5, 60]
        res `shouldBe` (55, 3)

      it "0 200" $ do 
        let res = doMovesState' 0 [200]
        res `shouldBe` (0, 2)
      it "0 200 -1" $ do 
        let res = doMovesState' 0 [200, -1]
        res `shouldBe` (99, 2)
      


