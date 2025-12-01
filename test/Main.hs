module Main (main) where

import Day0 (doMoves, doMoves') 
import Test.Hspec
  ( hspec,
    describe,
    it,
    shouldBe,
  )
 

main :: IO ()
main = hspec $ do
  describe "Day0" $ do 
    describe "doMoves" $ do 
      it "-68" $ do 
        let res = doMoves [-68]
        res `shouldBe` 1
      it "-68 -30 48" $ do 
        let res = doMoves [-68, -30, 48]
        res `shouldBe` 2
      it "-68 -30 48 -5" $ do 
        let res = doMoves [-68, -30, 48, -5]
        res `shouldBe` 2
      it "-68 -30 48 -5 60" $ do 
        let res = doMoves [-68, -30, 48, -5, 60]
        res `shouldBe` 3

      it "0 200" $ do 
        let res = doMoves' 0 [200]
        res `shouldBe` 2
      it "0 200 -1" $ do 
        let res = doMoves' 0 [200, -1]
        res `shouldBe` 2
      


