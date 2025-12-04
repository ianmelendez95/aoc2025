module Main (main) where

-- import Test.Day0 (testDay0)
import Test.Day3 (testDay3)
import Test.Hspec
  ( hspec,
  )

main :: IO ()
main = hspec $ do
  testDay3
