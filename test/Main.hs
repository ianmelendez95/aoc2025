module Main (main) where

-- import Test.Day0 (testDay0)
import Test.Day1 (testDay1)
import Test.Hspec
  ( hspec,
  )

main :: IO ()
main = hspec $ do
  testDay1
