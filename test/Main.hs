module Main (main) where

-- import Test.Day0 (testDay0)
import Test.Day5 (test)
import Test.Hspec
  ( hspec,
  )

main :: IO ()
main = hspec $ do
  test
