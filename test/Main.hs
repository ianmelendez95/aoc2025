module Main (main) where

-- import Test.Day0 (testDay0)
import Test.Day6 (test)
import Test.Hspec
  ( hspec,
  )

main :: IO ()
main = hspec $ do
  test
