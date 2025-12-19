module Main (main) where

-- import Test.Day0 (testDay0)
import Test.GEOS qualified as GEOS
import Test.Day9 (test)
import Test.Hspec
  ( hspec,
  )

main :: IO ()
main = hspec $ do
  test
  GEOS.test

