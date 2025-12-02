module Main (main) where

-- import Test.Day0 (testDay0)
import Test.Day1 (testDay1)
import Test.Hspec
  ( hspec,
  )
import Control.Concurrent

main :: IO ()
main = do 
  n <- getNumCapabilities 
  putStrLn $ "THREADS: " ++ show n
  hspec $ do
    testDay1
