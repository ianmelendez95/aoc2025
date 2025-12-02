module Main (main) where 

import Day1
import Control.Concurrent

main :: IO ()
main = do 
  -- n <- getNumCapabilities 
  -- putStrLn $ "THREADS: " ++ show n
  soln "src/Day1/full.txt" >>= print
