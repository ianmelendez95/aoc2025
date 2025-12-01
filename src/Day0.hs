module Day0 where 

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

readInput :: IO T.Text
readInput = TIO.readFile "files/Day0/short.txt"

soln :: IO () 
soln = do 
  content <- readInput
  TIO.putStrLn content
