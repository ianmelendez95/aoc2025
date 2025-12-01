module Day0 where 

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read

import Debug.Trace (traceShowId)

readInput :: IO T.Text
-- readInput = TIO.readFile "src/Day0/short.txt" -- 3
readInput = TIO.readFile "src/Day0/full.txt" -- 1097

soln :: IO () 
soln = do 
  content <- readInput
  let nums = map readLine (T.lines content)
      accum_sum = scanl moveDial 50 nums
  -- print nums
  -- print accum_sum 
  -- TIO.putStrLn content
  print $ length $ filter (==0) accum_sum

accumSum :: Int -> [Int] -> [Int]
accumSum = scanl (+)

moveDial :: Int -> Int -> Int
moveDial start delta 
  | next < 0 = 100 + next
  | otherwise = next
  where 
    next = (start + delta) `mod` 100

readLine :: T.Text -> Int
readLine line = 
  case T.uncons line of 
    Just ('L', num) -> -(readInt num) 
    Just ('R', num) -> readInt num
    _ -> error $ show line

readInt :: T.Text -> Int
readInt = either error fst . decimal
