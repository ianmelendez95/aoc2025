module Day0 (soln, moveDial', doMoves) where 

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

doMoves :: [Int] -> (Int, Int)
doMoves = foldl' doMove (50, 0)
  where 
    doMove :: (Int, Int) -> Int -> (Int, Int)
    doMove (cur_pos, passes) delta = 
      let (next, next_passes) = moveDial' cur_pos delta
       in (next, passes + next_passes)

moveDial' :: Int -> Int -> (Int, Int)
moveDial' start delta 
  | next_raw < 0 = 
     let (q, r) = next_raw `quotRem` 100
      in (r + 100, (abs q) + 1)
  | next_raw >= 100 = 
     let (q, r) = next_raw `quotRem` 100
      in (r, q)
  | next_raw == 0 = (0, 1)
  | otherwise = (next_raw, 0)
  where 
    next_raw = start + delta

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
