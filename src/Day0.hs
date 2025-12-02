module Day0
  ( soln,
    moveDial',
    doMoves,
    doMoves',
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read

readInput :: IO T.Text
-- readInput = TIO.readFile "src/Day0/short.txt" -- 3
readInput = TIO.readFile "src/Day0/full.txt" -- p1: 1097, p2: 7101

soln :: IO ()
soln = do
  content <- readInput
  let nums = map readLine (T.lines content)
  print . snd $ doMoves nums

doMoves :: [Int] -> (Int, Int)
doMoves = doMoves' 50

doMoves' :: Int -> [Int] -> (Int, Int)
doMoves' start = foldl' doMove (start, 0)
  where
    doMove :: (Int, Int) -> Int -> (Int, Int)
    doMove (cur_pos, passes) delta =
      let (next, next_passes) = moveDial' cur_pos delta
       in (next, passes + next_passes)

moveDial' :: Int -> Int -> (Int, Int)
moveDial' start delta =
  let next_raw = start + delta
      next_pos = next_raw `mod` 100
      next_zeros = 
        if next_raw > 0 
          then next_raw `div` 100
          else abs (next_raw `quot` 100) + (if start == 0 then 0 else 1)
   in (next_pos, next_zeros)

readLine :: T.Text -> Int
readLine line =
  case T.uncons line of
    Just ('L', num) -> -(readInt num)
    Just ('R', num) -> readInt num
    _ -> error $ show line

readInt :: T.Text -> Int
readInt = either error fst . decimal
