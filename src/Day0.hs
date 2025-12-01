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
moveDial' start delta
  | next_raw < 0 =
      let (q, r) = next_raw `quotRem` 100
       in (r `mod` 100, abs q + (if start == 0 then 0 else 1))
  | next_raw >= 100 =
      let (q, r) = next_raw `quotRem` 100
       in (r, q)
  | next_raw == 0 = (0, 1)
  | otherwise = (next_raw, 0)
  where
    next_raw = start + delta

readLine :: T.Text -> Int
readLine line =
  case T.uncons line of
    Just ('L', num) -> -(readInt num)
    Just ('R', num) -> readInt num
    _ -> error $ show line

readInt :: T.Text -> Int
readInt = either error fst . decimal
