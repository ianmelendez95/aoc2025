module Day0
  ( soln,
    moveDial',
    doMoves,
    doMoves',
    execDial,
    mkDial,
    doMovesState,
    doMovesState',
    moveDialRight,
    moveDialLeft,
    moveDialStates,
    moveDialState,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Debug.Trace (traceShowId)

import Control.Monad (void, replicateM_, mapM_)
import Control.Monad.State (State, get, put, runState, execState)

data DialCtx = DialCtx {
  _dialPos :: Int,
  _dialZeros :: Int
} deriving (Show, Eq)

mkDial :: Int -> DialCtx
mkDial start = DialCtx start 0

type DialState = State DialCtx

readInput :: IO T.Text
readInput = TIO.readFile "src/Day0/short.txt" -- 3
-- readInput = TIO.readFile "src/Day0/full.txt" -- 1097

soln :: IO ()
soln = do
  content <- readInput
  let nums = map readLine (T.lines content)
  -- accum_sum = scanl moveDial 50 nums
  -- print nums
  -- print accum_sum
  -- TIO.putStrLn content
  print $ doMoves nums

execDial :: DialState a -> DialCtx -> DialCtx
execDial = execState

doMoves :: [Int] -> (Int, Int)
doMoves = doMoves' 50

doMovesState :: [Int] -> (Int, Int)
doMovesState = doMovesState' 50

doMovesState' :: Int -> [Int] -> (Int, Int)
doMovesState' start moves = 
  let (DialCtx pos zeros) = execDial (moveDialStates moves) (mkDial start)
   in (pos, zeros)

doMoves' :: Int -> [Int] -> (Int, Int)
doMoves' start = foldl' doMove (start, 0)
  where
    doMove :: (Int, Int) -> Int -> (Int, Int)
    doMove (cur_pos, passes) delta =
      let (next, next_passes) = moveDial' cur_pos delta
       in (next, passes + next_passes)

moveDialStates :: [Int] -> DialState ()
moveDialStates = mapM_ moveDialState

moveDialState :: Int -> DialState ()
moveDialState delta
  | delta == 0 = pure ()
  | delta > 0 = replicateM_ delta moveDialRight
  | otherwise = replicateM_ (abs delta) moveDialLeft

moveDial' :: Int -> Int -> (Int, Int)
moveDial' start delta
  | next_raw < 0 =
      let (q, r) = next_raw `quotRem` 100
       in (r + 100, (abs q) + (if start == 0 then 0 else 1))
  | next_raw >= 100 =
      let (q, r) = next_raw `quotRem` 100
       in (r, q)
  | next_raw == 0 = (0, 1)
  | otherwise = (next_raw, 0)
  where
    next_raw = start + delta

moveDialRight :: DialState ()
moveDialRight = do 
  (DialCtx pos zeros) <- get
  if pos >= 99 
    then put (DialCtx 0 (zeros + 1))
    else put (DialCtx (pos + 1) zeros)

moveDialLeft :: DialState ()
moveDialLeft = do 
  (DialCtx pos zeros) <- get
  case pos of 
    0 -> put (DialCtx 99 zeros)
    1 -> put (DialCtx 0 (zeros + 1))
    _ -> put (DialCtx (pos - 1) zeros)

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
