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
-- readInput = TIO.readFile "src/Day0/short.txt" -- 3
readInput = TIO.readFile "src/Day0/full.txt" -- p1: 1097, p2: 7101

soln :: IO ()
soln = do
  content <- readInput
  let nums = map readLine (T.lines content)
  -- accum_sum = scanl moveDial 50 nums
  -- print nums
  -- print accum_sum
  -- TIO.putStrLn content
  print $ doMoves nums

doMovesC :: Int -> [Int] -> (Int, Int)
doMovesC start = foldl' doMoveC (start, 0)

doMoveC :: (Int, Int) -> Int -> (Int, Int)
doMoveC (start, zeros) delta = 
  let (DialCtx s_pos s_zeros) = execState (moveDialState delta) (mkDial start)
      (m_pos, m_zeros) = moveDial' start delta
   in if s_pos /= m_pos
        then error $ "Mismatch! pos: " ++ show (start, delta, s_pos, m_pos, s_zeros, m_zeros)
        else if s_zeros /= m_zeros 
               then error $ "Mismatch! zeros: " ++ show (start, delta, s_pos, m_pos, s_zeros, m_zeros)
               else (s_pos, s_zeros + zeros)

execDial :: DialState a -> DialCtx -> DialCtx
execDial = execState

doMoves :: [Int] -> (Int, Int)
doMoves = doMoves' 50

doMoves' :: Int -> [Int] -> (Int, Int)
doMoves' start = foldl' doMove (start, 0)
  where
    doMove :: (Int, Int) -> Int -> (Int, Int)
    doMove (cur_pos, passes) delta =
      let (next, next_passes) = moveDial' cur_pos delta
       in (next, passes + next_passes)

doMovesState :: [Int] -> (Int, Int)
doMovesState = doMovesState' 50

doMovesState' :: Int -> [Int] -> (Int, Int)
doMovesState' start moves = 
  let (DialCtx pos zeros) = execDial (moveDialStates moves) (mkDial start)
   in (pos, zeros)

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
       in (if r >= 0 then r else r + 100, abs q + (if start == 0 then 0 else 1))
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
