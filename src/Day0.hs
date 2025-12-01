module Day0
  ( soln,
    mkDial,
    moveDialRight,
    moveDialLeft,
    doMoves,
    doMoves'
  )
where

import Control.Monad
  ( replicateM_,
  )
import Control.Monad.State (State, execState, get, put)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read

data DialCtx = DialCtx
  { _dialPos :: Int,
    _dialZeros :: Int
  }
  deriving (Show, Eq)

mkDial :: Int -> DialCtx
mkDial start = DialCtx start 0

type DialState = State DialCtx

readInput :: IO T.Text
-- readInput = TIO.readFile "src/Day0/short.txt" -- 3
readInput = TIO.readFile "src/Day0/full.txt" -- p1: 1097, p2: 7101

soln :: IO ()
soln = do
  content <- readInput
  let moves = map readLine (T.lines content)
  putStr "Solution: "
  print $ doMoves moves

doMoves :: [Int] -> Int
doMoves = doMoves' 50

doMoves' :: Int -> [Int] -> Int
doMoves' start moves = _dialZeros $ execState (mapM_ moveDial moves) (mkDial start)

moveDial :: Int -> DialState ()
moveDial delta
  | delta == 0 = pure ()
  | delta > 0 = replicateM_ delta moveDialRight
  | otherwise = replicateM_ (abs delta) moveDialLeft

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

readLine :: T.Text -> Int
readLine line =
  case T.uncons line of
    Just ('L', num) -> -(readInt num)
    Just ('R', num) -> readInt num
    _ -> error $ show line

readInt :: T.Text -> Int
readInt = either error fst . decimal
