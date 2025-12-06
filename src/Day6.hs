module Day6
  ( soln,
  )
where

import Control.Applicative (liftA2)
import Data.List
  ( intercalate,
    intersperse,
    isPrefixOf,
    sortBy,
    transpose,
    unsnoc
  )
import Data.List.Split (chunksOf, splitEvery)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Debug.Trace (trace)
import Text.Read (readMaybe)

type Range = (Int, Int)

type NumSet = S.Set Int

type MathRow = ([Int], Op)

data Op = Add | Mult deriving Show

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let math_rows = parseInput0 ls
  mapM_ print math_rows
  print ls
  pure 0

parseInput0 :: [T.Text] -> [MathRow]
parseInput0 ls =
  let matrix = map T.words ls
      matrix' = transpose matrix
   in map parseMathRow0 matrix'

parseMathRow0 :: [T.Text] -> MathRow
parseMathRow0 ws = 
  case unsnoc ws of 
    Nothing -> error "parse"
    Just (nums, op) -> (map readInt nums, parseOp $ T.unpack op)

parseOp :: String -> Op
parseOp "+" = Add
parseOp "*" = Mult
parseOp x = error $ "op parse: " ++ x

-- trace :: String -> a -> a
-- trace _ x = x

readInt :: T.Text -> Int
readInt = either error fst . decimal
