module Day6
  ( soln,
    parseInput1,
    parseNums0,
    Op (..)
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
import Debug.Trace (trace, traceShowId, traceWith)
import Text.Read (readMaybe)

type Range = (Int, Int)

type NumSet = S.Set Int

type MathRow = ([Int], Op)

data Op = Add | Mult deriving (Show, Eq)

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let math_rows = parseInput1 ls
      -- math_results = map doMathRow0 math_rows
      res = sum $ map doMathRow0 math_rows
  -- mapM_ print math_rows
  -- mapM_ print math_results
  -- print ls
  pure res

doMathRow0 :: MathRow -> Int
doMathRow0 (ns, Add) = sum ns
doMathRow0 (ns, Mult) = product ns

parseInput1 :: [T.Text] -> [MathRow]
parseInput1 rows = 
  case reverse rows of 
    [] -> error $ "reverse: " ++ show rows
    (op_row : num_rows) -> 
      parseInput1' op_row (reverse num_rows)

parseInput1' :: T.Text -> [T.Text] -> [MathRow]
parseInput1' op_row num_rows = 
  case T.uncons op_row of 
    Nothing -> []
    Just (op_char, rest_with_blanks) -> 
      let (blanks, op_row_rest) = T.break (/=' ') rest_with_blanks
          op = parseOp op_char
          parse_len = T.length blanks + 1 -- include the op char itself

          split_num_txts = map (T.splitAt parse_len) num_rows
          num_row_rests = map snd split_num_txts
          num_txts = map fst split_num_txts
          
          nums = parseNums0 num_txts
       in (nums, op) : parseInput1' op_row_rest num_row_rests

parseNums0 :: [T.Text] -> [Int]
parseNums0 = map readInt . filter (/= "") . map T.strip . T.transpose

parseOp :: Char -> Op
parseOp '+' = Add
parseOp '*' = Mult
parseOp x = error $ "op parse: " ++ [x]

-- trace :: String -> a -> a
-- trace _ x = x

readInt :: T.Text -> Int
readInt = either error fst . decimal
