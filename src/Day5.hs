module Day5 
  ( soln, 
    readDbFile,
    readRange,
    mergeRanges0,
    totalNums0,
    totalNums1,
    sumRanges0,
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf, sortBy, intersperse, intercalate)
import Data.List.Split (chunksOf, splitEvery)
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.Ord (Down (..), comparing)

import Data.Map qualified as M
import Data.Set qualified as S

import Debug.Trace (trace)

type Range = (Int, Int)

type NumSet = S.Set Int

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let (ranges, _) = readDb ls
  pure $ totalNums0 ranges

totalNums0 :: [Range] -> Int
totalNums0 ranges = 
  let merged_ranges = mergeRanges0 ranges
   in sumRanges0 merged_ranges

totalNums1 :: [Range] -> Int
totalNums1 = S.size . S.unions . map rangeNums1

rangeNums1 :: Range -> NumSet
rangeNums1 (s, e) = S.fromList [s..e]

sumRanges0 :: [Range] -> Int
sumRanges0 = sum . map (\(s, e) -> e - s + 1)

mergeRanges0 :: [Range] -> [Range]
mergeRanges0 = foldl' mergeRange0 [] . sortBy compareRange

mergeRange0 :: [Range] -> Range -> [Range]
mergeRange0 [] r' = {--trace ("\nadding end: " ++ commaRange r')--} [r']
mergeRange0 all_rs@(r@(s, e):rs) r'@(s', e') 
  | e' < s = traceStep "prepending" $ r' : r : rs
  | e' `isBetween` r = traceStep "end is between" $ mergePair r r' : rs
  | s' `isBetween` r =  
      let trace' = traceStep "start is between"
       in trace' $ mergeRange0 rs (mergePair r r')
  | otherwise = traceStep "continuing" $ r : mergeRange0 rs r'
  where 
    traceStep :: String -> a -> a
    traceStep prefix = id
      -- trace ("\n" ++ prefix ++ ": " ++ commaRange r' ++ " - " ++ concatMap commaRange (take 3 all_rs))

    mergePair :: Range -> Range -> Range
    mergePair (ms, me) (ms', me') = (min ms ms', max me me')

compareRange :: Range -> Range -> Ordering
compareRange = comparing fst

isBetween :: Int -> Range -> Bool
isBetween i (l, r) = i >= l && i <= r

readDbFile :: FilePath -> IO ([Range], [Int])
readDbFile file = readDb . T.lines <$> TIO.readFile file

readDb :: [T.Text] -> ([Range], [Int])
readDb ls = 
  let (ranges, rest) = span (/= "") ls
      ingrs = drop 1 rest
   in (map readRange ranges, map readInt ingrs)

readRange :: T.Text -> (Int, Int)
readRange line = 
  case T.split (=='-') line of 
    [s, e] -> (readInt s, readInt e)
    _ -> error "parse"

commaRange :: Range -> String
commaRange (s, e) = "(" ++ commaNum s ++ "-" ++ commaNum e ++ ")"

commaNum :: Int -> String
commaNum x = reverse (intercalate "," . chunksOf 3 . reverse . show $ x)

readInt :: T.Text -> Int
readInt = either error fst . decimal

-- trace :: String -> a -> a
-- trace _ x = x

