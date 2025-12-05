module Day5 
  ( soln, 
    readDbFile,
    readRange,
    mergeRanges0,
    sumRanges0
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

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let (ranges, _) = readDb ls
      merged_ranges = mergeRanges0 ranges
  pure $ sumRanges0 merged_ranges

sumRanges0 :: [Range] -> Int
sumRanges0 = sum . map (\(s, e) -> e - s + 1)

mergeRanges0 :: [Range] -> [Range]
mergeRanges0 = foldl' mergeRange0 [] . sortBy compareRange

mergeRange0 :: [Range] -> Range -> [Range]
mergeRange0 [] r' = trace ("\nadding end: " ++ commaRange r') [r']
mergeRange0 all_rs@(r@(s, e):rs) r'@(s', e') 
  | e' < s = traceStep "prepending" $ r' : r : rs
  | e' `isBetween` r = traceStep "end is between" $ (s', e) : rs
  | s' `isBetween` r =  
      let trace' = traceStep "start is between"
       in trace' $ mergeRange0 rs (s, e')
  | otherwise = traceStep "continuing" $ r : mergeRange0 rs r'
  where 
    traceStep :: String -> a -> a
    traceStep prefix = trace ("\n" ++ prefix ++ ": " ++ commaRange r' ++ " - " ++ concatMap commaRange (take 3 all_rs))

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

