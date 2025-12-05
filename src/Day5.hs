module Day5 
  ( soln, 
    readDbFile,
    readRange,
    mergeRanges0
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf, sortBy, intersperse, intercalate)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.Ord (Down (..), comparing)

import Debug.Trace (trace)

import Data.Map qualified as M
import Data.Set qualified as S

import Debug.Trace (trace)

type Range = (Int, Int)

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let (ranges, ingrs) = readDb ls
      fresh_ingrs = filter (not . isSpoiled0 ranges) ingrs
  pure $ length fresh_ingrs

mergeRanges0 :: [Range] -> [Range]
mergeRanges0 rs = 
  let rs' = foldl mergeRange0 [] rs
   in if length rs == length rs' then rs else mergeRanges0 rs'

mergeRange0 :: [Range] -> Range -> [Range]
mergeRange0 [] r' = trace ("\nadding end: " ++ show r') [r']
mergeRange0 all_rs@(r@(s, e):rs) r'@(s', e') 
  | e' < s = trace ("\nprepending: " ++ show r' ++ " - " ++ show all_rs) $ r' : r : rs
  | e' `isBetween` r = trace ("\nend is between: " ++ show r' ++ " - " ++ show all_rs) $ (s', e) : rs
  | s' `isBetween` r = trace ("\nstart is between: " ++ show r' ++ " - " ++ show all_rs) $ (s, e') : rs
  | otherwise = trace ("\ncontinuing: " ++ show r' ++ " - " ++ show all_rs) $ r : mergeRange0 rs r'

isSpoiled0 :: [Range] -> Int -> Bool
isSpoiled0 rs i = not $ any (i `isBetween`) rs

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

readInt :: T.Text -> Int
readInt = either error fst . decimal



