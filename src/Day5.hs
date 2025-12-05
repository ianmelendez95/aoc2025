module Day5 
  ( soln, 
    readDbFile,
    readRange
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

type Range = (Int, Int)

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let (ranges, ingrs) = readDb ls
      fresh_ingrs = filter (not . isSpoiled0 ranges) ingrs
  pure $ length fresh_ingrs

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



