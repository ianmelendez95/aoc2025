module Day5 
  ( soln, 
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

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  let (ranges, ingrs) = readDb ls
  print ranges
  print ingrs
  pure 0

readDb :: [T.Text] -> ([(Int, Int)], [Int])
readDb ls = 
  let (ranges, rest) = span (/= "") ls
      ingrs = drop 1 rest
   in (map readRange ranges, map readInt ingrs)
  where 
    readRange :: T.Text -> (Int, Int)
    readRange line = 
      case T.split (=='-') line of 
        [s, e] -> (readInt s, readInt e)
        _ -> error "parse"

readInt :: T.Text -> Int
readInt = either error fst . decimal



