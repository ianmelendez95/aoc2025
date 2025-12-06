module Day6
  ( soln, 
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
  print ls
  pure 0

readInt :: T.Text -> Int
readInt = either error fst . decimal

-- trace :: String -> a -> a
-- trace _ x = x

