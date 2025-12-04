module Day4 
  ( soln, 
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.Ord (Down (..), comparing)

import Debug.Trace (trace)

soln :: FilePath -> IO Int
soln file = do
  roll_lines <- T.lines <$> TIO.readFile file
  print roll_lines
  pure 0


