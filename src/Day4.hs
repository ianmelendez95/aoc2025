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

import Data.Map qualified as M

type Coord = (Int, Int)

soln :: FilePath -> IO Int
soln file = do
  roll_lines <- T.lines <$> TIO.readFile file
  mapM_ print . M.toList $ readRollRows0 roll_lines
  pure 0

readRollRows0 :: [T.Text] -> M.Map Coord Bool
readRollRows0 row_txts = M.fromList . concat $ zipWith readRollRow0 [0..] row_txts

readRollRow0 :: Int -> T.Text -> [(Coord, Bool)]
readRollRow0 row_n = zipWith (\col_n is_roll -> ((row_n, col_n), is_roll)) [0..] . map (== '@') . T.unpack


