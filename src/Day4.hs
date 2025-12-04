module Day4 
  ( soln, 
    readRollSet,
    adjCoords0,
    ableRoll0
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
import Data.Set qualified as S

type RollSet = S.Set Coord
type Coord = (Int, Int)

soln :: FilePath -> IO Int
soln file = do
  roll_map <- readRollSet file
  pure $ ableRollCount0 roll_map

readRollSet :: FilePath -> IO RollSet
readRollSet file = do
  roll_lines <- T.lines <$> TIO.readFile file
  pure $ readRollRows0 roll_lines

-- pruneRolls0 :: RollSet -> RollSet

ableRollCount0 :: RollSet -> Int
ableRollCount0 roll_map = length . filter (`ableRoll0` roll_map) $ S.toList roll_map

ableRoll0 :: Coord -> RollSet -> Bool
ableRoll0 coord roll_map = isRollCoord coord roll_map && adjRolls0 coord roll_map < 4

adjRolls0 :: Coord -> RollSet -> Int
adjRolls0 coord roll_map = length . filter (`isRollCoord` roll_map) $ adjCoords0 coord

isRollCoord :: Coord -> RollSet -> Bool
isRollCoord = S.member 

adjCoords0 :: Coord -> [Coord]
adjCoords0 coord@(r, c) = concatMap (filter (coord /=). zip [r-1..r+1] . repeat) [c-1..c+1]

readRollRows0 :: [T.Text] -> RollSet
readRollRows0 row_txts = S.fromList . concat $ zipWith readRollRow0 [0..] row_txts

readRollRow0 :: Int -> T.Text -> [Coord]
readRollRow0 row_n = map fst . filter snd . zipWith (\col_n is_roll -> ((row_n, col_n), is_roll)) [0..] . map (== '@') . T.unpack


