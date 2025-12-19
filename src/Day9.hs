module Day9
  ( soln,
    readCoordsFile,
    bigRects0,
    bigRectsCoords0,
    pairArea0,
    pairs0,
  )
where

import Control.Monad (filterM)
import Control.Applicative (liftA2)
import Data.List
  ( find,
    foldl',
    intercalate,
    intersperse,
    isPrefixOf,
    maximumBy,
    minimumBy,
    sort,
    sortBy,
    transpose,
    uncons,
    unsnoc,
  )
import Data.List.Split (chunksOf, splitEvery)
import Data.Map qualified as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
    listToMaybe,
    maybe,
  )
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Debug.Trace (trace, traceShowId, traceWith)
import Text.Read (readMaybe)
import GEOS

type Coord = (Int, Int)

type Rect = (Coord, Coord)

soln :: FilePath -> IO Int
soln file = do
  coords <- readCoordsFile file
  let input_poly_wkt = wktPolygon coords
      big_rects = bigRects0 (pairs0 $ sort coords)
  big_rect_in_poly <- findIO (polyContainsRectIO input_poly_wkt) big_rects
  case big_rect_in_poly of 
    Nothing -> error "NO BIG RECTS"
    Just big_rect -> 
      pure $ pairArea0 big_rect

findIO :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findIO _ [] = pure Nothing
findIO f (x:xs) = do
  p <- f x
  if p 
  then pure $ Just x
  else findIO f xs
  
polyContainsRectIO :: T.Text -> Rect  -> IO Bool
polyContainsRectIO poly_wkt rect_pair = do
  -- putStrLn $ "Checking: " ++ show rect_pair
  let result = polyContainsRect poly_wkt rect_pair
  -- putStrLn $ "Contains: rect=" ++ show rect_pair ++ " :" ++ show result
  pure result

polyContainsRect :: T.Text -> Rect  -> Bool
polyContainsRect poly_wkt rect_pair =
  geosContains poly_wkt (wktPolygon $ rectCoords rect_pair)

rectCoords :: Rect -> [Coord]
rectCoords ((x1, y1), (x2, y2)) = 
  [ (x1, y1),
    (x1, y2),
    (x2, y2),
    (x2, y1)
  ]

bigRectsCoords0 :: [Coord] -> [Rect]
bigRectsCoords0 = bigRects0 . pairs0 . sort

bigRects0 :: [Rect] -> [Rect]
bigRects0 = map snd . sortBy (comparing (Down . fst)) . map (\cs -> (pairArea0 cs, cs))

bigRect0 :: [Rect] -> Int
bigRect0 = maximum . map pairArea0

pairArea0 :: Rect -> Int
pairArea0 ((x, y), (x', y')) = (abs (x' - x) + 1) * (abs (y' - y) + 1)

pairs0 :: [Coord] -> [Rect]
pairs0 [] = []
pairs0 (c : cs) = map (c,) cs ++ pairs0 cs

readCoordsFile :: FilePath -> IO [Coord]
readCoordsFile file =
  readCoords . T.lines <$> TIO.readFile file

readCoords :: [T.Text] -> [Coord]
readCoords = map readCoord

readCoord :: T.Text -> Coord
readCoord coord_txt =
  case T.split (== ',') coord_txt of
    [x, y] -> (readInt x, readInt y)
    _ -> error "parse"

readInt :: T.Text -> Int
readInt = either error fst . decimal
