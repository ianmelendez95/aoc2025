module Day9
  ( soln,
    pairArea0,
  )
where

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

type Coord = (Int, Int)

data Edge
  = HEdge Int Int Int
  | VEdge Int Int Int
  deriving (Show, Eq, Ord)

soln :: FilePath -> IO Int
soln file = do
  coords <- readCoordsFile file
  let max_area = bigRect0 . pairs0 $ coords
      edges = coordEdges0 coords
  putStrLn $ "len: " ++ show (length edges)
  mapM_ print edges
  pure max_area

coordEdges0 :: [Coord] -> [Edge]
coordEdges0 [] = []
coordEdges0 [_] = []
coordEdges0 (c1@(x1, y1) : c2@(x2, y2) : cs)
  | x1 == x2 = VEdge x1 (min y1 y2) (max y1 y2) : coordEdges0 (c2 : cs)
  | y1 == y2 = HEdge y1 (min x1 x2) (max x1 x2) : coordEdges0 (c2 : cs)
  | otherwise = error $ "not straight: " ++ show (c1, c2)

bigRect0 :: [(Coord, Coord)] -> Int
bigRect0 = maximum . map pairArea0

pairArea0 :: (Coord, Coord) -> Int
pairArea0 ((x, y), (x', y')) = (abs (x' - x) + 1) * (abs (y' - y) + 1)

pairs0 :: [Coord] -> [(Coord, Coord)]
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
