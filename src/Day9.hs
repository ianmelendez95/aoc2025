module Day9
  ( soln,
    pairArea0
  )
where

import Control.Applicative (liftA2)
import Data.List
  ( intercalate,
    intersperse,
    isPrefixOf,
    sortBy,
    transpose,
    uncons,
    unsnoc,
    find,
    foldl',
    minimumBy,
    sort,
    maximumBy
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

soln :: FilePath -> IO Int
soln file = do
  coords <- readCoordsFile file
  let max_area = bigRect0 . pairs0 $ coords
  mapM_ print coords
  pure max_area

bigRect0 :: [(Coord, Coord)] -> Int
bigRect0 = maximum . map pairArea0

pairArea0 :: (Coord, Coord) -> Int
pairArea0 ((x, y), (x', y')) = (abs (x' - x) + 1) * (abs (y' - y) + 1) 

pairs0 :: [Coord] -> [(Coord, Coord)]
pairs0 [] = []
pairs0 (c:cs) = map (c,) cs ++ pairs0 cs

readCoordsFile :: FilePath -> IO [Coord]
readCoordsFile file = 
  readCoords . T.lines <$> TIO.readFile file

readCoords :: [T.Text] -> [Coord]
readCoords = sort . map readCoord

readCoord :: T.Text -> Coord
readCoord coord_txt = 
  case T.split (==',') coord_txt of 
    [x, y] -> (readInt x, readInt y)
    _ -> error "parse"

readInt :: T.Text -> Int
readInt = either error fst . decimal
