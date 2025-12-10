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

data Edges = Edges {
  vEdges :: [Edge],
  hEdges :: [Edge]
}

soln :: FilePath -> IO Int
soln file = do
  coords <- readCoordsFile file
  let max_area = bigRect0 . pairs0 $ coords
      all_edges = coordEdges0 coords
      Edges{vEdges, hEdges} = reduceEdges0 all_edges
  putStrLn "\n--- Edges:"
  mapM_ print all_edges
  putStrLn "\n--- Vert Edges: "
  mapM_ print vEdges
  putStrLn "\n--- Horiz Edges: "
  mapM_ print hEdges
  pure max_area

reduceEdges0 :: [Edge] -> Edges
reduceEdges0 es = 
  let Edges{vEdges, hEdges} = foldl' mergeEdges0 (Edges [] []) es
   in Edges (sort vEdges) (sort hEdges)

mergeEdges0 :: Edges -> Edge -> Edges
mergeEdges0 es@Edges{vEdges} e@VEdge{} = es{vEdges = e : vEdges}
mergeEdges0 es@Edges{hEdges} e@HEdge{} = es{hEdges = e : hEdges}

coordEdges0 :: [Coord] -> [Edge]
coordEdges0 = zipPairs0 coordEdge0 

zipPairs0 :: (Coord -> Coord -> Edge) -> [Coord] -> [Edge]
zipPairs0 _ [] = error "no coords"
zipPairs0 f cs@(c:cs_tail) = zipWith f cs (cs_tail ++ [c])

coordEdge0 :: Coord -> Coord -> Edge
coordEdge0 c1@(x1, y1) c2@(x2, y2)
  | x1 == x2 = VEdge x1 (min y1 y2) (max y1 y2)
  | y1 == y2 = HEdge y1 (min x1 x2) (max x1 x2)
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
