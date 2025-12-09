module Day9
  ( soln,
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
    minimumBy
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
  content <- TIO.readFile file
  let coords = map readCoord $ T.lines content
  mapM_ print coords
  pure 0

readCoord :: T.Text -> Coord
readCoord coord_txt = 
  case T.split (==',') coord_txt of 
    [x, y] -> (readInt x, readInt y)
    _ -> error "parse"

readInt :: T.Text -> Int
readInt = either error fst . decimal
