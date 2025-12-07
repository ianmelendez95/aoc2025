module Day7
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
    unsnoc
  )
import Data.List.Split (chunksOf, splitEvery)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Debug.Trace (trace, traceShowId, traceWith)
import Text.Read (readMaybe)

soln :: FilePath -> IO Int
soln file = do
  ls <- T.lines <$> TIO.readFile file
  print ls
  pure 0

readInt :: T.Text -> Int
readInt = either error fst . decimal
