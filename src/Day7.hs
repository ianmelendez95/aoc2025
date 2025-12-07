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
    uncons,
    unsnoc,
    find
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

soln :: FilePath -> IO Int
soln file = do
  (start_line : splitter_lines) <- T.lines <$> TIO.readFile file
  let start = readStart start_line
      splitters = map readSplitters splitter_lines
  putStrLn $ "Start: " ++ show start
  putStr "Splitters"
  mapM_ print splitters
  pure 0

readStart :: T.Text -> Int
readStart =
  maybe (error "uncons") fst
    . find ((== 'S') . snd)
    . zip [0 ..]
    . T.unpack

readSplitters :: T.Text -> S.Set Int
readSplitters = S.fromList . map fst . filter ((== '^') . snd) . zip [0..] . T.unpack

readInt :: T.Text -> Int
readInt = either error fst . decimal
