module Day7
  ( soln,
    collide,
    splitBeam,
    splitUnivs,
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
    foldl'
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

type Beams = S.Set Int

type Beam = Int

type Splitter = Int

type Count = Int

type Splitters = S.Set Int

type Univs = M.Map Beam Count

soln :: FilePath -> IO Int
soln file = do
  (start_line : splitter_lines) <- T.lines <$> TIO.readFile file
  let start = readStart start_line
      origin_univ = M.singleton start 1
      splitters = map readSplitters splitter_lines
      final_beams = traverseUnivs origin_univ splitters
      final_count = M.foldr (+) 0 final_beams
  -- putStrLn $ "Start: " ++ show start
  -- putStr "Splitters"
  -- mapM_ print splitters
  pure final_count

traverseUnivs :: M.Map Beam Count -> [Splitters] -> M.Map Beam Count
traverseUnivs = foldl' splitUnivs

splitUnivs :: M.Map Beam Count -> Splitters -> M.Map Beam Count
splitUnivs univs splitters = 
  M.fromAscListWith (+) . sortBy (comparing fst) . concatMap (splitBeam splitters) . M.toList $ univs

splitBeam :: Splitters -> (Beam, Count) -> [(Beam, Count)]
splitBeam splitters b@(beam, count) = 
  let is_hit = S.member beam splitters
   in if not is_hit
        then [b]
        else [(beam - 1, count), (beam + 1, count)]

collide :: (Beams, Int) -> Beams -> (Beams, Int)
collide (beams, colls) splitters = 
  let misses = S.difference beams splitters -- these stay

      hits = S.intersection beams splitters
      new_beams = S.foldl' collectSplits misses hits
   in (new_beams, colls + length hits)
  where 
    collectSplits :: Beams -> Int -> Beams
    collectSplits c_beams hit = S.union c_beams (S.fromList [hit - 1, hit + 1])

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
