{-# LANGUAGE PatternSynonyms #-}

module Day11
  ( soln,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.State
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
import Data.Ord (Down (..), comparing)
import Data.Sequence (Seq, pattern (:<|), pattern (:|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (pattern (:<), pattern (:>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.Void
import Debug.Trace (trace, traceShowId, traceWith)
import Parse qualified as P
import Text.Read (readMaybe)

type Graph = M.Map T.Text [T.Text]

type Hits = (Int, Int, Int) -- (dac, fft, out)

data TravD = TravD
  { visited :: M.Map T.Text Int,
    graph :: Graph
  }

type TravS = State TravD

initTravD :: T.Text -> Graph -> TravD
initTravD start graph = 
  TravD 
    { visited = M.singleton start 0,
      graph
    }

nextNodes0 :: T.Text -> TravS ([T.Text], Int)
nextNodes0 cur_node = gets nextNodesS
  where
    nextNodesS :: TravD -> ([T.Text], Int)
    nextNodesS TravD {visited, graph} =
      let next_nodes = fromMaybe (error $ "no member" ++ show cur_node) $ M.lookup cur_node graph -- graph M.! cur_node
          unvisited_nodes = filter (not . (`M.member` visited)) next_nodes
          visited_sum = sum $ mapMaybe (`M.lookup` visited) next_nodes 
       in (unvisited_nodes, visited_sum)

soln :: T.Text -> [T.Text] -> FilePath -> IO Int
soln start search_nodes file = do
  t_lines <- T.lines <$> TIO.readFile file
  let graph = readGraph t_lines
  pure $ evalState (travNodes0 (S.fromList search_nodes) start) (initTravD start graph)

travNodes0 :: Set T.Text -> T.Text -> TravS Int
travNodes0 search_nodes "out" = pure $ if S.null search_nodes then 1 else 0
travNodes0 search_nodes cur_node 
  | cur_node `S.member` search_nodes = travNodes0 (S.delete cur_node search_nodes) (trace ("hit: " ++ show (cur_node, search_nodes)) cur_node)
  | otherwise = do
      (unvisited_nodes, visited_sum) <- traceWith (\next -> "next: " ++ show (cur_node, search_nodes, next)) <$> nextNodes0 cur_node
      unvisited_sum <- sum <$> mapM (travNodes0 search_nodes) unvisited_nodes
      let cur_node_sum = visited_sum + unvisited_sum
      modify (\t@TravD{visited} -> t{visited = M.insert cur_node cur_node_sum visited})
      pure cur_node_sum

readGraph :: [T.Text] -> Graph
readGraph txt_lines = 
  let nodes = map readLine txt_lines
   in M.fromList nodes

readLine :: T.Text -> (T.Text, [T.Text])
readLine txt_line =
  case T.words txt_line of
    (start_word : nexts) ->
      case T.unsnoc start_word of
        Just (start, _) -> (start, nexts)
        Nothing -> error $ "unsnoc" ++ show start_word
    _ -> error $ "words" ++ show txt_line

readInt :: T.Text -> Int
readInt = either error fst . decimal
