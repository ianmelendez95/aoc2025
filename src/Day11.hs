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

data TravD = TravD
  { visited :: M.Map T.Text Int,
    graph :: Graph
  }

type TravS = State TravD

initTravD :: Graph -> TravD
initTravD graph = 
  TravD 
    { visited = M.singleton "you" 0,
      graph
    }

nextNodes0 :: T.Text -> TravS ([T.Text], Int)
nextNodes0 cur_node = gets nextNodesS
  where
    nextNodesS :: TravD -> ([T.Text], Int)
    nextNodesS TravD {visited, graph} =
      let next_nodes = graph M.! cur_node
          unvisited_nodes = filter (not . (`M.member` visited)) next_nodes
          visited_sum = sum $ mapMaybe (`M.lookup` visited) next_nodes 
       in (unvisited_nodes, visited_sum)

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let graph = readGraph t_lines
  pure $ evalState (travNodes0 "you") (initTravD graph)

travNodes0 :: T.Text -> TravS Int
travNodes0 "out" = pure 1
travNodes0 cur_node = do
  (unvisited_nodes, visited_sum) <- traceWith (\next_nodes -> "traverse: " ++ show (cur_node, next_nodes)) <$> nextNodes0 cur_node
  unvisited_sum <- sum <$> mapM travNodes0 unvisited_nodes
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
