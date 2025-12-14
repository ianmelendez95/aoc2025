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
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Sequence (Seq, pattern (:<|), pattern (:|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text, pattern (:<), pattern (:>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.Void
import Debug.Trace (trace, traceShowId, traceWith)
import Parse qualified as P
import Text.Read (readMaybe)

data Node = SVR | DAC | FFT | OUT deriving (Eq, Show, Ord)

type Graph = M.Map T.Text [T.Text]

type SVisited = Map (Set Node) Int

type Visited = Map Text SVisited

type Hits = (Int, Int, Int) -- (dac, fft, out)

data TravD = TravD
  { visited :: Visited,
    graph :: Graph
  }

type TravS = State TravD

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let graph = readGraph t_lines
      visited_sets = evalState (travNodes0 "svr") (initTravD graph)
      all_visited_count = visited_sets M.! S.fromList [FFT, DAC, OUT]
  pure all_visited_count

initTravD :: Graph -> TravD
initTravD graph =
  TravD
    { visited = M.singleton "svr" M.empty,
      graph
    }

travNodes0 :: Text -> TravS SVisited
travNodes0 "out" = pure $ M.singleton (S.singleton OUT) 1
travNodes0 cur_node =
  do
    (unvisited_nodes, visited_sets) <- nextNodes0 cur_node
    unvisited_sets <- mapM travNodes0 unvisited_nodes

    let child_sets = M.unionsWith (+) (visited_sets ++ unvisited_sets)
        cur_node_sets =
          case mNode cur_node of
            Nothing -> child_sets -- just the child sets
            -- this is a special node, so insert it into all the node sets
            Just spec_node -> M.mapKeys (S.insert spec_node) child_sets

    modify (\t@TravD {visited} -> t {visited = M.insert cur_node cur_node_sets visited})
    pure cur_node_sets

nextNodes0 :: T.Text -> TravS ([T.Text], [SVisited])
nextNodes0 cur_node = gets nextNodesS
  where
    nextNodesS :: TravD -> ([T.Text], [SVisited])
    nextNodesS TravD {visited, graph} =
      let next_nodes = fromMaybe (error $ "no member" ++ show cur_node) $ M.lookup cur_node graph -- graph M.! cur_node
          unvisited_nodes = filter (not . (`M.member` visited)) next_nodes
          visited_sets = mapMaybe (`M.lookup` visited) next_nodes
       in (unvisited_nodes, visited_sets)

mNode :: Text -> Maybe Node
mNode "out" = Just OUT
mNode "dac" = Just DAC
mNode "fft" = Just FFT
mNode _ = Nothing

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
