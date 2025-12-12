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
  ( fromJust,
    fromMaybe,
    listToMaybe,
    maybe,
  )
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

type Graph = M.Map T.Text (S.Set T.Text)

data TravD = TravD
  { visited :: S.Set T.Text,
    queue :: Seq (T.Text, Int),
    graph :: Graph
  }

type TravS = State TravD

initTravD :: Graph -> TravD
initTravD graph = 
  let init_nodes = graph M.! "you"
   in TravD 
        { visited = S.singleton "you",
          queue = Seq.fromList . map (,0) . S.toList $ init_nodes,
          graph
        }

curNode :: TravS (T.Text, Int)
curNode = state curNodeS
  where
    curNodeS :: TravD -> ((T.Text, Int), TravD)
    curNodeS t@TravD {queue} =
      case Seq.viewl queue of
        Seq.EmptyL -> error "queue empty!!"
        (n Seq.:< nodes) -> (n, t {queue = nodes})

nextNodes0 :: T.Text -> Int -> TravS (Set T.Text)
nextNodes0 cur_node cur_depth = state nextNodesS
  where
    nextNodesS :: TravD -> (Set T.Text, TravD)
    nextNodesS trav_d@TravD {visited, graph, queue} =
      let next_nodes = S.difference visited $ graph M.! cur_node
       in ( next_nodes,
            trav_d
              { visited = S.union visited next_nodes,
                queue = seqConcat queue (map (, cur_depth + 1) . S.toList $ next_nodes)
              }
          )

seqConcat :: Seq a -> [a] -> Seq a
seqConcat = foldl' (Seq.|>)

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let graph = readGraph t_lines
  pure $ evalState travNodes0 (initTravD graph)

travNodes0 :: TravS Int
travNodes0 = do
  (cur_node, cur_depth) <- curNode
  next_nodes <- nextNodes0 cur_node cur_depth
  if T.pack "out" `elem` next_nodes
    then pure cur_depth
    else travNodes0

readGraph :: [T.Text] -> Graph
readGraph txt_lines = 
  let nodes = map readLine txt_lines
   in M.fromList nodes

readLine :: T.Text -> (T.Text, Set T.Text)
readLine txt_line =
  case T.words txt_line of
    (start_word : nexts) ->
      case T.unsnoc start_word of
        Just (start, _) -> (start, S.fromList nexts)
        Nothing -> error $ "unsnoc" ++ show start_word
    _ -> error $ "words" ++ show txt_line

readInt :: T.Text -> Int
readInt = either error fst . decimal
