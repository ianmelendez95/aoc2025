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

type Graph = M.Map T.Text [T.Text]

data TravD = TravD
  { visited :: S.Set T.Text,
    graph :: Graph
  }

type TravS = State TravD

initTravD :: Graph -> TravD
initTravD graph = 
  TravD 
    { visited = S.singleton "you",
      graph
    }

nextNodes0 :: T.Text -> TravS [T.Text]
nextNodes0 cur_node = state nextNodesS
  where
    nextNodesS :: TravD -> ([T.Text], TravD)
    nextNodesS trav_d@TravD {visited, graph} =
      let next_nodes = filter (not . (`S.member` visited)) $ graph M.! cur_node
       in ( next_nodes,
            trav_d
              { visited = foldr (\n vs -> if n == "out" then vs else S.insert n vs) visited next_nodes
              }
          )

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let graph = readGraph t_lines
  pure $ evalState (travNodes0 "you") (initTravD graph)

travNodes0 :: T.Text -> TravS Int
travNodes0 "out" = pure 1
travNodes0 cur_node = do
  next_nodes <- traceWith (\next_nodes -> "traverse: " ++ show (cur_node, next_nodes)) <$> nextNodes0 cur_node
  sum <$> mapM travNodes0 next_nodes

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
