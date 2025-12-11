{-# LANGUAGE PatternSynonyms #-}

module Day10
  ( soln,
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
import Data.Text (pattern (:<), pattern (:>))
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Debug.Trace (trace, traceShowId, traceWith)
import Text.Read (readMaybe)
import Data.Void
import Parse qualified as P

type Lights = S.Set Int
type Button = S.Set Int

data Mach = Mach Lights [Button]

soln :: FilePath -> IO Int
soln file = do
  content <- TIO.readFile file
  print content
  pure 0

readMachine :: T.Text -> Mach
readMachine txt = 
  case T.span (/= ']') . T.drop 1 $ txt of 
    (T.Empty, _) -> error "parse nothing"
    (_, T.Empty) -> error "parse rest nothing"
    (lights_txt, _ :< rest_txt) -> 
      undefined

-- readLights :: T.Text -> S.Set Int
-- readLights lights_txt = T.map

pMachine :: P.Parser Mach
pMachine = Mach <$> pLights <*> P.some pButton

pButton :: P.Parser Button 
pButton = S.fromList <$> P.between (P.char '(') (P.char ')') (P.sepBy1 P.decimal (P.symbol ","))

pLights :: P.Parser Lights
pLights = do
  light_states <- P.between (P.char '[') (P.char ']') $ P.some pLight
  pure . S.fromList $ map fst . filter snd . zip [1..] $ light_states

pLight :: P.Parser Bool
pLight = (== '#') <$> P.satisfy (\c -> c == '.' || c == '#')

readInt :: T.Text -> Int
readInt = either error fst . decimal


























