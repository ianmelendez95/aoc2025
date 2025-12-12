{-# LANGUAGE PatternSynonyms #-}

module Day10
  ( soln,
    Mach (..),
    pMachine,
    pressButton0,
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
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (pattern (:<), pattern (:>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.Void
import Debug.Trace (trace, traceShowId, traceWith)
import Parse qualified as P
import Text.Read (readMaybe)

type Lights = S.Set Int

type Button = S.Set Int

data Mach = Mach Lights [Button] deriving (Show, Eq)

data MS = MS
  { univs :: Seq.Seq (Lights, Int),
    buttons :: [Button],
    expected :: Lights
  }

type M = State MS

mkMS :: Mach -> MS
mkMS (Mach lights buttons) =
  MS
    { univs = Seq.singleton (S.empty, 0),
      buttons,
      expected = lights
    }

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let machs = map readMachine t_lines
  mapM_ print machs
  pure 0

-- stepM :: M
-- stepM = do 
--   cur_lights <- gets curLights
--   btns <- gets buttons
--   expected <- gets expected
--   let next_lights = map (pressButton0 cur_lights) btns
--   modify ()
--   if any (== expected) next_lights
--     then modify ()
--     else undefined

buttonPresses0 :: Lights -> Lights -> [Button] -> Int
buttonPresses0 expected current buttons
  | expected == current = 0
  | otherwise = undefined

-- let all_presses = map (\b -> buttonPresses0 expected (pressButton0 b) buttons) buttons
--  in undefined

pressButton0 :: Lights -> Button -> Lights
pressButton0 = S.symmetricDifference

readMachine :: T.Text -> Mach
readMachine = P.parse pMachine

pMachine :: P.Parser Mach
pMachine = Mach <$> P.lexeme pLights <*> P.some (P.lexeme pButton)

pButton :: P.Parser Button
pButton = S.fromList <$> P.between (P.char '(') (P.char ')') (P.sepBy1 P.decimal (P.symbol ","))

pLights :: P.Parser Lights
pLights = do
  light_states <- P.between (P.char '[') (P.char ']') $ P.some pLight
  pure . S.fromList $ map fst . filter snd . zip [0 ..] $ light_states

pLight :: P.Parser Bool
pLight = (== '#') <$> P.satisfy (\c -> c == '.' || c == '#')

readInt :: T.Text -> Int
readInt = either error fst . decimal
