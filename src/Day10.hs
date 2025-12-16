{-# LANGUAGE PatternSynonyms #-}

module Day10
  ( soln,
    Mach (..),
    evalMachine,
    pMachine,
    pressButton0,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.State
import Control.Monad
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

type Joltage = [Int]

type Univ = (Joltage, Int)

data Mach = Mach Lights [Button] Joltage deriving (Show, Eq)

data MS = MS
  { univs :: Seq.Seq Univ,
    buttons :: [Button],
    expected :: Joltage
  }

type M = State MS

mkMS :: Mach -> MS
mkMS (Mach _ buttons joltage) =
  MS
    { univs = Seq.singleton (replicate (length joltage) 0, 0),
      buttons,
      expected = joltage
    }

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let machs = map readMachine t_lines
  sum <$> zipWithM evalMachineM [(1 :: Int)..] machs

evalMachineM :: Int -> Mach -> IO Int
evalMachineM eval_n mach = do 
  let res = evalMachine mach
  putStrLn ("Result " ++ show eval_n ++ ": " ++ show res)
  pure res

evalMachine :: Mach -> Int
evalMachine machine = evalState travM (mkMS machine)

travM :: M Int
travM = do
  (cur_joltage, cur_presses) <- curJoltage
  btns <- gets buttons
  expected <- gets expected
  let next_joltages = map (pressButton0 cur_joltage) btns
      next_presses = 1 + cur_presses
      next_univs = map (, next_presses) next_joltages
  if expected `elem` next_joltages
  then pure next_presses
  else do 
    modify $ \ms@MS{univs} -> ms{univs = foldl' (Seq.|>) univs next_univs}
    travM

curJoltage :: M (Joltage, Int)
curJoltage = do 
  cur_univs <- gets univs
  case Seq.viewl cur_univs of 
    (cur_univ Seq.:< rest_univs) -> do 
      modify $ \ms -> ms{univs = rest_univs}
      pure cur_univ
    _ -> error "no more univs!!"

-- let all_presses = map (\b -> buttonPresss0 expected (pressButton0 b) buttons) buttons
--  in undefined

pressButton0 :: Joltage -> Button -> Joltage
pressButton0 cur_joltage btn_joltages = 
  zipWith (\i jolts -> if i `S.member` btn_joltages then jolts + 1 else jolts) [0..] cur_joltage

readMachine :: T.Text -> Mach
readMachine = P.parse pMachine

pMachine :: P.Parser Mach
pMachine = Mach <$> P.lexeme pLights <*> P.some (P.lexeme pButton) <*> pJoltage

pButton :: P.Parser Button
pButton = S.fromList <$> P.between (P.char '(') (P.char ')') (P.sepBy1 P.decimal (P.symbol ","))

pLights :: P.Parser Lights
pLights = do
  light_states <- P.between (P.char '[') (P.char ']') $ P.some pLight
  pure . S.fromList $ map fst . filter snd . zip [0 ..] $ light_states

pJoltage :: P.Parser Joltage
pJoltage = P.between (P.char '{') (P.char '}') $ P.sepBy P.decimal (P.char ',')

pLight :: P.Parser Bool
pLight = (== '#') <$> P.satisfy (\c -> c == '.' || c == '#')

readInt :: T.Text -> Int
readInt = either error fst . decimal
