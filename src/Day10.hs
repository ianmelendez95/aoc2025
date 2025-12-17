{-# LANGUAGE PatternSynonyms #-}

module Day10
  ( soln,
    Mach (..),
    pMachine,
    machToZ3,
    solveMach
  )
where

import Control.Applicative (liftA2)
import Control.Monad.State
import Control.Monad
import Data.List
  ( find,
    foldl',
    foldr1,
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
import Data.SBV

type Lights = S.Set Int

type Button = S.Set Int

type Joltage = [Int]

data Mach = Mach Lights [Button] Joltage deriving (Show, Eq)

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let machs = map readMachine t_lines
  sum <$> mapM solveMach machs

solveMach :: Mach -> IO Int
solveMach mach = do 
  let c_set = machToZ3 mach
  -- print c_set
  opt_result <- optimizeWith (z3{verbose = True}) Lexicographic c_set
  -- print opt_result
  pure $ getResult opt_result

verifyPresses :: Mach -> SMTResult -> SMTResult
verifyPresses (Mach _ buttons expect_joltages) = undefined

getResult :: OptimizeResult -> Int
getResult (LexicographicResult smt_result) = 
  maybe (error "UNSAT") fromIntegral . getModelValue "presses" $ smt_result

machToZ3 :: Mach -> ConstraintSet
machToZ3 (Mach _ buttons joltages) = do
  btns_with_symbols <- zipWithM (\i b -> ((b,) <$>) . sInteger . ("b" ++) . show $ i) [0..(length buttons - 1)] buttons
  let btn_symbols = map snd btns_with_symbols
  mapM (constrain . (.>= 0)) btn_symbols
  zipWithM (\i j -> constrain . (.>= (literal (toInteger j))) . foldr1 (+) . map snd . filter ((S.member i) . fst) $ btns_with_symbols) [0..(length joltages - 1)] joltages
  minimize "presses" . foldr1 (+) $ btn_symbols

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
