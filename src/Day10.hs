{-# LANGUAGE PatternSynonyms #-}

module Day10
  ( soln,
    Mach (..),
    pMachine,
    machToZ3,
    solveMach,
    simulatePresses,
    simPress
  )
where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.State
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
import Data.SBV
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

data Mach = Mach Lights [Button] Joltage deriving (Show, Eq)

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  let machs = map readMachine t_lines
  sum <$> mapM solveMach machs

solveMach :: Mach -> IO Int
solveMach mach = do
  let c_set = machToZ3 mach
  opt_result <- optimizeWith (z3 {verbose = False}) Lexicographic c_set
  -- print opt_result
  let verified_result = verifyResult mach opt_result
  pure $ getResult verified_result

verifyResult :: Mach -> OptimizeResult -> OptimizeResult
verifyResult mach@(Mach _ buttons expect_joltages) opt_result = 
  let result_btns = getResultBtnPresses (length buttons) opt_result
      result_final_joltages = simulatePresses mach result_btns 
   in if result_final_joltages /= expect_joltages
        then error $ "ERROR - invalid solution: mach='" ++ show mach ++ "' soln:\n" ++ show opt_result
        else opt_result

simulatePresses :: Mach -> [Int] -> [Int]
simulatePresses (Mach _ buttons expect_joltages) press_counts = 
  let initial_joltages = replicate (length expect_joltages) 0
   in foldl' foldPresses initial_joltages $ zip press_counts buttons
  where
    foldPresses :: [Int] -> (Int, Button) -> [Int]
    foldPresses cur_joltages (press_count, btn_set) = 
      simPress press_count btn_set cur_joltages

simPress :: Int -> Button -> [Int] -> [Int]
simPress press_count btn_set cur_joltages =
  zipWith (+) (map (\j_idx -> if j_idx `S.member` btn_set then press_count else 0) [0..(length cur_joltages - 1)]) cur_joltages

getResultBtnPresses :: Int -> OptimizeResult -> [Int]
getResultBtnPresses btn_length opt_result = map ((`getResultVar` opt_result) . ('b' :) . show) [0 .. (btn_length - 1)]

getResult :: OptimizeResult -> Int
getResult = getResultVar "presses"

getResultVar :: String -> OptimizeResult -> Int
getResultVar var_name (LexicographicResult smt_result) =
  maybe (error "UNSAT") integerToInt . getModelValue var_name $ smt_result
  where
    integerToInt :: Integer -> Int
    integerToInt = fromIntegral
getResultVar _ _ = error "UNSAT"

machToZ3 :: Mach -> ConstraintSet
machToZ3 (Mach _ buttons joltages) = do
  btns_with_symbols <- zipWithM (\i b -> ((b,) <$>) . sInteger . ("b" ++) . show $ i) [0 .. (length buttons - 1)] buttons
  let btn_symbols = map snd btns_with_symbols
  mapM (constrain . (.>= 0)) btn_symbols
  zipWithM (\i j -> constrain . (.>= (literal (toInteger j))) . foldr1 (+) . map snd . filter ((S.member i) . fst) $ btns_with_symbols) [0 .. (length joltages - 1)] joltages
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
