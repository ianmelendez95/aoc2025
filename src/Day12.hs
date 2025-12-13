{-# LANGUAGE PatternSynonyms #-}

module Day12
  ( soln,
    Region (..),
    pInput,
    pShape,
    pRegion
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
import Data.Functor
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

type Shape = Int
data Region = Region Int [Int] deriving Show

soln :: FilePath -> IO Int
soln file = do
  t_lines <- T.lines <$> TIO.readFile file
  mapM_ TIO.putStrLn t_lines
  pure 0

pInput :: P.Parser ([Shape], [Region])
pInput = liftM2 (,) (P.many (P.try pShape)) (P.many (P.lexeme pRegion))

pShape :: P.Parser Shape
pShape = do 
  _ <- P.lexeme $ (P.decimal :: P.Parser Int) >> P.char ':'
  shape_lines <- P.some (P.lexeme pShapeLine) 
  pure $ shapeLinesArea shape_lines
  where 
    shapeLinesArea :: [T.Text] -> Int
    shapeLinesArea = sum . map (T.length . T.filter (== '#'))

    pShapeLine :: P.Parser T.Text
    pShapeLine = P.takeWhile1P Nothing (\c -> c == '#' || c == '.')

pRegion :: P.Parser Region
pRegion = do
  r_area <- P.lexeme $ liftA2 (*) (P.decimal <* P.char 'x') (P.decimal <* P.char ':') 
  Region r_area <$> P.some (P.hlexeme P.decimal) <* ((P.eol $> ()) P.<|> P.eof)


-- readInput :: [T.Text] -> ([Shape], [])
-- readInput input_txt = undefined
--   where 
--     parseShapes :: ([Shape], [T.Text]) -> ([Shape], [T.Text])
--     parseShapes state@(shapes, next_input) = 
--       case break (== "") next_input of 
--         ([], _) -> state
--         (shape_txt, rest_input) -> (readShape shape_txt : shapes, rest_input)
--
--     readShape :: [T.Text] -> Shape
--     readShape = sum . concatMap (T.length . T.filter (== '#'))
--
--     scanShapes :: ([[T.Text]], [T.Text]) -> [T.Text] -> ([[T.Text]], [T.Text])
--     scanShapes (shapes, next_input) = 
--       case scanShape next_input of 
--         Nothing -> ([], next_input)
--         Just (shape, rest_input) -> scanShapes (shape : shapes, rest_input)
--
--     isIndexLine :: T.Text -> Bool
--     isIndexLine txt = (not . ("x" `T.isInfixOf` txt) && ":" )


readInt :: T.Text -> Int
readInt = either error fst . decimal
