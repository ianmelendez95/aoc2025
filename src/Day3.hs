module Day3 
  ( soln, 
    numbers0,
    findMaxNum0
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

soln :: FilePath -> IO Int
soln file = do
  banks <- T.lines <$> TIO.readFile file
  pure $ sum (map (findMaxNum0 . T.unpack) banks)

findMaxNum0 :: [Char] -> Int
findMaxNum0 digits = maximum $ numbers0 digits

numbers0 :: [Char] -> [Int]
numbers0 [] = []
numbers0 (d:ds) = map (digitsToNum0 d) ds ++ numbers0 ds

digitsToNum0 :: Char -> Char -> Int
digitsToNum0 d1 d2 = fromMaybe (error $ show [d1, d2]) $ readMaybe [d1, d2]
