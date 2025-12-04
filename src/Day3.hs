module Day3 
  ( soln, 
    numbers0,
    findMaxNum0,
    takeAny0,
    dodecNums0,
    dodecMax0,
    charsWithPos1,
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.Ord (Down (..), comparing)

soln :: FilePath -> IO Int
soln file = do
  banks <- T.lines <$> TIO.readFile file
  pure $ sum (map (dodecMax0 . T.unpack) banks)

charsWithPos1 :: [Char] -> [(Int, Char)]
charsWithPos1 = sortBy (comparing (Down . snd)) . zip [0..] . reverse

findMaxNum0 :: [Char] -> Int
findMaxNum0 digits = maximum $ numbers0 digits

dodecMax0 :: String -> Int
dodecMax0 = maximum . dodecNums0

dodecNums0 :: String -> [Int]
dodecNums0 = map read . takeAny0 12

takeAny0 :: Int -> [Char] -> [String]
takeAny0 0 _ = [[]]
takeAny0 _ [] = [] -- ran out, so can't do anything
takeAny0 n (x:xs) = 
  --((x:) <$> takeAny0 (n - 1) xs) ++ takeAny0 n xs
  let with_x = (x:) <$> takeAny0 (n - 1) xs
      without_x = takeAny0 n xs
   in with_x ++ without_x

numbers0 :: [Char] -> [Int]
numbers0 [] = []
numbers0 (d:ds) = map (digitsToNum0 d) ds ++ numbers0 ds

digitsToNum0 :: Char -> Char -> Int
digitsToNum0 d1 d2 = fromMaybe (error $ show [d1, d2]) $ readMaybe [d1, d2]
