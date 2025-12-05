module Day3 
  ( soln, 
    numbers0,
    findMaxNum0,
    takeAny0,
    dodecNums0,
    dodecMax0,
    dodecMax1,
    buildBigNum1,
    buildBigNum1',
    charsWithPos1,
    withEach0,
    buildBigNumFinal1
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.Ord (Down (..), comparing)

import Debug.Trace (trace)

soln :: FilePath -> IO Int
soln file = do
  banks <- T.lines <$> TIO.readFile file
  pure $ sum (map (dodecMax0 . T.unpack) banks)

dodecMax1 :: [Char] -> Int
dodecMax1 = buildBigNumFinal1 12 . charsWithPos1

buildBigNumFinal1 :: Int -> [(Int, Char)] -> Int
buildBigNumFinal1 n cs = 
  let big_nums = concatMap (\c -> buildBigNum1' (n - 1) c cs) cs
   in case big_nums of 
        [] -> error "No soln"
        (big_num:_) -> read big_num

buildBigNum1' :: Int -> (Int, Char) -> [(Int, Char)] -> [String]
buildBigNum1' 0 (_, c) _ = [[c]]
buildBigNum1' _ _ [] = []
buildBigNum1' n (c_n, c) cs = 
  let cs_after_c = filter (\(c_n', _) -> c_n > c_n') cs
      cs_with_enough = filter (\(c_n', _) -> c_n' >= (n - 1)) cs_after_c
      lower_digit_sets :: [String]
      lower_digit_sets = concatMap (\c' -> buildBigNum1' (n - 1) c' cs_after_c) cs_with_enough
   in (c:) <$> lower_digit_sets

{--
 - buildBigNum1' 2 (2, '1') [(0,'3'),(1,'2'),(2,'1')]
 - cs_after_c = [(0,'3'),(1,'2')]
 - cs_with_enough = 
 - --}


buildBigNum1 :: Int -> [(Int, Char)] -> Maybe String
buildBigNum1 0 _ = Just []
buildBigNum1 _ [] = Nothing
buildBigNum1 n ((c_n, c):cs) = 
  let cs_after_c = filter (\(c_n', _) -> c_n' < c_n) cs
      cs_with_enough = filter (\(c_n', _) -> c_n' >= (n - 1)) cs_after_c
   in case buildBigNum1 (n - 1) cs_with_enough of 
        Nothing -> Nothing
        Just big_boy -> Just (c : big_boy)

withEach0 :: forall a b. (a -> [a] -> Maybe b) -> [a] -> [b]
withEach0 f = catMaybes . go []
  where 
    go :: [a] -> [a] -> [Maybe b]
    go _ [] = []
    go ls (r:rs) = 
      let r' = f r (ls ++ rs) 
       in r' : go (r:ls) rs

traceLog :: Show a => a -> a
traceLog x = trace ("\nTRACE'" ++ show x ++ "'\n") x 

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
