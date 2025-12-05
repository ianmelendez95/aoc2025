module Day3 
  ( soln, 
    numbers0,
    findMaxNum0,
    takeAny0,
    dodecNums0,
    dodecMax0,
    dodecMax1,
    dodecMax2,
    buildBigNum1,
    buildBigNum1',
    charsWithPos1,
    withEach0,
    buildBigNumFinal1,
    buildBigNum2
  ) 
  where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf, sortBy, maximumBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.Ord (Down (..), comparing)

import Debug.Trace (trace)

soln :: FilePath -> IO Int
soln file = do
  banks <- T.lines <$> TIO.readFile file
  pure $ sum (map (dodecMax1 . T.unpack) banks)

dodecMax2 :: [Char] -> Int
dodecMax2 = read . buildBigNum2 12

buildBigNum2 :: Int -> [Char] -> [Char]
buildBigNum2 n cs = 
  let next_possible = take (length cs - n + 1) cs
      (next_idx, next_digit) = maximumBy (comparing fst) . zip [(1 :: Int)..] $ next_possible
   in next_digit : drop next_idx cs

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
 - buildBigNum1' 2 (3, '8') [(0,'9'),(2,'8'),(3,'8'),(1,'1')]
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
charsWithPos1 = sortBy compareChars . zip [0..] . reverse
  where 
    compareChars :: (Int, Char) -> (Int, Char) -> Ordering
    compareChars c1 c2 = 
      comparing (Down . snd) c1 c2 <> comparing (Down . fst) c1 c2
      

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
