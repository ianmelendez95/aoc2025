module Day1 where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List.Split (chunksOf)
import Control.Concurrent
import Control.Concurrent.Async (withAsync, wait, mapConcurrently)
import Control.Exception (evaluate)

solnAsync :: FilePath -> IO Int
solnAsync file = withAsync (soln file) wait

soln :: FilePath -> IO Int
soln file = do 
  ranges <- readLine <$> TIO.readFile file
  num_threads <- getNumCapabilities
  putStrLn $ "I've got " ++ show num_threads ++ " threads, and I'll use " ++ show num_threads ++ " threads!"
  let range_chunks = chunksOf (length ranges `div` num_threads) ranges
  -- chunk_sums :: [[Int]]
  print $ map length range_chunks
  sum <$> mapConcurrently (evaluate . sum . map sumRepeats) range_chunks
  -- withAsync (evaluate . sum $ map sumRepeats ranges1) $ \s1P -> do
  --    withAsync (evaluate . sum $ map sumRepeats ranges2) $ \s2P -> do
  --      s1 <- wait s1P
  --      s2 <- wait s2P
  --      pure $ s1 + s2

printAndReturn :: Int -> IO Int
printAndReturn x = print x >> pure x
  
      
  -- chunk_sums <- mapConcurrently (pure . map sumRepeats) range_chunks
  -- print $ sum $ map sum chunk_sums
  -- sumRepeatsInTxt <$> TIO.readFile file

sumRepeatsInTxt :: T.Text -> Int
sumRepeatsInTxt content = 
  let ranges = readLine content
   in sum $ map sumRepeats ranges

sumRepeats :: (Int, Int) -> Int
sumRepeats (s, e) = sum $ filter isRepeat [s..e]

isRepeat :: Int -> Bool
isRepeat = isRepeatChars . show

isRepeatChars :: [Char] -> Bool
isRepeatChars cs = any (checkRepeats cs) (substrings cs)

checkRepeats :: [Char] -> [Char] -> Bool
checkRepeats str sub_str = all (sub_str ==) $ chunksOf (length sub_str) str

substrings :: [Char] -> [[Char]]
substrings str = 
  let len = length str
      divisors = filter (\n -> len `mod` n == 0) [1..(len `div` 2)]
   in map (`take` str) divisors

readLine :: T.Text -> [(Int, Int)]
readLine text = map splitDelim $ T.split (==',') text
  where 
    splitDelim :: T.Text -> (Int, Int)
    splitDelim range_txt = 
      case T.split (=='-') range_txt of 
        [start, end] -> (readInt start, readInt end)
        split -> error $ show range_txt ++ show split

readInt :: T.Text -> Int
readInt = either error fst . decimal
