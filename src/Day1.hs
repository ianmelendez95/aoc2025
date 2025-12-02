module Day1 where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf)
import Data.List.Split (chunksOf)
import Control.Concurrent.Async (withAsync, wait, mapConcurrently)

solnAsync :: FilePath -> IO Int
solnAsync file = withAsync (soln file) wait

soln :: FilePath -> IO Int
soln file = do 
  ranges <- readLine <$> TIO.readFile file
  let (ranges1, ranges2) = splitAt (length ranges `div` 2) ranges
  -- chunk_sums :: [[Int]]
  res <- withAsync (printAndReturn $ sum $ map sumRepeats ranges1) $ \s1P -> do
            putStrLn "TRACE: Started range 1"
            withAsync (printAndReturn $ sum $ map sumRepeats ranges2) $ \s2P -> do
              putStrLn "TRACE: Started range 2"
              s1 <- wait s1P
              s2 <- wait s2P
              putStrLn "TRACE: DONE!"
              pure $ s1 + s2
  putStrLn "TRACE: Actually done"
  pure res

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
