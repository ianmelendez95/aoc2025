module Day1 where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf)

soln :: FilePath -> IO Int
soln file = sumRepeatsInTxt <$> TIO.readFile file

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
checkRepeats [] _ = True
checkRepeats str sub_str = 
  sub_str `isPrefixOf` str && checkRepeats sub_str (drop (length sub_str) str)

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
