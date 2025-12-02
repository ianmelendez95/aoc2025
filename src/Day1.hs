module Day1 where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read

readInput :: IO T.Text
readInput = TIO.readFile "src/Day1/short.txt" -- 
-- readInput = TIO.readFile "src/Day1/full.txt" -- 

soln :: IO ()
soln = do
  content <- readInput
  let ranges = readLine content
  -- accum_sum = scanl moveDial 50 nums
  -- print nums
  -- print accum_sum
  -- TIO.putStrLn content
  mapM_ (\(start, end) -> print $ take 5 [start..end]) ranges

isRepeat :: Int -> Bool
isRepeat = undefined

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
