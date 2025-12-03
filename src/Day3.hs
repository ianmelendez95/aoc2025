module Day3 where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Data.List (isPrefixOf)
import Data.List.Split (chunksOf)

soln :: FilePath -> IO Int
soln file = do
  content <- TIO.readFile file
  TIO.putStr content
  pure 0


