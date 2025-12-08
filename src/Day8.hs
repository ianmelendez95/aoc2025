module Day8
  ( soln,
    Coord (..),
    coordDist0,
    nearCoord0,
    readCoordsFile,
    againstEach
  )
where

import Control.Applicative (liftA2)
import Data.List
  ( intercalate,
    intersperse,
    isPrefixOf,
    sortBy,
    transpose,
    uncons,
    unsnoc,
    find,
    foldl',
    minimumBy
  )
import Data.List.Split (chunksOf, splitEvery)
import Data.Map qualified as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
    listToMaybe,
    maybe,
  )
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read
import Debug.Trace (trace, traceShowId, traceWith)
import Text.Read (readMaybe)

data Coord = Coord Int Int Int deriving (Show, Eq)

type CoordCircs = M.Map Coord Int

soln :: FilePath -> IO Int
soln file = do
  coords <- readCoordsFile file 
  mapM_ print coords
  pure 0

readCoordsFile :: FilePath -> IO [Coord]
readCoordsFile file = do 
  ls <- T.lines <$> TIO.readFile file
  pure $ map readCoord ls

conCoords0 :: CoordCircs -> Coord -> [Coord] -> CoordCircs
conCoords0 circs cs = undefined

pairCoords0 :: [Coord] -> [(Coord, Coord)]
pairCoords0 = againstEach pairCoord
  where 
    pairCoord :: Coord -> [Coord] -> (Coord, Coord)
    pairCoord c cs = (c,) $ nearCoord0 c cs

againstEach :: forall a b. (a -> [a] -> b) -> [a] -> [b]
againstEach f = go [] 
  where 
    go :: [a] -> [a] -> [b]
    go _ [] = []
    go xs' (x:xs) = f x (xs' ++ xs) : go (x : xs') xs

nearCoord0 :: Coord -> [Coord] -> Coord
nearCoord0 c = minimumBy (comparing (coordDist0 c))

coordDist0 :: Coord -> Coord -> Double
coordDist0 (Coord x y z) (Coord x' y' z') = 
  sqrt . sum . map square $ zipWith (-) [x, y, z] [x', y', z']

square :: Int -> Double
square = (** 2) . fromIntegral

readCoord :: T.Text -> Coord
readCoord line = 
  case map readInt . T.split (== ',') $ line of 
    [x, y, z] -> Coord x y z
    _ -> error $ "parse: " ++ show line

readInt :: T.Text -> Int
readInt = either error fst . decimal
