module Day8
  ( soln,
    Coord (..),
    Circuits (..),
    conPairs0,
    circuitSizes,
    coordDist0,
    nearCoord0,
    readCoordsFile,
    againstEach,
    pairCoords0,
    ascPairCoords0,
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

soln :: FilePath -> Int -> Int -> IO Int
soln file connection_n max_n = do
  coords <- readCoordsFile file 
  let pairs = ascPairCoords0 coords
      circuits@Circuits{circuitCount, circuitMap} = conPairs0 . take connection_n $ pairs
      circuit_sizes = circuitSizes circuits 
      circuit_sizes_desc = map snd . sortBy (comparing Down) . M.elems $ circuit_sizes
      max_circuit_sizes = (map take max_n $ circuit_sizes_desc)

      result :: Int
      result = product . take max_n $ circuit_sizes_desc

  pure result

readCoordsFile :: FilePath -> IO [Coord]
readCoordsFile file = do 
  ls <- T.lines <$> TIO.readFile file
  pure $ map readCoord ls

data Coord = Coord Int Int Int deriving (Show, Eq, Ord)

data Circuits = Circuits {
  circuitCount :: Int,
  circuitMap :: M.Map Coord Int
}

emptyCircuits :: Circuits
emptyCircuits = Circuits 0 M.empty

coordCircuits :: (Coord, Coord) -> Circuits -> (Maybe Int, Maybe Int)
coordCircuits (c1, c2) Circuits{circuitMap} = (M.lookup c1 circuitMap, M.lookup c2 circuitMap)

connectCoord :: Coord -> Int -> Circuits -> Circuits
connectCoord coord circuit cir@(Circuits{circuitMap}) = cir{circuitMap = M.insert coord circuit circuitMap}

mergeCircuits :: Int -> Int -> Circuits -> Circuits
mergeCircuits circuit1 circuit2 circuits@Circuits{circuitMap = c_map} =
  circuits{circuitMap = M.map (\c -> if c == circuit2 then circuit1 else c) c_map}

circuitSizes :: Circuits -> M.Map Int Int
circuitSizes = M.foldr mergeCount M.empty . circuitMap
  where 
    mergeCount :: Int -> M.Map Int Int -> M.Map Int Int
    mergeCount circuit = M.insertWith (+) circuit 1

newCircuit :: Coord -> Coord -> Circuits -> Circuits
newCircuit coord1 coord2 Circuits{circuitCount, circuitMap} = 
  let new_circuit = circuitCount + 1
   in Circuits new_circuit (M.insert coord1 new_circuit (M.insert coord2 new_circuit circuitMap))

conPairs0 :: [(Coord, Coord)] -> Circuits
conPairs0 = foldl' conCoords0 emptyCircuits 

conCoords0 :: Circuits -> (Coord, Coord) -> Circuits
conCoords0 circuits pair@(coord1, coord2) =  
  case coordCircuits pair circuits of 
    (Nothing, Nothing) -> newCircuit coord1 coord2 circuits
    -- both connected, merge circuits!
    (Just circuit1, Just circuit2) -> mergeCircuits circuit1 circuit2 circuits
    -- first one connected, connect the second one
    (Just circuit1, Nothing) -> connectCoord coord2 circuit1 circuits
    (Nothing, Just circuit2) -> connectCoord coord1 circuit2 circuits

ascPairCoords0 :: [Coord] -> [(Coord, Coord)]
ascPairCoords0 = map snd . sortBy (comparing fst) . pairCoords0

pairCoords0 :: [Coord] -> [(Double, (Coord, Coord))]
pairCoords0 [] = []
pairCoords0 (c:cs) = map pairCoord cs ++ pairCoords0 cs
  where 
    pairCoord :: Coord -> (Double, (Coord, Coord))
    pairCoord c' = (coordDist0 c c', (c, c'))

-- pairCoords0 :: [Coord] -> [(Int, (Coord, Coord))]
-- pairCoords0 = againstEach pairCoord
--   where 
--     pairCoord :: Coord -> [Coord] -> [(Int, (Coord, Coord))]
--     pairCoord c cs = (\c' -> ) $ nearCoord0 c cs

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
