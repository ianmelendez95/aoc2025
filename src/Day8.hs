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

soln :: FilePath -> Int -> IO Int
soln file connection_n = do
  coords <- readCoordsFile file 
  let pairs = ascPairCoords0 coords
      init_circs = loneCircuits0 coords
      (Coord x1 _ _, Coord x2 _ _) = fromMaybe (error "No soln") $ connTillOne init_circs pairs
      -- circuits =  . take connection_n $ pairs
      -- circuit_sizes = circuitSizes circuits 
      -- circuit_sizes_desc = sortBy (comparing Down) . M.elems $ circuit_sizes
      -- big_three = take 3 circuit_sizes_desc
      -- max_circuit_sizes = (map take max_n $ circuit_sizes_desc)

      -- result :: Int
      -- result = product . take max_n $ circuit_sizes_desc

  -- putStrLn "--- Pair ---"
  -- mapM_ print pairs
  -- putStrLn $ "Circuits: " ++ show circuits
  -- putStrLn $ "Big three: " ++ show big_three
  pure $ x1 * x2

readCoordsFile :: FilePath -> IO [Coord]
readCoordsFile file = do 
  ls <- T.lines <$> TIO.readFile file
  pure $ map readCoord ls

data Coord = Coord Int Int Int deriving (Show, Eq, Ord)

newtype Circuits = Circuits {
  circuitMap :: M.Map Coord Int
} deriving Show

-- emptyCircuits :: Circuits
-- emptyCircuits = Circuits M.empty

connTillOne :: Circuits -> [(Coord, Coord)] -> Maybe (Coord, Coord)
connTillOne _ [] = Nothing
connTillOne circuits (pair : pairs) = 
  let circuits' = conCoords0 circuits pair
   in if allAreOne circuits'
        then Just pair
        else connTillOne circuits' pairs

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

-- newCircuit :: Coord -> Coord -> Circuits -> Circuits
-- newCircuit coord1 coord2 Circuits{circuitCount, circuitMap} = 
--   let new_circuit = circuitCount + 1
--    in Circuits new_circuit (M.insert coord1 new_circuit (M.insert coord2 new_circuit circuitMap))

conPairs0 :: [(Coord, Coord)] -> Circuits -> Circuits
conPairs0 pairs init_circuits = foldl' conCoords0 init_circuits pairs

loneCircuits0 :: [Coord] -> Circuits
loneCircuits0 = Circuits . M.fromList . (`zip` [0..])

conCoords0 :: Circuits -> (Coord, Coord) -> Circuits
conCoords0 circuits pair =  
  case coordCircuits pair circuits of 
    (Just circuit1, Just circuit2) -> mergeCircuits circuit1 circuit2 circuits
    _ -> error $ "Coord without circuit: " ++ show (circuits, pair)

allAreOne :: Circuits -> Bool
allAreOne Circuits{circuitMap} =
  ((== 1) . S.size) . S.fromList . M.elems $ circuitMap

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
