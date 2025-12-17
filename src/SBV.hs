module SBV (optimizeLexicographic) where 

import Data.SBV

{-- 

type Satisfiable = SatisfiableM IO

ExtractIO m => SatisfiableM m SBool
 
class ExtractIO m => SatisfiableM (m :: Type -> Type) a 

type Symbolic = SymbolicT IO

data SymbolicT (m :: Type -> Type) a


type ConstraintSet = Symbolic ()

sat :: Satisfiable a => a -> IO SatResult

sInteger :: String -> Symbolic SInteger

constrain :: (SolverContext m, QuantifiedBool a) => a -> m ()

(.==) :: a -> a -> SBool

sTrue :: SBool

minimize :: Metric a => String -> SBV a -> Symbolic ()

problem :: ConstraintSet
problem = do [x1, x2] <- mapM sReal ["x1", "x2"]

             constrain $ x1 + x2 .<= 10
             constrain $ x1 - x2 .>= 3
             constrain $ 5*x1 + 4*x2 .<= 35
             constrain $ x1 .>= 0
             constrain $ x2 .>= 0

             maximize "goal" $ 5 * x1 + 6 * x2
--}

optimizeLexicographic :: ConstraintSet -> IO OptimizeResult
optimizeLexicographic = optimize Lexicographic

solve1 = optimize Lexicographic $ do
  b0 <- sInteger "b0"
  b1 <- sInteger "b1"
  b2 <- sInteger "b2"
  b3 <- sInteger "b3"
  b4 <- sInteger "b4"
  b5 <- sInteger "b5"

  constrain $ b0 .>= 0
  constrain $ b1 .>= 0
  constrain $ b2 .>= 0
  constrain $ b3 .>= 0
  constrain $ b4 .>= 0
  constrain $ b5 .>= 0
  constrain $ (b4 + b5) .>= 3
  constrain $ (b1 + b5) .>= 5
  constrain $ (b2 + b3 + b4) .>= 4
  constrain $ (b0 + b1 + b3) .>= 7

  minimize "presses" (b0 + b1 + b2 + b3 + b4 + b5)

