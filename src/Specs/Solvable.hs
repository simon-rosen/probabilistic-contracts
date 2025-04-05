module Specs.Solvable where
import           Math
import qualified Solvers.LTL.Aalta     as Aalta
import qualified Solvers.LTL.Black     as Black
import qualified Solvers.LTL.Portfolio as Portfolio
import qualified Solvers.LTL.Spot      as Spot
import           Solvers.Solver
import qualified Specs.LTL             as LTL

-- a class for representing specs
class (Complementable a, Intersectable a) => Solvable a where
  -- solve sat / language emptiness for a spec (should be either
  -- a logical formula or an automata based specification) using
  -- an external solver.
  -- Arguments are: solver to use, specificaiton.
  -- Returns either an error from the external program
  -- or a result
  solve :: String -> a -> IO (Either String Bool)

instance Solvable LTL.Formula where
  solve solver f = case solver of
    "black" -> solverResultToEither <$> Black.solve f
    "aalta" -> solverResultToEither <$> Aalta.solve f
    "spot"  -> solverResultToEither <$> Spot.solve f
    --"portfolio" -> Portfolio.solve f
    _       -> pure $ Left ("unknown solver: " <> solver)


solverResultToEither :: SolverResult -> Either String Bool
solverResultToEither res = case res of
  Completed b -> Right b
  Failed err  -> Left err
  TimedOut    -> Left "timedout"

solveWithTimeout :: Int -> String -> a -> IO SolverResult
solveWithTimeout t solver f = undefined


