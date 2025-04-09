module Specs.Solvable where
import           Control.Concurrent.Timeout (timeout)
import           Math
import qualified Solvers.LTL.Aalta          as Aalta
import qualified Solvers.LTL.Black          as Black
import qualified Solvers.LTL.Portfolio      as Portfolio
import qualified Solvers.LTL.Spot           as Spot
import           Solvers.Solver
import qualified Specs.LTL                  as LTL
import qualified Specs.MTL                  as MTL
import qualified Specs.Propositional        as Propositional

-- a class for representing specs
class (Complementable a, Intersectable a) => Solvable a where
  -- solve sat / language emptiness for a spec (should be either
  -- a logical formula or an automata based specification) using
  -- an external solver.
  -- Arguments are: solver to use, specificaiton.
  -- Returns either an error from the external program
  -- or a result
  solve :: String -> a -> IO (Either String Bool)


solverResultToEither :: SolverResult -> Either String Bool
solverResultToEither res = case res of
  Completed b -> Right b
  Failed err  -> Left err
  TimedOut    -> Left "timeout"


-- | set a timeout on a solver in microseconds
solveWithTimeout :: (Solvable a) => Integer -> String -> a -> IO (Either String Bool)
solveWithTimeout t solver f = do
  res <- timeout t $ solve solver f
  case res of
    Nothing  -> pure $ Left "TimedOut"
    Just res -> case res of
      Left err -> pure $ Left err
      Right b  -> pure $ Right b


instance Solvable LTL.Formula where
  solve solver f = case solver of
    "black"     -> solverResultToEither <$> Black.solve f
    "aalta"     -> solverResultToEither <$> Aalta.solve f
    "spot"      -> solverResultToEither <$> Spot.solve f
    "portfolio" -> solverResultToEither <$> Portfolio.solve f
    _           -> pure $ Left ("unknown solver: " <> solver)

instance Solvable MTL.Formula where
  solve solver f =
    let ltl = MTL.toLTL f -- first transform the MTL formula to LTL
    in solve solver ltl -- then solve the LTL formula

instance Solvable Propositional.Formula where
  solve solver f =
    let ltl = Propositional.toLTL f -- first transform the propositional logic formula to LTL
    in solve solver ltl -- then solve the LTL formula
