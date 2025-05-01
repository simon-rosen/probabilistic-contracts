module Specs.Solvable where
import           Control.Concurrent.Timeout (timeout)
import           Math
import qualified Solvers.LTL.Aalta          as Aalta
import qualified Solvers.LTL.Black          as Black
import qualified Solvers.LTL.Portfolio      as Portfolio
import qualified Solvers.LTL.Spot           as Spot
import           Solvers.Solver
import qualified Specs.LTL                  as LTL
import qualified Specs.MLTL                 as MLTL
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
  -- solve with a timeout given in microseconds, this has a default implementation
  solveWithTimeout :: Integer -> String -> a -> IO (Either String Bool)
  solveWithTimeout t solver f = do
    res <- timeout t $ solve solver f
    case res of
      Nothing  -> pure $ Left "TimedOut"
      Just res -> case res of
        Left err -> pure $ Left err
        Right b  -> pure $ Right b


solverResultToEither :: SolverResult -> Either String Bool
solverResultToEither res = case res of
  Completed b -> Right b
  Failed err  -> Left err
  TimedOut    -> Left "timeout"



instance Solvable LTL.Formula where
  solve solver f = case solver of
    "black"     -> solverResultToEither <$> Black.solve f
    "aalta"     -> solverResultToEither <$> Aalta.solve f
    "spot"      -> solverResultToEither <$> Spot.solve f
    "portfolio" -> solverResultToEither <$> Portfolio.solve f
    _           -> pure $ Left ("unknown solver: " <> solver)

instance Solvable MLTL.Formula where
  solve solver f = do
    converted <- MLTL.toLTL f
    case converted of
      Left err   -> (solverResultToEither <$> (pure $ Failed "failed during conversion"))
      Right ltlF -> solve solver ltlF

