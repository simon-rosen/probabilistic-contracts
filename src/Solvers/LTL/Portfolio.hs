-- | this is the solver I intend to use: a portfolio solver
-- that runs all other solvers in parallell and once one solver
-- finisher it terminates all processess
module Solvers.LTL.Portfolio where

import           Control.Concurrent.Timeout (timeout)
import qualified Solvers.LTL.Aalta          as Aalta
import qualified Solvers.LTL.Black          as Black
import qualified Solvers.LTL.Spot           as Spot
import           Solvers.Solver
import           Specs.LTL


import           Control.Concurrent.Async
import           Control.Monad              (forM, forM_, void)
import           Data.List                  (delete)
import           System.Process             (readProcessWithExitCode)


solve :: Formula -> IO SolverResult
solve = runSolversInParallel [ ("aalta", Aalta.solve)
                             , ("black", Black.solve)
                             , ("spot", Spot.solve)
                             ]

-- | this function runs all solvers in parallel and returns once any of them
-- gives a result. If all of them should fail then the error messages for them all
-- is returned.
runSolversInParallel :: [(String, Solver)] -> Formula -> IO SolverResult
runSolversInParallel solvers f = do
  -- start all solvers in parallel
  jobs <- forM solvers $ \(name, solver) -> async $ do
    result <- solver f -- run the solvingAction
    pure (name, result)

  -- wait for the first result / all of them to fail
  result <- waitResult jobs []
  -- cancel any remaining solvers
  mapM_ (\job -> cancel job >> waitCatch job) jobs
  -- explicitly kill all solvers (this makes running this whole function in parallell
  -- impossible, but i dont want to do that anyway). There was a problem with solvers
  -- getting stuck and not responding to their termination messages.
  forM_ ["aalta", "black", "spot_ltl_sat.py", "z3"] $ \name -> do
    _ <- readProcessWithExitCode "pkill" ["-9", "-f", name] ""
    pure ()
  -- return result
  pure result
  where
    waitResult :: [Async (String, SolverResult)] -> [(String, SolverResult)] -> IO SolverResult
    waitResult jobs errors = case jobs of
      -- all solvers failed
      []        -> do
        --putStrLn "all solvers failed"
        pure $ Failed ("all solvers failed")
      _ -> do
        (job, (name, solverRes)) <- waitAny jobs
        case solverRes of
          Completed b -> pure $ Completed b
          Failed err  -> waitResult (delete job jobs) []
          TimedOut    -> waitResult (delete job jobs) []


