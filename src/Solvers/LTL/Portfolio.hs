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
import           Control.Monad              (forM, void)
import           Data.List                  (delete)


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
    result <- timeout 10000000 $ solver f -- run the solvingAction
    case result of
      Nothing  -> pure (name, TimedOut)
      Just res -> pure (name, res)

  -- wait for the first result / all of them to fail
  result <- waitResult jobs []
  -- cancel any remaining solvers
  --mapM_ cancel jobs
  mapM_ (\job -> cancel job >> waitCatch job) jobs
  --putStrLn "cancelling solvers"
  -- return result
  pure result
  where
    waitResult :: [Async (String, SolverResult)] -> [(String, SolverResult)] -> IO SolverResult
    waitResult jobs errors = case jobs of
      -- all solvers failed
      []        -> do
        putStrLn "all solvers failed"
        pure $ Failed ("all solvers failed")
      _ -> do
        (job, (name, solverRes)) <- waitAny jobs
        case solverRes of
          Completed b -> do
            --putStrLn $ "formula solved by: " <> name
            print solverRes
            pure $ Completed b
          Failed err           -> do
            putStrLn $ name <> " error: " <> err
            waitResult (delete job jobs) []
          TimedOut -> do
            putStrLn $ name <> " timed out"
            waitResult (delete job jobs) []


-- | this function runs all solvers in parallel and returns once any of them
-- gives a result. If all of them should fail then the error messages for them all
-- is returned.
callPortfolio :: [(String, SolverCaller)] -> String -> IO SolverResult
callPortfolio solvers fstr = do
  -- start all solvers in parallel
  jobs <- forM solvers $ \(name, solver) -> async $ do
    result <- timeout 10000000 $ solver fstr -- run the solvingAction
    case result of
      Nothing  -> pure (name, TimedOut)
      Just res -> pure (name, res)

  -- wait for the first result / all of them to fail
  result <- waitResult jobs []
  -- cancel any remaining solvers
  --mapM_ cancel jobs
  mapM_ (\job -> cancel job >> waitCatch job) jobs
  --putStrLn "cancelling solvers"
  -- return result
  pure result
  where
    waitResult :: [Async (String, SolverResult)] -> [(String, SolverResult)] -> IO SolverResult
    waitResult jobs errors = case jobs of
      -- all solvers failed
      []        -> do
        putStrLn "all solvers failed"
        pure $ Failed ("all solvers failed")
      _ -> do
        (job, (name, solverRes)) <- waitAny jobs
        case solverRes of
          Completed b -> do
            --putStrLn $ "formula solved by: " <> name
            print solverRes
            pure $ Completed b
          Failed err           -> do
            putStrLn $ name <> " error: " <> err
            waitResult (delete job jobs) []
          TimedOut -> do
            putStrLn $ name <> " timed out"
            waitResult (delete job jobs) []


