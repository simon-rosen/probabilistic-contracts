-- | Entry point for running some benchmarks
module Benchmark.Benchmark where
import           Benchmark.Database
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines
import           Control.Concurrent.Timeout   (timeout)
import           Data.Time.Clock
import           Database.SQLite.Simple       (close, open)
import           Generate.ProbContract        (randomRefinementProblemWithLTL)
import           Specs.Solvable
import           System.Random                (randomRIO)

type Secs = Double

-- one second is 1000 000ns
oneSecond :: Integer
oneSecond = 1000000

-- | Run a benchmark on a refinement problem
--
-- Given a time limit for each sat-check, the total max time for finding all vars,
-- and for solving the equation system;
-- solve the problem and record the time it took to find all vars, and the time
-- it took to solve the equation system.
--
-- Also record the number of "non-zero" variables and the result of the refinement
benchmarkRefinement :: (Eq a, Solvable a) => Secs -> Secs -> Secs -> (RefinementProblem a)
  -> IO (Either String (Secs, Secs, Int, Bool))
benchmarkRefinement satTime varsTime sysTime problem = do
  let contracts = (systemContract problem : componentContracts problem)
  -- find all "non-empty" variables
  -- * the full search is given a timeout of 10min
  -- * each sat-check is given a timeout of 1min (there can be 2^n such sat checks
  -- so if many of them takes > 1min then the total time could be too large)
  putStrLn "finding vars"
  t0 <- getCurrentTime
  eVars <- timeout ((round varsTime)*oneSecond) $ nonZeroVars ((round satTime)*oneSecond) "portfolio" contracts
  t1 <- getCurrentTime
  case eVars of
    Nothing -> pure $ Left "finding vars error: timeout"
    Just (Left err) -> pure $ Left ("finding vars error: "<> err)
    Just (Right vars) -> do
      -- solve the equation system
      -- the timeout for this is also 10min
      putStrLn "solving sys"
      t2 <- getCurrentTime
      res <- timeout ((round sysTime)*oneSecond) $ createAndSolveIneqs contracts vars
      t3 <- getCurrentTime
      case res of
        Nothing             -> pure $ Left "solving sys error: timeout"
        Just (Left err)     -> pure $ Left ("solving sys error: " <> err)
        Just (Right solved) -> do
          let secs1 = realToFrac (diffUTCTime t1 t0) :: Double
          let secs2 = realToFrac (diffUTCTime t3 t2) :: Double
          let numVars = length vars
          -- if the system is unsolvable then the refinement holds
          let refinesResult = not solved
          putStrLn $ "success: " <> show (secs1, secs2, numVars, refinesResult)
          pure $ Right (secs1, secs2, numVars, refinesResult)

-- | run one random benchmarks on LTL refinement problems in some parameter space
runLTLBenchmark :: Int -> Int -> Int -> IO ()
runLTLBenchmark maxComponents maxFormulaSize maxAtoms = do
  -- generate the random configuration
  comps <- randomRIO (1, maxComponents)
  mfs <- randomRIO (1, maxFormulaSize)
  ma <- randomRIO (1, maxAtoms)
  putStrLn $ "configuration: LTL, components = " <> show comps
              <> " formulaSize = " <> show mfs
              <> " atoms = " <> show ma
  -- get a random LTL refinement problem
  rp <- randomRefinementProblemWithLTL comps mfs ma
  -- run the benchmark
  benchRes <- benchmarkRefinement 60 (10*60) (10*60) rp
  case benchRes of
    Left err -> putStrLn $ "error in benchmark: " <> err
    Right (vt, st, nv, res) -> do
      -- save to database
      conn <- open "benchmark.db"
      createLTLProblemsTable conn
      let benchmark = LTLProblemBenchmark { ltlProblem = show rp
                                          , ltlFindingVarsTime = vt
                                          , ltlSolvingSysTime = st
                                          , ltlResult = show res
                                          }
      insertLTLProblemBenchmark conn benchmark
      close conn
      pure ()


runBenchmarks :: IO ()
runBenchmarks = do
  putStrLn "new benchmark"
  runLTLBenchmark 5 50 10
  -- repeat
  runBenchmarks




