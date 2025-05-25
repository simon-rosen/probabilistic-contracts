-- | Entry point for running some benchmarks
module Benchmark.Benchmark where
import           Benchmark.Database
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines
import           Control.Concurrent.Timeout   (timeout)
import           Control.Exception            (evaluate)
import           Data.Time.Clock
import           Database.SQLite.Simple       (Connection, close, open)
import           Generate.ProbContract        (randomRefinementProblemWithLTL,
                                               randomRefinementProblemWithMLTL)
import           Math
import qualified Specs.LTL                    as LTL
import qualified Specs.MLTL                   as MLTL
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
benchmarkRefinementLTL :: (Eq a, Solvable a) => Integer -> Integer -> (RefinementProblem a)
  -> IO (Either String (Secs, Secs, Int, Bool))
benchmarkRefinementLTL varsTime sysTime problem = do
  let contracts = (systemContract problem : componentContracts problem)
  -- find all "non-empty" variables
  -- * the full search is given a timeout of 10min
  -- * each sat-check is given a timeout of 1min (there can be 2^n such sat checks
  -- so if many of them takes > 1min then the total time could be too large)
  putStrLn "finding vars"
  t0 <- getCurrentTime
  eVars <- timeout (varsTime*oneSecond) $ nonZeroVars "portfolio" contracts
  case eVars of
    Nothing -> pure $ Left "finding vars error: timeout"
    Just (Left err) -> pure $ Left ("finding vars error: "<> err)
    Just (Right vars) -> do
      _ <- evaluate (length vars) -- force evaluation before timing ends
      t1 <- getCurrentTime
      -- solve the equation system
      -- the timeout for this is also 10min
      putStrLn "solving sys"
      t2 <- getCurrentTime
      res <- timeout (sysTime*oneSecond) $ createAndSolveIneqs contracts vars
      _ <- evaluate res -- force evaluation before timing ends
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


benchmarkRefinementMLTL :: (Eq a, Solvable a) => Integer -> Integer -> (RefinementProblem a)
  -> IO (Either String (Secs, Secs, Int, Bool))
benchmarkRefinementMLTL varsTime sysTime problem = do
  let contracts = (systemContract problem : componentContracts problem)
  -- find all "non-empty" variables
  -- * the full search is given a timeout of 10min
  -- * each sat-check is given a timeout of 1min (there can be 2^n such sat checks
  -- so if many of them takes > 1min then the total time could be too large)
  putStrLn "finding vars"
  t0 <- getCurrentTime
  eVars <- timeout (varsTime*oneSecond) $ nonZeroVars "smt" contracts
  case eVars of
    Nothing -> pure $ Left "finding vars error: timeout"
    Just (Left err) -> pure $ Left ("finding vars error: "<> err)
    Just (Right vars) -> do
      _ <- evaluate (length vars) -- force evaluation before timing ends
      t1 <- getCurrentTime
      -- solve the equation system
      -- the timeout for this is also 10min
      putStrLn "solving sys"
      t2 <- getCurrentTime
      res <- timeout (sysTime*oneSecond) $ createAndSolveIneqs contracts vars
      _ <- evaluate res -- force evaluation before timing ends
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
runLTLBenchmark :: Connection -> Bool -> Int -> Int -> Int -> Integer -> Integer -> IO ()
runLTLBenchmark conn common maxComponents maxFormulaSize maxAtoms varsT sysT = do
  -- generate the random configuration
  comps <- randomRIO (1, maxComponents)
  mfs <- randomRIO (1, maxFormulaSize)
  ma <- randomRIO (1, maxAtoms)
  putStrLn $ "configuration: LTL, components = " <> show comps
              <> " formulaSize = " <> show mfs
              <> " atoms = " <> show ma
  -- get a random LTL refinement problem
  rp <- randomRefinementProblemWithLTL common comps mfs ma
  -- analyze LTL formulas
  let specs = concat [[a,g] | (ProbContract (Probability (a,g) _ _)) <- (systemContract rp : componentContracts rp)]
  let totalSize = sum . map (LTL.totalSize) $ specs
  let maxTotalDepth = maximum . map (LTL.maxTotalDepth) $ specs
  let numTemporalOperators = sum . map (LTL.numTemporalOperators) $ specs
  let maxTemporalDepth = maximum . map (LTL.maxTemporalDepth) $ specs
  putStrLn $ "info: " <> "total_size = " <> show totalSize
                      <> ", max_total_depth = " <> show maxTotalDepth
                      <> ", num_temporal_operators = " <> show numTemporalOperators
                      <> ", max_temporal_depth = " <> show maxTemporalDepth
  -- create an initial model for the database
  let probModel = LTLProblemBenchmark { ltlProblem = show rp
                                      , ltlNumComponents = comps
                                      , ltlFormulaSize = mfs
                                      , ltlAtomsPerVar = ma
                                      -- some more information about formulas
                                      , ltlTotalSize = totalSize
                                      , ltlMaxTotalDepth = maxTotalDepth
                                      , ltlNumTemporalOperators = numTemporalOperators
                                      , ltlMaxTemporalDepth = maxTemporalDepth
                                      -- some default values that will be overwritten based
                                      -- on the result of the benchmark
                                      , ltlCompleted = False
                                      , ltlErrorMsg = "init"
                                      , ltlResult = False
                                      , ltlNumNonEmptyVars = 0
                                      , ltlFindingVarsTime = 0.0
                                      , ltlSolvingSysTime = 0.0
                                      }
  -- run the benchmark
  benchRes <- benchmarkRefinementLTL varsT sysT rp
  case benchRes of
    Left err -> do
      let benchmark = probModel { ltlErrorMsg = err}
      insertLTLProblemBenchmark conn benchmark
      pure ()
    Right (vt, st, nv, res) -> do
      -- save to database
      let benchmark =
            probModel { ltlCompleted = True
                      , ltlErrorMsg = "success"
                      , ltlResult = res
                      , ltlNumNonEmptyVars = nv
                      , ltlFindingVarsTime = vt
                      , ltlSolvingSysTime = st
                      }
      insertLTLProblemBenchmark conn benchmark
      pure ()


-- | run one random benchmark on MLTL refinement problems in some parameter space
runMLTLBenchmark :: Connection -> Bool -> Int -> Int -> Int -> Int -> Integer -> Integer -> IO ()
runMLTLBenchmark conn common maxComponents maxFormulaSize maxAtoms maxTime varsT sysT = do
  -- generate the random configuration
  comps <- randomRIO (1, maxComponents)
  mfs <- randomRIO (1, maxFormulaSize)
  ma <- randomRIO (1, maxAtoms)
  mt <- randomRIO (1, maxTime)
  putStrLn $ "configuration: MLTL, components = " <> show comps
              <> " formulaSize = " <> show mfs
              <> " atoms = " <> show ma
              <> " maxTime  = " <> show mt
  -- get a random MLTL refinement problem
  rp <- randomRefinementProblemWithMLTL common comps mfs ma mt
  -- analyze MLTL formulas
  let specs = concat [[a,g] | (ProbContract (Probability (a,g) _ _)) <- (systemContract rp : componentContracts rp)]
  let totalSize = sum . map (MLTL.totalSize) $ specs
  let maxTotalDepth = maximum . map (MLTL.maxTotalDepth) $ specs
  let numTemporalOperators = sum . map (MLTL.numTemporalOperators) $ specs
  let maxTemporalDepth = maximum . map (MLTL.maxTemporalDepth) $ specs
  putStrLn $ "info: " <> "total_size = " <> show totalSize
                      <> ", max_total_depth = " <> show maxTotalDepth
                      <> ", num_temporal_operators = " <> show numTemporalOperators
                      <> ", max_temporal_depth = " <> show maxTemporalDepth
  -- create an initial model for the database
  let probModel = MLTLProblemBenchmark { mltlProblem = show rp
                                       , mltlNumComponents = comps
                                       , mltlFormulaSize = mfs
                                       , mltlAtomsPerVar = ma
                                       , mltlMaxTime = mt
                                       -- some more information about formulas
                                       , mltlTotalSize = totalSize
                                       , mltlMaxTotalDepth = maxTotalDepth
                                       , mltlNumTemporalOperators = numTemporalOperators
                                       , mltlMaxTemporalDepth = maxTemporalDepth
                                       -- some default values that will be overwritten based
                                       -- on the result of the benchmark
                                       , mltlCompleted = False
                                       , mltlErrorMsg = "init"
                                       , mltlResult = False
                                       , mltlNumNonEmptyVars = 0
                                       , mltlFindingVarsTime = 0.0
                                       , mltlSolvingSysTime = 0.0
                                       }
  -- run the benchmark
  benchRes <- benchmarkRefinementMLTL varsT sysT rp
  case benchRes of
    Left err -> do
      let benchmark = probModel { mltlErrorMsg = err }
      insertMLTLProblemBenchmark conn benchmark
      pure ()
    Right (vt, st, nv, res) -> do
      -- save to database
      createLTLProblemsTable conn
      let benchmark =
            probModel { mltlCompleted = True
                      , mltlErrorMsg = "success"
                      , mltlResult = res
                      , mltlNumNonEmptyVars = nv
                      , mltlFindingVarsTime = vt
                      , mltlSolvingSysTime = st
                      }
      insertMLTLProblemBenchmark conn benchmark
      pure ()


runBenchmarks :: IO ()
runBenchmarks = do
  -- setup db
  conn <- open "benchmark.db"
  createLTLProblemsTable conn
  createMLTLProblemsTable conn
  -- write some info
  t <- getCurrentTime
  putStrLn $ "\nnew benchmark started at " <> show t
  -- run benchmark
  --runMLTLBenchmark conn 2 20 5 10
  putStrLn "benchmark finished\n"
  -- close db
  close conn
  -- repeat
  --runBenchmarks


