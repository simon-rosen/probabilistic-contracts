{-# LANGUAGE OverloadedStrings #-}
-- ^ the query is a bytestring

-- | Manage the sqlite database where the benchmarks are stored
--
-- The structure is quick and dirty: tests with LTL contracts will
-- be stored in one table with columns for each thing that is of interest.
-- And tests for MLTL contracts will be stored in another table.
--
-- Testcases will only be added to the database, hence there is no other logic
-- for working with the database than to insert new rows (and maybe run some
-- queries on the database).
--
-- inspired by the example https://hackage.haskell.org/package/sqlite-simple-0.4.19.0/docs/Database-SQLite-Simple.html
module Benchmark.Database where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField (ToField (..))
import           System.FilePath                (FilePath)

--------------------------- LTL -------------------------------------------------------
-- working with LTL problems
data LTLProblemBenchmark =
  LTLProblemBenchmark { ltlProblem         :: String -- for now, the full problem pretty printed
                      -- the configuration during random generation
                      , ltlNumComponents   :: Int
                      , ltlFormulaSize     :: Int
                      , ltlAtomsPerVar     :: Int
                      -- did the benchmark complete
                      , ltlCompleted       :: Bool
                      , ltlErrorMsg        :: String
                      -- if the benchmark completed this
                      -- can be set to true, otherwise it will allways be
                      -- set to false
                      , ltlResult          :: Bool
                      -- the number of variables in the equation system
                      , ltlNumNonEmptyVars :: Int
                      -- if the benchmark failes / timesout these will allways be 0
                      , ltlFindingVarsTime :: Double -- the time it takes to find all non-empty variables
                      , ltlSolvingSysTime  :: Double -- the time it takes to solve the equation system
                      }
                      deriving (Show)

instance FromRow LTLProblemBenchmark where
  fromRow = LTLProblemBenchmark
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field

instance ToRow LTLProblemBenchmark where
  toRow p = toRow
    ( ltlProblem p
    , ltlNumComponents p
    , ltlFormulaSize p
    , ltlAtomsPerVar p
    , ltlCompleted p
    , ltlErrorMsg p
    , ltlResult p
    , ltlNumNonEmptyVars p
    , ltlFindingVarsTime p
    , ltlSolvingSysTime p
    )

-- | Create a table to store the
createLTLProblemsTable :: Connection -> IO ()
createLTLProblemsTable conn = do
  execute_ conn
    ("CREATE TABLE IF NOT EXISTS ltl_problems"
      -- configuration
      <> "(problem TEXT, num_components INTEGER, formula_size INTEGER, atoms_per_var INTEGER"
      -- benchmarking status
      <> ", completed INTEGER, error_msg STRING"
      -- results
      <> ", result INTEGER, num_non_empty_vars INTEGER"
      <> ", finding_vars_time REAL, solving_sys_time REAL)")

-- | Insert a benchmark for a refinement problem with contracts specified in LTL
insertLTLProblemBenchmark :: Connection -> LTLProblemBenchmark -> IO ()
insertLTLProblemBenchmark conn ltlP =
  execute conn
    ( "INSERT INTO ltl_problems "
      -- configuration
      <> "(problem , num_components, formula_size, atoms_per_var"
      -- benchmarking status
      <> ", completed, error_msg"
      -- results
      <> ", result, num_non_empty_vars, finding_vars_time, solving_sys_time)"
      <> "VALUES (?,?,?,?,?,?,?,?,?,?)"
    ) ltlP



--------------------------- MLTL -------------------------------------------------------
-- A data model for MLTL benchmarks
-- the only difference to the LTL benchmark model is the configuration
-- "max_time" which is the largest time that can be used in the formulas
data MLTLProblemBenchmark =
  MLTLProblemBenchmark { mltlProblem         :: String -- for now, the full problem pretty printed
                       -- the configuration during random generation
                       , mltlNumComponents   :: Int
                       , mltlFormulaSize     :: Int
                       , mltlAtomsPerVar     :: Int
                       , mltlMaxTime         :: Int
                       -- did the benchmark complete
                       , mltlCompleted       :: Bool
                       , mltlErrorMsg        :: String
                       -- if the benchmark completed this
                       -- can be set to true, otherwise it will allways be
                       -- set to false
                       , mltlResult          :: Bool
                       -- the number of variables in the equation system
                       , mltlNumNonEmptyVars :: Int
                       -- if the benchmark failes / timesout these will allways be 0
                       , mltlFindingVarsTime :: Double -- the time it takes to find all non-empty variables
                       , mltlSolvingSysTime  :: Double -- the time it takes to solve the equation system
                       }
                       deriving (Show)

instance FromRow MLTLProblemBenchmark where
  fromRow = MLTLProblemBenchmark
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field

-- had to manually instantiate toRow for 11-tuples
instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k]

instance ToRow MLTLProblemBenchmark where
  toRow p = toRow
    ( mltlProblem p
    , mltlNumComponents p
    , mltlFormulaSize p
    , mltlAtomsPerVar p
    , mltlMaxTime p
    , mltlCompleted p
    , mltlErrorMsg p
    , mltlResult p
    , mltlNumNonEmptyVars p
    , mltlFindingVarsTime p
    , mltlSolvingSysTime p
    )

-- | Create a table to store the
createMLTLProblemsTable :: Connection -> IO ()
createMLTLProblemsTable conn = do
  execute_ conn
    ("CREATE TABLE IF NOT EXISTS mltl_problems"
      -- configuration
      <> "(problem TEXT, num_components INTEGER, formula_size INTEGER"
      <> ", atoms_per_var INTEGER, max_time INTEGER"
      -- benchmarking status
      <> ", completed INTEGER, error_msg STRING"
      -- results
      <> ", result INTEGER, num_non_empty_vars INTEGER"
      <> ", finding_vars_time REAL, solving_sys_time REAL)")

-- | Insert a benchmark for a refinement problem with contracts specified in LTL
insertMLTLProblemBenchmark :: Connection -> MLTLProblemBenchmark -> IO ()
insertMLTLProblemBenchmark conn mltlP =
  execute conn
    ( "INSERT INTO mltl_problems "
      -- configuration
      <> "(problem , num_components, formula_size, atoms_per_var, max_time"
      -- benchmarking status
      <> ", completed, error_msg"
      -- results
      <> ", result, num_non_empty_vars, finding_vars_time, solving_sys_time)"
      <> "VALUES (?,?,?,?,?,?,?,?,?,?,?)"
    ) mltlP











