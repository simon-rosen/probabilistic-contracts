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
import           System.FilePath                (FilePath)

-- working with LTL problems
data LTLProblemBenchmark =
  LTLProblemBenchmark { ltlProblem         :: String -- for now, the full problem pretty printed
                      , ltlFindingVarsTime :: Double -- the time it takes to find all non-empty variables
                      , ltlSolvingSysTime  :: Double -- the time it takes to solve the equation system
                      , ltlResult          :: String -- the result
                      }
                      deriving (Show)

instance FromRow LTLProblemBenchmark where
  fromRow = LTLProblemBenchmark <$> field <*> field <*> field <*> field

instance ToRow LTLProblemBenchmark where
  toRow p = toRow (ltlProblem p, ltlFindingVarsTime p, ltlSolvingSysTime p, ltlResult p)

-- | Create a table to store the
createLTLProblemsTable :: Connection -> IO ()
createLTLProblemsTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS ltl_problems (problem TEXT, finding_vars_time REAL, solving_sys_time INT, result TEXT)"

-- | Insert a benchmark for a refinement problem with contracts specified in LTL
insertLTLProblemBenchmark :: Connection -> LTLProblemBenchmark -> IO ()
insertLTLProblemBenchmark conn ltlP =
  execute conn "INSERT INTO ltl_problems (problem, finding_vars_time, solving_sys_time, result) VALUES (?,?,?,?)" ltlP

