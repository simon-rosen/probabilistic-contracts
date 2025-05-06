-- | solve MLTL using a conversion to LTL, do not use this because
-- it is really bad.
module Solvers.MLTL.UsingLTLSolvers where

import           Control.Exception (SomeException, bracket, evaluate, finally,
                                    try)
import           Data.List         (isInfixOf)
import           Solvers.Solver
import           Specs.LTL
import qualified Specs.MLTL        as MLTL
import qualified Specs.Solvable    as Solvable
import           System.Exit       (ExitCode (..))
import           System.IO
import           System.Process
import           System.Process    (readProcessWithExitCode)

-- | Solve an MLTL formula by converting it LTL
solve :: MLTL.Formula -> IO SolverResult
solve f = undefined
