-- | solving linear real arithmetic using external tools
module Solve.LRA where
import           Math
import qualified Solve.Solvers.Z3 as Z3

solve :: [Var] -> [Var] -> [LinearIneq] -> IO (Either String Bool)
solve = Z3.solve
