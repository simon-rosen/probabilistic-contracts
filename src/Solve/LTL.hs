module Solve.LTL where
import           Logics.LTL
import qualified Solve.Solvers.Black as Black

data Solver = Black

solve :: Solver -> Formula -> IO (Either String Bool)
solve s f = case s of
  Black -> Black.solve f

