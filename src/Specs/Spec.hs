module Specs.Spec where
import           Math
import qualified Solvers.LTL.Aalta as Aalta
import qualified Solvers.LTL.Black as Black
import qualified Specs.LTL         as LTL

-- a class for representing specs
class (Complementable a, Intersectable a) => Spec a where
  -- solve sat / language emptiness for a spec (should be either
  -- a logical formula or an automata based specification) using
  -- an external solver.
  -- Arguments are: solver to use, specificaiton.
  -- Returns either an error from the external program
  -- or a result
  solve :: String -> a -> IO (Either String Bool)

instance Spec LTL.Formula where
  solve solver f = case solver of
    "black" -> Black.solve f
    "aalta" -> Aalta.solve f
    _       -> pure $ Left ("unknown solver: " <> solver)

