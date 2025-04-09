-- | propositional logic
module Specs.Propositional where
import           Math
import qualified Specs.LTL as LTL

data Formula =
  Atom String
  | Not Formula
  | Formula `Or` Formula
  | Formula `And` Formula
  | Formula `Implies` Formula

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

-- | convert a formula in propositional logic to LTL, so that we
-- can solve it with the LTL solvers.
toLTL :: Formula -> LTL.Formula
toLTL f = case f of
  Atom s          -> LTL.Atom s
  Not f'          -> LTL.Not $ toLTL f'
  f1 `And` f2     -> (toLTL f1) `LTL.And` (toLTL f2)
  f1 `Or` f2      -> (toLTL f1) `LTL.Or` (toLTL f2)
  f1 `Implies` f2 -> (toLTL f1) `LTL.Implies` (toLTL f2)
