module Reductions.LinearEq where
import qualified Data.Set as Set
import           Math

-- The types of linear equations we want to express
data LinearEq =
    SumTo1 [Var] -- all vars sum to 1
  | NonNeg Var -- a var is >= 0
  | IsZero Var -- a var == 0
  -- the equation expressing the probability of a contract
  -- ex: z1 + z2 <= 0.5 * (z1 + z2 + z3)
  | ContractEq [Var] Compare Double [Var]
  deriving (Show)

-- | get all variables in a system of linear equations
getVars :: [LinearEq] -> [Var]
getVars eqs = (Set.elems . Set.fromList ) $ concatMap getVar eqs
  where
    getVar :: LinearEq -> [Var]
    getVar eq = case eq of
                  SumTo1 vs              -> vs
                  NonNeg v               -> [v]
                  IsZero v               -> [v]
                  ContractEq vs1 _ _ vs2 -> vs1 <> vs2
