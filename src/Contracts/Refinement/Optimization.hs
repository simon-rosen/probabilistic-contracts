-- This is an idea for an optimization of refinement
-- verification by doing it only two components at a
-- time
module Contracts.Refinement.Optimization where
import           Contracts.Probabilistic
import           Contracts.Refinement.Refines (refines)
import           Math
import           Specs.Solvable               (Solvable)


optRefines ::(Show a, Eq a, Solvable a) => RefinementProblem a -> IO (Either String Bool)
optRefines = undefined

refines' :: (Show a, Eq a, Solvable a) => ProbContract a -> [ProbContract a]
             -> IO (Either String Bool)
refines' sys comps = do
  print comps
  case comps of
    -- only one component:
    -- normal refinement using the system
    [_] -> refines "portfolio" "z3" sys comps
    --(c1:c2:[]) -> refines "portfolio" "z3" sys comps
    -- two or more components:
    -- create an intermediate "system contract" based
    -- on the first two component contracts. This intermediate
    -- contract states that "given a1 -> g2 happens with probability from c2"
    (c1@(ProbContract p1):c2@(ProbContract p2):comps') -> do
      let (a1, _g1) = getProbEvent p1
      let (_a2, g2) = getProbEvent p2
      let r2 = getProbCmp p2
      let ps1 = getProbPs p1
      let ps2 = getProbPs p2
      let tmpSys = ProbContract (ProbabilityProd (a1, g2) r2 (ps1 <> ps2))
      --print tmpSys
      tmpRefines <- refines "portfolio" "z3" tmpSys [c1, c2]
      case tmpRefines of
        Left err -> pure $ Left err
        Right False -> do
          print (tmpSys, [c1, c2])
          pure $ Right False
        Right _  -> refines' sys (tmpSys:comps')





