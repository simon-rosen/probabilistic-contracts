-- | MTL over naturals
-- based on the paper https://link.springer.com/chapter/10.1007/978-3-319-63046-5_20
module Specs.MTL where
import           Math
import qualified Specs.LTL as LTL

data Endpoint = Nat Int | PosInf
              deriving (Show, Eq, Ord)

getNum :: Endpoint -> Int
getNum (Nat i) = i

type Interval = (Endpoint, Endpoint)

-- | A MTL formula
data Formula =
    T -- ^ True
  | F -- ^ False
  | Atom String -- ^ p
  | Not Formula -- ^ !phi
  | Or Formula Formula -- ^ phi1 | phi2
  | And Formula Formula -- ^ phi1 & phi2
  | Implies Formula Formula -- ^ phi1 -> phi2
  | Next Interval Formula -- X phi
  | Future Interval Formula -- F phi
  | Globally Interval Formula -- G phi
  | Until Interval Formula Formula --phi1 U phi2
  | DualUntil Interval Formula Formula -- constrained dual until operator: makes nnf possible
  deriving (Show, Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2


-- push all negations of formulas so that they only appear in front of variables
nnf :: Formula -> Formula
nnf (Not f) = case f of -- formulas with negations in front: convert and recurse
  -- base cases
  T             -> F
  F             -> T
  Atom p        -> Atom p
  -- drop double negations
  Not f'        -> nnf f'
  -- de morgan
  Or f1 f2      -> And (Not $ nnf f1) (Not $ nnf f2)
  And f1 f2     -> Or (Not $ nnf f1) (Not $ nnf f2)
  -- convert a -> b to (!a) | b, and then run the nnf on this instead
  Implies f1 f2 -> nnf $ Or (Not f1) f2
  -- the next operator has a special case where a == b in the interval,
  -- see the article referenced in the beginning of this module.
  Next (a, b) f' | a == b -> (Next (a, b) (Not $ nnf f')) `Or` (Next (Nat 0, Nat $ getNum a - 1) T) `Or` (Next (Nat $ getNum b + 1, PosInf) T)
                 | otherwise -> Next (a, b) (Not $ nnf f')
  Future i f' -> Globally i (Not $ nnf f')
  Globally i f' -> Future i (Not $ nnf f')
  Until i f1 f2 -> DualUntil i (Not $ nnf f1) (Not $ nnf f2)
  DualUntil i f1 f2 -> Until i (Not $ nnf f1) (Not $ nnf f2)
nnf f = case f of -- formulas with no negations in front: just recurse
  -- base cases
  T                 -> T
  F                 -> F
  Atom p            -> Atom p
  -- Not f, will be catched earlier
  Or f1 f2          -> Or (nnf f1) (nnf f2)
  And f1 f2         -> And (nnf f1) (nnf f2)
  Implies f1 f2     -> Implies (nnf f1) (nnf f2)
  Next i f'         -> Next i (nnf f')
  Future i f'       -> Future i (nnf f')
  Globally i f'     -> Globally i (nnf f')
  Until i f1 f2     -> Until i (nnf f1) (nnf f2)
  DualUntil i f1 f2 -> DualUntil i (nnf f1) (nnf f2)

-- Flatten out a formula, so that there are no nested temporal operators.
-- This is done by renaming formulas with nested operators.
fnf :: Formula -> Formula
fnf f = case f of
  -- base cases
  T      -> T
  F      -> F
  Atom p -> Atom p
  -- not sure how to implement it yet...


-- | convert a MTL formula (over naturals) to a LTL formula,
-- using the gap translation and strict semantics of https://doi.org/10.1007/978-3-319-63046-5_20
toLTL :: Formula -> LTL.Formula
toLTL = undefined

