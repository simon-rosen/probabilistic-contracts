-- | MTL over naturals
module Specs.MTL where
import           Math
import qualified Specs.LTL as LTL

data Endpoint = Natural Int | PosInf
              deriving (Show, Eq, Ord)

type Interval = (Endpoint, Endpoint)

-- | A LTL formula
data Formula =
  | T -- ^ true
  | F -- ^ false
  | Atom String -- ^ p
  | Not Formula -- ^ !phi
  | Or Formula Formula -- ^ phi1 | phi2
  | And Formula Formula -- ^ phi1 & phi2
  | Implies Formula Formula -- ^ phi1 -> phi2
  | Next Interval Formula -- X phi
  | Future Interval Formula -- F phi
  | Globally Interval Formula -- G phi
  | Until Interval Formula Formula --phi1 U phi2
  deriving (Show, Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

-- | represent a MTL formula with only the minimal set of operators needed. 
-- this simplifies conversion to normal form.
minOp :: Formula -> Formula
minOp f = case f of
  -- these are the minimal set of operators needed for defining MTL
  Atom p -> Atom p
  Not f' -> Not $ minOp f'
  f1 `And` f2 -> f1 `And` f2
  Next i f' -> Next i (minOp f')
  Until i f1 f2 -> Until i (minOp f1) (minOp f2)
  -- these operators can be defined in terms of the previous ones
  T -> minOp $ Atom "p" `Or` (Not $ Atom "p")
  F -> minOp $ Not T
  f1 `Or` f2 -> minOp $ Not ((Not f1) `And` (Not f2))
  f1 `Implies` f2 -> minOp $ (Not f1) `Or` f2
  Future i f -> minOp $ Until i T f
  Globally i f -> minOp $ Not (Future i (Not f))

-- try to convert back a formula expressed with the minimal amount of operators
-- to use all allowed operators
maxOp :: Formula -> Formula
maxOp = undefined

-- | convert a MTL formula to NNF (negation normal form) - all
-- negations are only before atoms. Expects the input formula to only use
-- the minimal set of operators fot MTL (use minOp on formula first)
toNNF :: Formula -> Formula
toNNF f = case f of
    Atom p -> Atom p
    Not f' ->
    And f1 f2 ->
    Next 
  where
    dualUntil i f1 f2 = Not $ Until i (Not f1) (Not f2)

-- | convert a MTL formula to FNF (flat normal form) --
-- there are no nested temporal operators.
toFNF :: Formula -> Formula
toFNF = undefined

-- | convert a MTL formula (over naturals) to a LTL formula,
-- using the gap translation and strict semantics of https://doi.org/10.1007/978-3-319-63046-5_20
toLTL :: Formula -> LTL.Formula
toLTL = undefined
