module Specs.LTL where
import           Math

-- | A LTL formula
data Formula =
    Top -- always true
  | Bottom -- always false
  | Atom String -- ^ p
  | Not Formula -- ^ !phi
  | Or Formula Formula -- ^ phi1 | phi2
  | And Formula Formula -- ^ phi1 & phi2
  | Implies Formula Formula -- ^ phi1 -> phi2
  | Next Formula -- X phi
  | Future Formula -- F phi
  | Globally Formula -- G phi
  | Until Formula Formula --phi1 U phi2
  deriving (Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

-- | instancing show for LTL formulas with the same
-- syntacx that is accepted by the external solvers.
instance Show Formula where
  show f = case f of
      Top           -> "(top | !top)"
      Bottom        -> "(bottom & !bottom)"
      Atom p        -> p
      Not f         -> "!(" <> show f <> ")"
      Or f1 f2      -> binopstr f1 "|" f2
      And f1 f2     -> binopstr f1 "&" f2
      Implies f1 f2 -> binopstr f1 "->" f2
      Next f        -> "X(" <> show f <> ")"
      Future f      -> "F(" <> show f <> ")"
      Globally f    -> "G(" <> show f <> ")"
      Until f1 f2   -> binopstr f1 "U" f2
    where
      binopstr f1 op f2 =
        "(" <> show f1 <> ") " <> op <> " (" <> show f2 <> ")"


-- | Count the size of the formula (number of nodes in the parse tree, including leafs)
totalSize :: Formula -> Int
totalSize f = case f of
  Top           -> 1
  Bottom        -> 1
  Atom _        -> 1
  Not f1        -> 1 + totalSize f1
  Or f1 f2      -> 1 + totalSize f1 + totalSize f2
  And f1 f2     -> 1 + totalSize f1 + totalSize f2
  Implies f1 f2 -> 1 + totalSize f1 + totalSize f2
  Next f1       -> 1 + totalSize f1
  Future f1     -> 1 + totalSize f1
  Globally f1   -> 1 + totalSize f1
  Until f1 f2   -> 1 + totalSize f1 + totalSize f2

-- | Count the max depth of the formula
maxTotalDepth :: Formula -> Int
maxTotalDepth f = case f of
  Top           -> 0
  Bottom        -> 0
  Atom _        -> 0
  Not f1        -> 1 + maxTotalDepth f1
  Or f1 f2      -> 1 + max (maxTotalDepth f1) (maxTotalDepth f2)
  And f1 f2     -> 1 + max (maxTotalDepth f1) (maxTotalDepth f2)
  Implies f1 f2 -> 1 + max (maxTotalDepth f1) (maxTotalDepth f2)
  Next f1       -> 1 + maxTotalDepth f1
  Future f1     -> 1 + maxTotalDepth f1
  Globally f1   -> 1 + maxTotalDepth f1
  Until f1 f2   -> 1 + max (maxTotalDepth f1) (maxTotalDepth f2)

-- | The number of temporal operators in a formula
numTemporalOperators :: Formula -> Int
numTemporalOperators f = case f of
  Top           -> 0
  Bottom        -> 0
  Atom _        -> 0
  Not f1        -> numTemporalOperators f1
  Or f1 f2      -> numTemporalOperators f1 + numTemporalOperators f2
  And f1 f2     -> numTemporalOperators f1 + numTemporalOperators f2
  Implies f1 f2 -> numTemporalOperators f1 + numTemporalOperators f2
  Next f1       -> 1 + numTemporalOperators f1
  Future f1     -> 1 + numTemporalOperators f1
  Globally f1   -> 1 + numTemporalOperators f1
  Until f1 f2   -> 1 + numTemporalOperators f1 + numTemporalOperators f2

-- | the maximum
maxTemporalDepth :: Formula -> Int
maxTemporalDepth f = case f of
  Top           -> 0
  Bottom        -> 0
  Atom _        -> 0
  Not f1        -> maxTemporalDepth f1
  Or f1 f2      -> max (maxTemporalDepth f1) (maxTemporalDepth f2)
  And f1 f2     -> max (maxTemporalDepth f1) (maxTemporalDepth f2)
  Implies f1 f2 -> max (maxTemporalDepth f1) (maxTemporalDepth f2)
  Next f1       -> 1 + maxTemporalDepth f1
  Future f1     -> 1 + maxTemporalDepth f1
  Globally f1   -> 1 + maxTemporalDepth f1
  Until f1 f2   -> 1 + max (maxTemporalDepth f1) (maxTemporalDepth f2)


