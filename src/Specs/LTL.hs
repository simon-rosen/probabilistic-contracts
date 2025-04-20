module Specs.LTL where
import           Math

-- | A LTL formula
data Formula =
  Atom String -- ^ p
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


