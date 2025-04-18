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
  deriving (Show, Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

-- | pretty print an LTL formula. All external solvers accept
-- this syntax as well.
pretty :: Formula -> String
pretty f = case f of
    Atom p        -> p
    Not f         -> "!(" <> pretty f <> ")"
    Or f1 f2      -> binopstr f1 "|" f2
    And f1 f2     -> binopstr f1 "&" f2
    Implies f1 f2 -> binopstr f1 "->" f2
    Next f        -> "X(" <> pretty f <> ")"
    Future f      -> "F(" <> pretty f <> ")"
    Globally f    -> "G(" <> pretty f <> ")"
    Until f1 f2   -> binopstr f1 "U" f2
  where
    binopstr f1 op f2 =
      "(" <> pretty f1 <> ") " <> op <> " (" <> pretty f2 <> ")"


