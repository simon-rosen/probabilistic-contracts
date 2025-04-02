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

