module Logics.LTL where
import           Logics.Operations
import           System.Process    (readProcess)

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

-- | pretty print a formula
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


instance LogicOperations Formula where
  lNeg f = Not f
  lAnd f1 f2 = And f1 f2
  lSat = sat

-- | solve satisfiability of a LTL formula using the command line tool
-- "black"
sat :: Formula -> IO (Either String Bool)
sat ltl = do
  res <- readProcess "black" ["solve", "-f", pretty ltl] ""
  pure $ case res of
    "SAT\n"   -> Right True
    "UNSAT\n" -> Right False
    _         -> Left res


