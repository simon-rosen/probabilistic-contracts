-- | MLTL - Mission-time linear temporal logic
-- This is like MTL-over-naturals but over finite traces
-- and slightly different semantics. Also MLTL does not have
-- a next (X) operator.
module Specs.MLTL where
import           Control.Exception (bracket)
import           Math
import           System.IO
import           System.Process    (readProcessWithExitCode)

-- all temporal operators have bounded intervals associated
type Interval = (Int, Int)

-- | A MLTL formula
data Formula =
  Atom String -- ^ p
  | Not Formula -- ^ !phi
  | Or Formula Formula -- ^ phi1 | phi2
  | And Formula Formula -- ^ phi1 & phi2
  | Implies Formula Formula -- ^ phi1 -> phi2
  | Future Interval Formula -- F phi
  | Globally Interval Formula -- G phi
  | Until Interval Formula Formula --phi1 U phi2
  deriving (Show, Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

------------ conversion to LTL -------------------------
pretty :: Formula -> String
pretty f = case f of
  Atom name -> name
  Not f' -> "!(" <> pretty f' <>")"
  Or f1 f2 -> "(" <> pretty f1 <> ") | (" <> pretty f2 <> ")"
  And f1 f2 -> "(" <> pretty f1 <> ") & (" <> pretty f2 <> ")"
  Implies f1 f2 -> "(" <> pretty f1 <> ") -> (" <> pretty f2 <> ")"
  Future (a, b) f' -> "F[" <> show a <> "," <> show b <>"](" <> pretty f' <>")"
  Globally (a, b) f' -> "G[" <> show a <> "," <> show b <>"](" <> pretty f' <>")"
  Until (a, b) f1 f2 -> "(" <> pretty f1 <> ") U[" <> show a <> "," <> show b <>  "](" <> pretty f2 <> ")"

toLTLString :: Formula -> IO String
toLTLString f = do
  (_code, ltl, _err) <- readProcessWithExitCode "MLTLConvertor" ["-ltl", pretty f] ""
  pure ltl

