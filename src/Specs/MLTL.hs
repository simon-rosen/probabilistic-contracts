-- | MLTL - Mission-time linear temporal logic
-- This is like MTL-over-naturals but over finite traces
-- and slightly different semantics. Also MLTL does not have
-- a next (X) operator.
module Specs.MLTL where
import           Control.Exception
import           Control.Exception     (IOException, SomeException, bracket,
                                        catch, evaluate, finally, try)
import           Control.Monad         (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Math
import           Parse.LTLParser       (parseLTL)
import qualified Specs.LTL             as LTL
import           System.Exit           (ExitCode (..))
import           System.IO
import           System.Process

import           Control.Exception     (SomeException, evaluate, try)
import qualified Data.ByteString.Char8 as BS8
import           System.Exit           (ExitCode (..))
import           System.Process        (readProcessWithExitCode)

-- all temporal operators have bounded intervals associated
type Interval = (Int, Int)

-- | A MLTL formula
data Formula =
    Top -- always true
  | Bottom -- always false
  | Atom String -- ^ p
  | Not Formula -- ^ !phi
  | Or Formula Formula -- ^ phi1 | phi2
  | And Formula Formula -- ^ phi1 & phi2
  | Implies Formula Formula -- ^ phi1 -> phi2
  | Future Interval Formula -- F phi
  | Globally Interval Formula -- G phi
  | Until Interval Formula Formula --phi1 U phi2
  deriving (Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

instance Show Formula where
  show f = case f of
    Top -> "(top | !top)"
    Bottom -> "(bottom & !bottom)"
    Atom name -> name
    Not f' -> "!(" <> show f' <>")"
    Or f1 f2 -> "(" <> show f1 <> ") | (" <> show f2 <> ")"
    And f1 f2 -> "(" <> show f1 <> ") & (" <> show f2 <> ")"
    Implies f1 f2 -> "(" <> show f1 <> ") -> (" <> show f2 <> ")"
    Future (a, b) f' -> "F[" <> show a <> "," <> show b <>"](" <> show f' <>")"
    Globally (a, b) f' -> "G[" <> show a <> "," <> show b <>"](" <> show f' <>")"
    Until (a, b) f1 f2 -> "(" <> show f1 <> ") U[" <> show a <> "," <> show b <>  "](" <> show f2 <> ")"


-- | convert a MLTL formula to LTL, by using an external tool called MLTLConvertor
toLTL :: Formula -> IO (Either String LTL.Formula)
toLTL f = do
  (exitCode, out, err) <- readProcessWithExitCode "MLTLConvertor" ["-ltl", show f] ""

  case exitCode of
    ExitFailure code ->
      pure $ Left $ "MLTLConvertor failed with code " ++ show code ++ ": " ++ err

    ExitSuccess -> do
      parsed <- try (evaluate (parseLTL out)) :: IO (Either SomeException LTL.Formula)
      pure $ case parsed of
        Right ltl -> Right ltl
        Left ex   -> Left $ "Failed to parse LTL: " ++ show ex

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
  Future _ f1   -> 1 + totalSize f1
  Globally _ f1 -> 1 + totalSize f1
  Until _ f1 f2 -> 1 + totalSize f1 + totalSize f2

-- | Count the max depth of the formula
maxTotalDepth :: Formula -> Int
maxTotalDepth f = case f of
  Top           -> 0
  Bottom        -> 0
  Atom _        -> 0
  Not f1        -> maxTotalDepth f1
  Or f1 f2      -> max (maxTotalDepth f1) (maxTotalDepth f2)
  And f1 f2     -> max (maxTotalDepth f1) (maxTotalDepth f2)
  Implies f1 f2 -> max (maxTotalDepth f1) (maxTotalDepth f2)
  Future _ f1   -> 1 + maxTotalDepth f1
  Globally _ f1 -> 1 + maxTotalDepth f1
  Until _ f1 f2 -> 1 + max (maxTotalDepth f1) (maxTotalDepth f2)


-- | Count the number of temporal operators in an MLTL formula
numTemporalOperators :: Formula -> Int
numTemporalOperators f = case f of
  Top           -> 0
  Bottom        -> 0
  Atom _        -> 0
  Not f1        -> numTemporalOperators f1
  Or f1 f2      -> numTemporalOperators f1 + numTemporalOperators f2
  And f1 f2     -> numTemporalOperators f1 + numTemporalOperators f2
  Implies f1 f2 -> numTemporalOperators f1 + numTemporalOperators f2
  Future _ f1   -> 1 + numTemporalOperators f1
  Globally _ f1 -> 1 + numTemporalOperators f1
  Until _ f1 f2 -> 1 + numTemporalOperators f1 + numTemporalOperators f2

-- | Compute the maximum nesting depth of temporal operators in MLTL
maxTemporalDepth :: Formula -> Int
maxTemporalDepth f = case f of
  Top           -> 0
  Bottom        -> 0
  Atom _        -> 0
  Not f1        -> maxTemporalDepth f1
  Or f1 f2      -> max (maxTemporalDepth f1) (maxTemporalDepth f2)
  And f1 f2     -> max (maxTemporalDepth f1) (maxTemporalDepth f2)
  Implies f1 f2 -> max (maxTemporalDepth f1) (maxTemporalDepth f2)
  Future _ f1   -> 1 + maxTemporalDepth f1
  Globally _ f1 -> 1 + maxTemporalDepth f1
  Until _ f1 f2 -> 1 + max (maxTemporalDepth f1) (maxTemporalDepth f2)

