-- common mathematical things that we want to use in other modules
module Math where

-- | a class for representing things that can be complemented
class Complementable a where
  complement :: a -> a

-- | a type for expressing comparasion operations
data Compare = Less | Leq | Eq | Neq | Geq | Greater
             deriving (Eq)

mkCompare :: String -> Compare
mkCompare r = case r of
  "<"  -> Less
  "<=" -> Leq
  "="  -> Eq
  "!=" -> Neq
  ">=" -> Geq
  ">"  -> Greater

instance Complementable Compare where
  complement c = case c of
    Less    -> Geq
    Leq     -> Greater
    Eq      -> Neq
    Neq     -> Eq
    Geq     -> Less
    Greater -> Leq

instance Show Compare where
  show r = case r of
    Less    -> "<"
    Leq     -> "<="
    Eq      -> "="
    Neq     -> "!="
    Geq     -> ">="
    Greater -> ">"

-- | a type for expressing variables in equations, we just alias Int for this
newtype Var = Var (Int)

instance Show Var where
  show = showVar "x"

-- | show a variable with a custom prefix
showVar :: String -> Var -> String
showVar letter (Var i) = letter <> show i

-- | a type for representing probability statements, ex: P(x) = p
data Probability x = Probability x Compare Double

-- | complement a probabilistic statement: P(x) < p becomes P(x) >= p
instance Complementable Probability where
  complement (Probability x r p) = Probability x (complement r) p

instance Show x => Show (Probability x) where
  show (Probability x r p) = "P(" <> show x <> ") " <> show r <> " " <> show p

-- The types of linear equations we want to express
data LinearIneq =
    SumTo1 [Var] -- all vars sum to 1
  | NonNeg Var -- a var is >= 0
  -- the equation expressing the probability of a contract
  -- ex: z1 + z2 <= 0.5 * (z1 + z2 + z3)
  | ContractEq [Var] Compare Double [Var] -- the equation expressing the probability of a specific contract
  deriving (Show)


