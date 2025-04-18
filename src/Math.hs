-- common mathematical things that we want to use in other modules
module Math where

-- | a class for representing things that can be complemented
class Complementable a where
  complement :: a -> a

-- a class for representing things which can be intersected
class Intersectable a where
  intersect :: a -> a -> a

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

-- | a type for expressing variables in equations,
-- we just alias Integer for this
data Var = Var String | NumberedVar String Integer
            deriving (Eq, Ord)

instance Show Var where
  show v = case v of
    Var name           -> name
    NumberedVar name i -> name <> show i

-- | a type for representing probability statements, ex: P(x) = p
data Probability x = Probability x Compare Double
                   deriving (Eq)

-- | complement a probabilistic statement: P(x) < p becomes P(x) >= p
instance Complementable (Probability x) where
  complement (Probability x r p) = Probability x (complement r) p

instance Show x => Show (Probability x) where
  show (Probability x r p) = "P(" <> show x <> ") " <> show r <> " " <> show p

