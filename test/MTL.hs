-- testing translations from MTL over naturals to LTL
import           Math
import           Specs.MTL

-- example from the article mentioned in the Specs.MTL module
example1 = Next (Nat 2, Nat 3) (Not (Globally (Nat 1, Nat 2) (Atom "q")))

