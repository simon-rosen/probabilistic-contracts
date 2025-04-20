module Generate.LTL where
import           Specs.LTL
import           System.Random (randomRIO)

-- generate a random LTL formula with approx `parseTreeSize` nodes
-- in its parse tree and with the number of allowed atoms limited by `atomUnivSize`
random :: Int -> Int -> IO Formula
random parseTreeSize atomUnivSize = go parseTreeSize
  where
    -- setup the allowed atoms
    allowedAtoms = zipWith (\p n -> p <> show n) (repeat "p") [1..atomUnivSize]
    -- function to get a random atom from the allowed ones
    randomAtom = do
      i <- randomRIO (0, atomUnivSize - 1)
      let name = allowedAtoms !! i
      pure $ Atom name
    -- function to split up the size of the left and right parse tree in a
    -- tree of size n with root as a binary operator
    splitN :: Int -> IO (Int, Int)
    splitN n = do
      n' <- randomRIO (0, n)
      pure (n', n - n')
    -- generate an LTL formula of parseTreeSize n using the allowed atoms
    go :: Int -> IO Formula
    go n
      -- base case: the leaves of the LTL parse tree are atoms
      | n <= 0 = randomAtom
      -- recursive case: pick operators at random and recurse, with
      -- binary operators the left and right subtrees get a randomly
      -- partitioned size.
      | otherwise = do
          op <- randomRIO (1, 8) :: IO Int
          -- partition left and right tree size for binops
          (left, right) <- splitN (n-1)
          case op of
            1 -> Not <$> go (n-1)
            2 -> Or <$> (go left) <*> (go right)
            3 -> And <$> (go left) <*> (go right)
            4 -> Implies <$> (go left) <*> (go right)
            5 -> Next <$> go (n-1)
            6 -> Future <$> go (n-1)
            7 -> Globally <$> go (n-1)
            8 -> Until <$> (go left) <*> (go right)

