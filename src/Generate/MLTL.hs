module Generate.MLTL where
import           Specs.MLTL
import           System.Random (randomRIO)

-- generate a random MLTL formula with approx `parseTreeSize` nodes
-- in its parse tree, with the number of allowed atoms limited by `atomUnivSize`
-- , and with the interval of temporal operators bounded by [0, `maxTime`]
random :: Int -> [Formula]-> Int -> IO Formula
random parseTreeSize allowedAtoms maxTime = go parseTreeSize
  where
    -- function to get a random atom from the allowed ones
    randomAtom = do
      i <- randomRIO (0, (length allowedAtoms) - 1)
      pure $ allowedAtoms !! i
    -- function to split up the size of the left and right parse tree in a
    -- tree of size n with root as a binary operator
    splitN :: Int -> IO (Int, Int)
    splitN n = do
      n' <- randomRIO (0, n)
      pure (n', n - n')
    -- generate a random interval
    randomInterval upper = do
      a <- randomRIO (0, upper)
      b <- randomRIO (a, upper)
      pure (a, b)
    -- generate an LTL formula of parseTreeSize n using the allowed atoms
    go :: Int -> IO Formula
    go n
      -- base case: the leaves of the LTL parse tree are atoms
      | n <= 0 = randomAtom
      -- recursive case: pick operators at random and recurse, with
      -- binary operators the left and right subtrees get a randomly
      -- partitioned size.
      | otherwise = do
          op <- randomRIO (1, 7) :: IO Int
          -- partition left and right tree size for binops
          (left, right) <- splitN (n-1)
          -- generate random interval
          interval <- randomInterval maxTime
          case op of
            1 -> Not <$> go (n-1)
            2 -> Or <$> (go left) <*> (go right)
            3 -> And <$> (go left) <*> (go right)
            4 -> Implies <$> (go left) <*> (go right)
            5 -> Future interval <$> go (n-1)
            6 -> Globally interval <$> go (n-1)
            7 -> Until interval <$> (go left) <*> (go right)


-- generate a random LTL formula based on common specification patterns
-- these are inspired by "https://dl.acm.org/doi/10.1145/1368088.1368094"
randomCommonPattern :: Int -> [Formula] -> Int -> IO Formula
randomCommonPattern size allowedAtoms maxTime = go size
   where
    -- function to get a random atom from the allowed ones
    randomAtom = do
      i <- randomRIO (0, (length allowedAtoms) - 1)
      pure $ allowedAtoms !! i
    -- function to split up the size of the left and right parse tree in a
    -- tree of size n with root as a binary operator
    splitN :: Int -> IO (Int, Int)
    splitN n = do
      n' <- randomRIO (0, n)
      pure (n', n - n')
    -- generate a random interval
    randomInterval upper = do
      a <- randomRIO (0, upper)
      b <- randomRIO (a, upper)
      pure (a, b)
    -- generate just the propositional formula
    goPhi :: Int -> IO Formula
    goPhi n
      -- base case: the leaves of the LTL parse tree are atoms
      | n <= 0 = randomAtom
      -- recursive case: pick operators at random and recurse, with
      -- binary operators the left and right subtrees get a randomly
      -- partitioned size.
      | otherwise = do
          op <- randomRIO (1, 4) :: IO Int
          -- partition left and right tree size for binops
          (left, right) <- splitN (n-1)
          -- generate random interval
          interval <- randomInterval maxTime
          case op of
            1 -> Not <$> goPhi (n-1)
            2 -> Or <$> (goPhi left) <*> (goPhi right)
            3 -> And <$> (goPhi left) <*> (goPhi right)
            4 -> Implies <$> (goPhi left) <*> (goPhi right)
    -- generate one of the common specification patterns
    -- using the propositional logic formula
    go :: Int -> IO Formula
    go n = do
      phi1 <- goPhi n
      phi2 <- goPhi n
      op <- randomRIO (1, 3) :: IO Int
      -- generate random interval
      interval <- randomInterval maxTime
      interval2 <- randomInterval maxTime
      pure $ case op of
        -- probabilistic existence
        1 -> Future interval phi1
        -- probabilistic until
        2 -> Until interval phi1 phi2
        -- probabilistic response
        3 -> Globally interval (phi1 `Implies` (Future interval2 phi2))






