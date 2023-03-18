-- | Functionality for accumulating a sequence of elements and
-- computing a "final" result from the accumulated "sum"
--
-- Import qualified!

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.Utils.Accumulator
(
  Accumulator,
  accumulatorOf,
  add,
  resultOf,

  -- * Computing the sum and result

  sum,
  resultOfSum,

  -- * Some concrete accumulators

  constantOf,
  accumulatorWithCount,
  countAccumulator,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Prelude hiding (sum)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Summarizes a sequence of elements of type a,
-- giving a final result of type r that depends on the sum of a:s
data Accumulator a r = Accumulator
  {
    sAdd    :: a -> Accumulator a r
  , sResult :: r
  }

add :: Accumulator a r -> a -> Accumulator a r
add = sAdd

resultOf :: Accumulator a r -> r
resultOf = sResult

sum :: Accumulator a r -> [a] -> Accumulator a r
sum zero xs = foldl add zero xs

resultOfSum :: Accumulator a r -> [a] -> r
resultOfSum zero xs = resultOf $ sum zero xs

-- | Constructs an accumulator
accumulatorOf
  :: s
  -> (s -> a -> s)
  -> (s -> r)
  -> Accumulator a r
accumulatorOf zeroS addS getResult =
  Accumulator
  {
    sAdd    = \x -> accumulatorOf (addS zeroS x) addS getResult
  , sResult = getResult zeroS
  }

-- | An accumulator that gives a constant result
constantOf :: r -> Accumulator a r
constantOf result = accumulatorOf () (\_ _ -> ()) (const result)

-- | A Accumulator that gives the "result computation"
-- access to the number of added elements.
accumulatorWithCount
  :: forall s a r.
     s
  -> (s -> a -> s)
  -> ((Int, s) -> r)
  -> Accumulator a r
accumulatorWithCount zeroS addS getResult = accumulatorOf zeroS' addS' getResult
  where
    zeroS' :: (Int, s)
    zeroS'  = (0, zeroS)

    addS'  :: (Int, s) -> a -> (Int, s)
    addS' (n, s) a = (succ n, addS s a)

-- | A Accumulator that gives the "result computation"
-- access to the number of added elements.
countAccumulator
  :: (Int -> r)
  -> Accumulator a r
countAccumulator getResult = accumulatorOf 0 add getResult
  where
    add    :: Int -> a -> Int
    add n _ = succ n
