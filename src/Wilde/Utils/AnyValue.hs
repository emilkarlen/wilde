{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Wilde.Utils.AnyValue
       (
         Container(..),
         apply,
         apply2,
         applyM,
       )
       where

data Container t = forall a . Container (t a)

-- | Applies a function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Container'.
apply :: (forall a . t a -> b)
      -> Container t
      -> b
apply f (Container x) = f x

apply2 :: (forall a . t a -> u b)
       -> Container t
       -> Container u
apply2 f (Container x) = Container (f x)

-- | Applies a \"monadic\" function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Container'.
applyM :: Monad m
       => (forall a . t a -> m (u b))
       -> Container t
       -> m (Container u)
applyM f (Container x) =
  do
    x' <- f x
    pure $ Container x'
