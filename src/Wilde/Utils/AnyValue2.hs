{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-------------------------------------------------------------------------------
-- | Container that hides two type parameters of a type.
--
-- Usage: Import qualified.
-------------------------------------------------------------------------------
module Wilde.Utils.AnyValue2
       (
         Container(..),
         apply,
         apply2,
         applyM,
       )
       where

data Container t = forall a1 a2 . Container (t a1 a2)

-- | Applies a function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Container'.
apply :: (forall a1 a2 . t a1 a2 -> b)
      -> Container t
      -> b
apply f (Container x) = f x

apply2 :: (forall a1 a2 . t a1 a2 -> u b1 b2)
       -> Container t
       -> Container u
apply2 f (Container x) = Container (f x)

-- | Applies a \"monadic\" function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Container'.
applyM :: Monad m
       => (forall a1 a2 . t a1 a2 -> m (u b1 b2))
       -> Container t
       -> m (Container u)
applyM f (Container x) =
  do
    x' <- f x
    pure $ Container x'
