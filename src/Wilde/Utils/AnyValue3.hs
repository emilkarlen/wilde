{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-------------------------------------------------------------------------------
-- | Container that hides three type parameters of a type.
--
-- Usage: Import qualified.
-------------------------------------------------------------------------------
module Wilde.Utils.AnyValue3
       (
         Container(..),
         apply,
         apply2,
         applyM,
       )
       where

data Container t = forall a1 a2 a3 . Container (t a1 a2 a3)

-- | Applies a function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Container'.
apply :: (forall a1 a2 a3 . t a1 a2 a3 -> b)
      -> Container t
      -> b
apply f (Container x) = f x

apply2 :: (forall a1 a2 a3 . t a1 a2 a3 -> u b1 b2 b3)
       -> Container t
       -> Container u
apply2 f (Container x) = Container (f x)

-- | Applies a \"monadic\" function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Container'.
applyM :: Monad m
       => (forall a1 a2 a3 . t a1 a2 a3 -> m (u b1 b2 b3))
       -> Container t
       -> m (Container u)
applyM f (Container x) =
  do
    x' <- f x
    return $ Container x'
