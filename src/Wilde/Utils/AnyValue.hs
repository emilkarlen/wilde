{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

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
    return $ Container x'
