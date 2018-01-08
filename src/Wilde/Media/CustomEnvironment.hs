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

-- | A custom environment of (key,value) for applications.
module Wilde.Media.CustomEnvironment
       (
         MonadWithCustomEnvironment(..),
         MonadWithCustomEnvironmentAndLookup(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.ElementSet


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A monad with Custom Environment.
class Monad m => MonadWithCustomEnvironment m where
  getCustomEnvironment      :: m ElementSet

-- | A monad with Custom Environment and
-- support for lookup of elements in that set.
class MonadWithCustomEnvironment m => MonadWithCustomEnvironmentAndLookup m where
  inCustomEnvironment :: Lookuper a -> m a
