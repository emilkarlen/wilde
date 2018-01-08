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

-- | Classes for monads with access to the \"input media\"
module Wilde.Media.MonadWithInputMedia
       (
         MonadWithInputMedia(..) ,
         MonadWithInputMediaAndLookup(..),
         
         withInputMedia,
         inInputMedia_raw,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.ElementSet as ES


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A monad with Custom Environment.
class Monad m => MonadWithInputMedia m where
  getInputMedia :: m ES.ElementSet

-- | A monad with Custom Environment and
-- support for lookup of elements in that set.
class MonadWithInputMedia m => MonadWithInputMediaAndLookup m where
  inInputMedia :: ES.Lookuper a -> m a

-- | Short cut for a computation 
withInputMedia :: MonadWithInputMedia m
               => (ES.ElementSet -> m a)
               -> m a
withInputMedia f = getInputMedia >>= f

inInputMedia_raw :: MonadWithInputMedia m
                 => ES.Lookuper a
                 -> m (ES.LookupResult a)
inInputMedia_raw lookuper = getInputMedia >>= (return . lookuper)
