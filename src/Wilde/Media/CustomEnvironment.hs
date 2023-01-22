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
