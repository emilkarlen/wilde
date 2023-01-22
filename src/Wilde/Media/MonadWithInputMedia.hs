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
