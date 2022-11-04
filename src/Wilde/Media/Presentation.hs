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
{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- | Media and monad for presentation.
--
-- Import qualified.
module Wilde.Media.Presentation
  ( module ES,
    module Wilde.Media.CustomEnvironment,

    -- * The monad
    Monad,
    Result,
    Error (..),

    -- * Monad Execution
    run,

    -- * Monad Environment
    Environment (..),
    Outputing (..),
    CustomServiceLinkRenderer,
    envStandardServiceLinkRenderer,
    getCustomServiceLinkRenderer,
    getEnvs,

    -- * Error handling
    throwErr,
    catchErr,
    ToPresentationError (..),
    liftIOWithError,

    -- * Utilities
    ToPresentationMonad (..),
    toPresentationMonadWithConn,
    toPresentationMonadWithCar,
  )
where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import qualified Control.Monad as MMonad
import qualified Control.Monad.Trans as MTrans
import qualified Control.Monad.Trans.Except as MExcept
import qualified Control.Monad.Trans.Reader as MReader
import Database.HDBC.Types (ConnWrapper)
import Wilde.Application.ServiceLink
import qualified Wilde.Application.StandardServices as StandardServices
import qualified Wilde.Database.Executor as DbExecutor
import Wilde.Media.CustomEnvironment
import Wilde.Media.Database hiding (ObjectModelError)
import qualified Wilde.Media.Database.Monad as DBIO
import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.Translations
import Wilde.Media.WildeMedia
import qualified Wilde.Media.WildeValue as WildeValue
import Wilde.WildeUi.StdValueTypes
import Prelude hiding (Monad)

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- - Errors -
-------------------------------------------------------------------------------

-- | All types of errors that can occurr.
data Error
  = DatabaseError DatabaseError
  | UnclassifiedError UnclassifiedError
  | ObjectModelError String
  | MediaLookupError ES.ElementLookupError
  | ImplementationError String
  deriving (Show)

-------------------------------------------------------------------------------
-- - Environment and monad -
-------------------------------------------------------------------------------

type Result a = Either Error a

data Environment = Environment
  { envCustomEnvironment :: ES.ElementSet,
    envDbConfiguration :: DbExecutor.Configuration,
    envOutputing :: Outputing
  }

-- | Renders a link to a global service.
type CustomServiceLinkRenderer = ServiceSpecification -> WildeStyling LinkLabel -> [GenericParameter] -> WildeValue.AnySVALUE

-- | Part of the 'Environment' that contains
-- functionality for outputing.
data Outputing = Outputing
  { outTranslations :: Translations,
    -- | Renderer for the \"standard\" services.
    --
    -- NOTE: See "Wilde.Application.StandardServices".
    outStandardServiceLinkRenderer :: StandardServices.StandardServiceLinkRenderer,
    outGetCustomServiceLinkRenderer :: Monad CustomServiceLinkRenderer
  }

-- | Gets the 'StandardServices.StandardServiceLinkRenderer' from the
-- 'Environment'.
envStandardServiceLinkRenderer ::
  Environment ->
  StandardServices.StandardServiceLinkRenderer
envStandardServiceLinkRenderer = outStandardServiceLinkRenderer . envOutputing

-- | Gets the 'CustomServiceLinkRenderer'.
getCustomServiceLinkRenderer :: Monad CustomServiceLinkRenderer
getCustomServiceLinkRenderer = do
  getIt <- getEnvs $ outGetCustomServiceLinkRenderer . envOutputing
  getIt

newtype Monad a
  = Monad (MExcept.ExceptT Error (MReader.ReaderT Environment IO) a)

instance MonadWithCustomEnvironment Monad where
  getCustomEnvironment = fmap envCustomEnvironment getEnv

instance MonadWithCustomEnvironmentAndLookup Monad where
  inCustomEnvironment = ES.integrateLookup integration
    where
      integration =
        ES.ElementSetMonadIntegration
          { ES.getElementSet = getCustomEnvironment,
            ES.throwError = throwElementLookupError
          }

throwElementLookupError :: ES.ElementLookupError -> Monad a
throwElementLookupError err = throwErr $ MediaLookupError err

-- | Class for the error "sub types" of Error.
class ToPresentationError a where
  toError :: a -> Error

instance ToPresentationError Error where
  toError = id

instance ToPresentationError TranslationError where
  toError = DatabaseError . DBIO.toDatabaseError

instance ToPresentationError DatabaseError where
  toError = DatabaseError

instance ToPresentationError UnclassifiedError where
  toError = UnclassifiedError

instance ToPresentationError GeneralError where
  toError (GeneralUnclassifiedError s) = UnclassifiedError (unclassifiedError s)
  toError (GeneralObjectModelError s) = ObjectModelError s

instance ToPresentationError ObjectAndObjectTypeMismatchError where
  toError (ObjectAndObjectTypeMismatchError descr cause) =
    ImplementationError $ descr ++ ": " ++ show cause

instance MMonad.Monad Monad where
  return = Monad . return
  (Monad m) >>= f = Monad $
    do
      a <- m
      let Monad m' = f a
      m'

instance Applicative Monad where
  pure = Monad . pure
  (Monad ma) <*> (Monad mb) = Monad $ ma <*> mb

instance Functor Monad where
  fmap f (Monad m) = Monad $ fmap f m

instance MTrans.MonadIO Monad where
  liftIO = Monad . MTrans.lift . MTrans.lift

instance DbExecutor.MonadWithDatabaseConfiguration Monad where
  getDatabaseConfiguration = getEnvs envDbConfiguration

-- | \"Computations\" (e.g. monads) that are instances of this class
-- can be integrated into the 'Monad'.
class ToPresentationMonad m where
  toPresentationMonad :: m a -> Monad a

-- | Runs a 'Monad' computation.
run ::
  Environment ->
  Monad a ->
  IO (Result a)
run env (Monad errT) = MReader.runReaderT (MExcept.runExceptT errT) env

getEnv :: Monad Environment
getEnv = Monad $ MTrans.lift MReader.ask

-- | Gets the environment of the 'Monad'.
getEnvs :: (Environment -> a) -> Monad a
getEnvs = Monad . MTrans.lift . MReader.asks

-- | Corresponds to 'Control.Monad.Trans.Error's throwError.
throwErr ::
  ToPresentationError err =>
  err ->
  Monad a
throwErr err = Monad $ MExcept.throwE (toError err)

-- | Corresponds to 'Control.Monad.Trans.Error's catchError.
catchErr ::
  -- | The computation that can throw an error.
  Monad a ->
  -- | Error handler
  (Error -> Monad a) ->
  Monad a
catchErr m handler =
  let (Monad errT) = m
      handlerErrT err =
        let (Monad errT) = handler err
         in errT
   in Monad $ MExcept.catchE errT handlerErrT

instance ToPresentationError err => ToPresentationMonad (Either err) where
  toPresentationMonad (Left err) = throwErr err
  toPresentationMonad (Right ok) = return ok

instance
  ToPresentationError err =>
  ToPresentationMonad (MExcept.ExceptT err IO)
  where
  toPresentationMonad m =
    do
      res <- MTrans.liftIO $ MExcept.runExceptT m
      toPresentationMonad res

instance ToPresentationMonad DBIO.DatabaseMonad where
  toPresentationMonad m =
    do
      custEnv <- getCustomEnvironment
      res <- MTrans.liftIO $ DBIO.runDatabase custEnv m
      toPresentationMonad res

-- | Integrates monads of type "IO (Either err a)"
-- into the Monad monad
liftIOWithError ::
  ToPresentationError err =>
  IO (Either err a) ->
  Monad a
liftIOWithError io =
  do
    res <- MTrans.liftIO io
    case res of
      Left err -> throwErr err
      Right ok -> return ok

toPresentationMonadWithConn ::
  (ConnWrapper -> DBIO.DatabaseMonad a) ->
  Monad a
toPresentationMonadWithConn f =
  do
    conn <- DbExecutor.getDatabaseConnection
    toPresentationMonad $ f conn

toPresentationMonadWithCar ::
  (DbExecutor.ConnectionAndRenderer -> DBIO.DatabaseMonad a) ->
  Monad a
toPresentationMonadWithCar f =
  do
    car <- DbExecutor.getDatabaseConnectionAndRenderer
    toPresentationMonad $ f car
