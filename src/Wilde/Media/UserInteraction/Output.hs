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
{-# LANGUAGE TypeSynonymInstances #-}

-- | Monad for generating User Interaction Output - information for producing a
-- User Interface that lets the user input information to the program.
module Wilde.Media.UserInteraction.Output
       (
         module Wilde.Media.UserInteraction,
         module ES,
         module Wilde.Media.CustomEnvironment,
         module Wilde.Media.MonadWithInputMedia,
         
         -- * Types
         
         WidgetConstructorGetter,
         WidgetConstructorForObjectWithDefault,
         -- * The monad
         
         UserInteractionOutputMonad,
         UserInteractionOutputResult,
         UserInteractionOutputError(..),
         Presentation.Error(..),
         
         -- * Monad Execution
         
         run,

         -- * Monad Environment
         
         UserInteractionOutputEnvironment(..),
         Presentation.Outputing(..),
         PopUpButtonTexter,
         
         envButtonTexter,
         envStandardServiceLinkRenderer,
         
         getEnvs,
         
         -- * Error handling
         
         throwErr,
         catchErr,
         liftIOWithError,
         
         -- * Utilities
         
         ToUserInteractionOutputMonad(..),
         
         toUserInteractionOutputMonadWithConn,
         toUserInteractionOutputMonadWithCar,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader

import Database.HDBC.Types (ConnWrapper)

import qualified Wilde.Database.Executor as DbExecutor

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.CustomEnvironment
import Wilde.Media.UserInteraction
import qualified Wilde.Media.Database.Monad as DBIO
import qualified Wilde.Media.Presentation as Presentation

import Wilde.Media.MonadWithInputMedia

import qualified Wilde.Application.PopUp as PopUp
import           Wilde.Media.Translations
import qualified Wilde.Application.StandardServices as StandardServices


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- TODO rename: e.g. WidgetConstructorGetter
type WidgetConstructorGetter defaultType =
  UserInteractionOutputMonad
  (WidgetConstructorForObjectWithDefault defaultType)

-- | Widget constructor for an object.
type WidgetConstructorForObjectWithDefault defaultType = Maybe defaultType
                                                         -> ObjectName
                                                         -> AnyWIDGET


-------------------------------------------------------------------------------
-- - The monad -
-------------------------------------------------------------------------------


type UserInteractionOutputError = Presentation.Error

type UserInteractionOutputResult a = Either UserInteractionOutputError a

data UserInteractionOutputEnvironment =
  UserInteractionOutputEnvironment
  {
    envMedia             :: ES.ElementSet
  , envCustomEnvironment :: ES.ElementSet
  , envDbConfiguration   :: DbExecutor.Configuration
  , envOutputing         :: Presentation.Outputing
  }

-- | Gets the 'StandardServices.StandardServiceLinkRenderer' from a
-- 'Environment'.
envStandardServiceLinkRenderer :: UserInteractionOutputEnvironment
                               -> StandardServices.StandardServiceLinkRenderer
envStandardServiceLinkRenderer = Presentation.outStandardServiceLinkRenderer . envOutputing

-- | Gets the 'PopUpButtonTexter' from a
-- 'UserInteractionOutputEnvironment'.
envButtonTexter :: UserInteractionOutputEnvironment 
                -> PopUpButtonTexter
envButtonTexter = trButtonTexter . Presentation.outTranslations . envOutputing

-- | A function that provides the label for Pop Up Buttons.
type PopUpButtonTexter = PopUp.Button -> String

newtype UserInteractionOutputMonad a =
  UserInteractionOutputMonad (ErrorT UserInteractionOutputError (ReaderT UserInteractionOutputEnvironment IO) a)

instance MonadWithInputMedia UserInteractionOutputMonad where
  getInputMedia = fmap envMedia getEnv
  
instance MonadWithInputMediaAndLookup UserInteractionOutputMonad where
  inInputMedia = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getInputMedia
        , ES.throwError    = throwElementLookupError
        }

instance MonadWithCustomEnvironment UserInteractionOutputMonad where
  getCustomEnvironment = fmap envCustomEnvironment getEnv
  
instance MonadWithCustomEnvironmentAndLookup UserInteractionOutputMonad where
  inCustomEnvironment = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getCustomEnvironment
        , ES.throwError    = throwElementLookupError
        }

throwElementLookupError :: ES.ElementLookupError -> UserInteractionOutputMonad a
throwElementLookupError err = throwErr $ Presentation.MediaLookupError err

instance Monad UserInteractionOutputMonad where
  return = UserInteractionOutputMonad . return
  (UserInteractionOutputMonad m) >>= f = UserInteractionOutputMonad $
                                         do a <- m
                                            let UserInteractionOutputMonad m' = f a
                                            m'

instance Applicative UserInteractionOutputMonad where
  pure = UserInteractionOutputMonad . pure
  (UserInteractionOutputMonad ma) <*> (UserInteractionOutputMonad mb) =
    UserInteractionOutputMonad $ ma <*> mb

instance Functor UserInteractionOutputMonad where
  fmap f (UserInteractionOutputMonad m) = UserInteractionOutputMonad $ fmap f m

instance MonadIO UserInteractionOutputMonad where
  liftIO = UserInteractionOutputMonad . lift . lift

instance DbExecutor.MonadWithDatabaseConfiguration UserInteractionOutputMonad where
  getDatabaseConfiguration = getEnvs envDbConfiguration
  
-- | \"Computations\" (e.g. monads) that are instances of this class
-- can be integrated into the 'UserInteractionOutputMonad'.
class ToUserInteractionOutputMonad m where
  toUserInteractionOutputMonad :: m a -> UserInteractionOutputMonad a

-- | Runs a 'UserInteractionOutputMonad' computation.
run :: UserInteractionOutputEnvironment
    -> UserInteractionOutputMonad a
    -> IO (UserInteractionOutputResult a)
run env (UserInteractionOutputMonad errT) = runReaderT (runErrorT errT) env

getEnv :: UserInteractionOutputMonad UserInteractionOutputEnvironment
getEnv = UserInteractionOutputMonad $ lift ask

-- | Gets the environment of the 'UserInteractionOutputMonad'.
getEnvs :: (UserInteractionOutputEnvironment -> a) 
        -> UserInteractionOutputMonad a
getEnvs = UserInteractionOutputMonad . lift . asks

-- | Corresponds to 'Control.Monad.Trans.Error's throwError.
throwErr :: Presentation.ToPresentationError err
         => err 
         -> UserInteractionOutputMonad a
throwErr err = UserInteractionOutputMonad $ throwError (Presentation.toError err)

-- | Corresponds to 'Control.Monad.Trans.Error's catchError.
catchErr :: UserInteractionOutputMonad a                                    -- ^ The computation that can throw an error.
         -> (UserInteractionOutputError -> UserInteractionOutputMonad a) -- ^ Error handler
         -> UserInteractionOutputMonad a                                 
catchErr m handler =
  let
    (UserInteractionOutputMonad errT) = m
    handlerErrT err = let (UserInteractionOutputMonad errT) = handler err
                      in  errT
  in
   UserInteractionOutputMonad $ catchError errT handlerErrT

instance ToUserInteractionOutputMonad Presentation.Monad where
  toUserInteractionOutputMonad m =
    do
      env        <- getEnv
      let presEnv = toPresEnv env
      res        <- liftIO $ Presentation.run presEnv m
      toUserInteractionOutputMonad res
    where
      toPresEnv :: UserInteractionOutputEnvironment -> Presentation.Environment
      toPresEnv (UserInteractionOutputEnvironment
                 { envCustomEnvironment = theEnvCustomEnvironment
                 , envDbConfiguration   = theEnvDbConfiguration
                 , envOutputing         = theEnvOutputing }) =
        Presentation.Environment
        {
          Presentation.envCustomEnvironment = theEnvCustomEnvironment
        , Presentation.envDbConfiguration   = theEnvDbConfiguration
        , Presentation.envOutputing         = theEnvOutputing
        }

instance Presentation.ToPresentationError err => ToUserInteractionOutputMonad (Either err) where
  toUserInteractionOutputMonad (Left err) = throwErr err
  toUserInteractionOutputMonad (Right ok) = return ok

instance Presentation.ToPresentationError err =>
         ToUserInteractionOutputMonad (ErrorT err IO) where
  toUserInteractionOutputMonad m =
    do
      res <- liftIO $ runErrorT m
      toUserInteractionOutputMonad res

instance ToUserInteractionOutputMonad DBIO.DatabaseMonad where
  toUserInteractionOutputMonad m =
    do
      custEnv <- getCustomEnvironment
      res     <- liftIO $ DBIO.runDatabase custEnv m
      toUserInteractionOutputMonad res
  
-- | Integrates monads of type "IO (Either err a)"
-- into the UserInteractionOutputMonad monad
liftIOWithError :: Presentation.ToPresentationError err
                => IO (Either err a)
                -> UserInteractionOutputMonad a
liftIOWithError io =
  do
    res <- liftIO io
    case res of
      Left err -> throwErr err
      Right ok -> return ok

toUserInteractionOutputMonadWithConn :: (ConnWrapper -> DBIO.DatabaseMonad a)
                                     -> UserInteractionOutputMonad a
toUserInteractionOutputMonadWithConn f =
  do
    conn <- DbExecutor.getDatabaseConnection
    toUserInteractionOutputMonad $ f conn

toUserInteractionOutputMonadWithCar :: (DbExecutor.ConnectionAndRenderer
                                        -> DBIO.DatabaseMonad a)
                                     -> UserInteractionOutputMonad a
toUserInteractionOutputMonadWithCar f =
  do
    car <- DbExecutor.getDatabaseConnectionAndRenderer
    toUserInteractionOutputMonad $ f car

-- | Integrates monads of type "ErrorT err IO"
-- into the UserInteractionOutputMonad monad
-- liftIOWithErrorT :: ToUserInteractionOutputError err
--                  => ErrorT err IO a
--                  -> UserInteractionOutputMonad a
-- liftIOWithErrorT io =
--   do
--     res <- liftIO $ runErrorT io
--     either throwErr return res
