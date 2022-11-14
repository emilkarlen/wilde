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

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------
-- | Definition of \"service\".
--
-- A Wilde application constist of a set of services, and these services
-- define all behaviour of the application.
--
-- The main function of an application looks up which service to execute
-- and then executes it.
--
-- Services execute in the 'ServiceMonad'.  From this monad it is possible
-- to execute code in the monad for database communication and in the
-- monads for User Interaction input and output.
--
--
-- TODO This module is too big.
-------------------------------------------------------------------------------
module Wilde.Service.Monad
       (
         module MIIA,

         -- * The service monad

         ServiceMonad,
         runService,

         withFinally,

         ToServiceMonad(..),
         toServiceMonad_wDefaultDbConn,

         -- ** Environment

         ServiceEnvironment(envCurrentService, envCustomEnvironment, envMedia, envOutputing),
         newEnvironment,
         getEnv,
         getEnvs,

         -- ** Errors

         ServiceError(..),
         InvocationError(..),
         ToServiceError(..),
         throwErr,
         catchErr,

         -- * Logging
         Logging.MonadWithLogging(..),

       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
    ( ReaderT(runReaderT), ask, asks )


import qualified Data.Map as Map

import Database.HDBC as HDBC

import qualified Wilde.Utils.ExceptReaderT as ExceptReaderT
import qualified Wilde.Utils.Logging.Class as Logger
import qualified Wilde.Utils.Logging.Monad as Logging
import qualified Wilde.Media.MonadWithInputMedia as MIIA
import qualified Wilde.Media.ElementSet as ES
import           Wilde.Media.CustomEnvironment
import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Media.UserInteraction.Output as UiOM
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.Media.Database.Monad as DbConn
import qualified Wilde.Media.Presentation as Presentation

import Wilde.Service.ServiceLink

import qualified Wilde.Service.SingleDbConnectionHandler as SingleDbConnectionHandler

import Wilde.Service.Error


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Corresponds to 'Control.Monad.Trans.Error's throwError.
throwErr :: ToServiceError err
         => err -> ServiceMonad a
throwErr = ServiceMonad . throwE . toServiceError

-- | Corresponds to 'Control.Monad.Trans.Error's catchError.
catchErr :: ServiceMonad a                   -- ^ The computation that can throw an error.
         -> (ServiceError -> ServiceMonad a) -- ^ Error handler
         -> ServiceMonad a
catchErr m handler =
  let
    (ServiceMonad errT) = m
    handlerErrT err = let (ServiceMonad errT) = handler err
                      in  errT
  in
   ServiceMonad $ catchE errT handlerErrT


-------------------------------------------------------------------------------
-- - Environment -
-------------------------------------------------------------------------------

-- | Media for inputing an attribute FROM the User Interaction.
--
-- A map : NameOfVariable -> Values
type InputMedia  = Map.Map String [String]

-- | The environment accessible by a service.
-- (This is provided by the main program.)
data ServiceEnvironment =
  ServiceEnvironment
  {
    envCurrentService    :: ServiceId
  , envCustomEnvironment :: ES.ElementSet
  , envMedia             :: ES.ElementSet
  , envDbConfiguration   :: DbConf.Configuration
  , envOutputing         :: UiOM.Outputing
  , envLogger            :: Logger.AnyLogger
  }

newEnvironment :: ServiceId
               -> ES.ElementSet -- ^ custom environment
               -> ES.ElementSet -- ^ media
               -> DbConf.Configuration -> UiOM.Outputing
               -> Logger.AnyLogger
               -> ServiceEnvironment
newEnvironment = ServiceEnvironment

setLogger :: Logger.AnyLogger -> ServiceEnvironment -> ServiceEnvironment
setLogger logger env = env { envLogger = logger }

-------------------------------------------------------------------------------
-- | Guarranties that a \"cleanup\" computation is executed after
-- another \"main\" computation.
--
-- Mimics Java's \"finally\".
-------------------------------------------------------------------------------
withFinally :: ServiceMonad b -> ServiceMonad a -> ServiceMonad a
withFinally cleanup action =
  let normal =
        do
          res <- action
          cleanup
          return res
      exceptionHandling error = cleanup >> throwErr error
  in  catchErr normal exceptionHandling


-------------------------------------------------------------------------------
-- - ServiceMonad -
-------------------------------------------------------------------------------


newtype ServiceMonad a = ServiceMonad (ExceptT ServiceError (ReaderT ServiceEnvironment IO) a)

-- | Executes a computation in the Service Monad.
runService :: ServiceEnvironment
           -- ^ The conn-provider of the db config must create a new connection
           -- on every invokation.
           -> ServiceMonad a
           -> IO (Either ServiceError a)
runService envWMkNewDbConnOnEveryInvokation (ServiceMonad m) =
  do
    let srvcHdrStr = toLogStr $ envCurrentService envWMkNewDbConnOnEveryInvokation
    let logger = envLogger envWMkNewDbConnOnEveryInvokation
    Logger.register logger (Logger.LIBRARY, srvcHdrStr ++ " BEGIN", Nothing)
    (envWDbConnHandling, doDbConnCleanup) <- dbEnvWithSingleDbConnHandling
    res <- runReaderT (runExceptT m) envWDbConnHandling
    reporting <- doDbConnCleanup
    let dbConnHandlingMsg = "Db connection handling: " ++ reporting
    Logger.register logger (Logger.LIBRARY, dbConnHandlingMsg, Nothing)
    Logger.register logger (Logger.LIBRARY, srvcHdrStr ++ " END", Nothing)
    pure res
  
  where
    dbEnvWithSingleDbConnHandling :: IO (ServiceEnvironment, IO String)
    dbEnvWithSingleDbConnHandling = do
      let confWMkNewDbConnOnEveryInvokation = envDbConfiguration envWMkNewDbConnOnEveryInvokation
      let getNewConn     = DbConf.connectionProvider confWMkNewDbConnOnEveryInvokation
      singleConnHandler <- SingleDbConnectionHandler.newHandler getNewConn
      let getSingleConn  = SingleDbConnectionHandler.getConnection singleConnHandler
      pure (envWDbConnProvider getSingleConn,
            SingleDbConnectionHandler.disconnectIfNeeded singleConnHandler)

    envWDbConnProvider :: IO ConnWrapper -> ServiceEnvironment
    envWDbConnProvider connProvider =
      let
        origDbConf = envDbConfiguration envWMkNewDbConnOnEveryInvokation
        newDbConf  = origDbConf { DbConf.connectionProvider =  connProvider }
      in
        envWMkNewDbConnOnEveryInvokation { envDbConfiguration = newDbConf }

instance Monad ServiceMonad where
  return = ServiceMonad . return
  (ServiceMonad m) >>= f = ServiceMonad $
                           do a <- m
                              let ServiceMonad m' = f a
                              m'

instance Applicative ServiceMonad where
  pure = ServiceMonad . pure
  (ServiceMonad ma) <*> (ServiceMonad mb) = ServiceMonad $ ma <*> mb

instance Functor ServiceMonad where
  fmap f (ServiceMonad m) = ServiceMonad $ fmap f m


-------------------------------------------------------------------------------
-- - access to the monad environment -
-------------------------------------------------------------------------------


-- | Gets the environment of the 'ServiceMonad'.
getEnv :: ServiceMonad ServiceEnvironment
getEnv = ServiceMonad $ lift ask

-- | Gets the environment of the 'ServiceMonad'.
getEnvs :: (ServiceEnvironment -> a) -> ServiceMonad a
getEnvs = ServiceMonad . lift . asks

withEnv :: (ServiceEnvironment -> ServiceEnvironment) -> ServiceMonad a -> ServiceMonad a
withEnv modifyEnv (ServiceMonad m) = ServiceMonad $ ExceptReaderT.withEnv modifyEnv m

-------------------------------------------------------------------------------
-- - instances of "monad interfaces" -
-------------------------------------------------------------------------------


instance MonadIO ServiceMonad where
  liftIO = ServiceMonad . lift . lift

instance MIIA.MonadWithInputMedia ServiceMonad where
  getInputMedia = getEnvs envMedia

instance MIIA.MonadWithInputMediaAndLookup ServiceMonad where
  inInputMedia = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = MIIA.getInputMedia
        , ES.throwError    = throwElementLookupError
        }

instance MonadWithCustomEnvironment ServiceMonad where
  getCustomEnvironment = getEnvs envCustomEnvironment

instance MonadWithCustomEnvironmentAndLookup ServiceMonad where
  inCustomEnvironment = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getCustomEnvironment
        , ES.throwError    = throwElementLookupError
        }

instance Logging.MonadWithLogging ServiceMonad where
  getLogger = getEnvs envLogger
  withLogger logger = withEnv (setLogger logger)


throwElementLookupError :: ES.ElementLookupError -> ServiceMonad a
throwElementLookupError err = throwErr $ UiMediaLookupError err


-------------------------------------------------------------------------------
-- - instances of ToServiceMonad -
-------------------------------------------------------------------------------

-- | Class for translating values to 'ServiceMonad' values.
--
-- Especially, changing the "container" of a value to the 'ServiceMonad' "container".
-- Integrates other monads into the 'ServiceMonad'.
class ToServiceMonad m where
  toServiceMonad :: m a -> ServiceMonad a

-- For, among others, 'TranslationResult'.
instance ToServiceError err => ToServiceMonad (Either err) where
  toServiceMonad (Right x) = return x
  toServiceMonad (Left er) = throwErr er

instance ToServiceMonad Presentation.Monad where
  toServiceMonad m =
    do
      env        <- getEnv
      let presEnv = Presentation.newEnvironment
                    (envCustomEnvironment env) (envDbConfiguration env)
                    (envOutputing env) (envLogger env)
      result     <- liftIO $ Presentation.run presEnv m
      toServiceMonad result

instance ToServiceMonad UiOM.UserInteractionOutputMonad where
  toServiceMonad uiom =
    do
      env        <- getEnv
      let uiomEnv = UiOM.newEnvironment
                    (envMedia env) (envCustomEnvironment env)
                    (envDbConfiguration env) (envOutputing env) (envLogger env)
      result     <- liftIO $ UiOM.run uiomEnv uiom
      toServiceMonad result

instance ToServiceMonad UiI.Monad where
  toServiceMonad uiom =
    do
      inputMap <- UiI.getInputMedia
      custEnv  <- getCustomEnvironment
      result   <- liftIO $ UiI.run (UiI.Environment inputMap custEnv) uiom
      toServiceMonad result

instance ToServiceMonad IO where
  toServiceMonad = ServiceMonad . lift . lift


-------------------------------------------------------------------------------
-- - Executing DB monads -
-------------------------------------------------------------------------------


toServiceMonad_wDefaultDbConn :: DbConn.Monad a -> ServiceMonad a
toServiceMonad_wDefaultDbConn m =
  do
    env                 <- getEnv
    let logger           = envLogger env
    let dbConf           = envDbConfiguration env
    conn                <- liftIO $ DbConf.connectionProvider dbConf
    let dmlRenderer      = DbConf.dmlRenderer dbConf
    let dbConnMonadEnv   = DbConn.newEnv (envCustomEnvironment env) dmlRenderer conn logger
    res                 <- liftIO $ DbConn.run dbConnMonadEnv m
    toServiceMonad res


toLogStr :: ServiceId -> String
toLogStr (ServiceId srvc mbOt) = "Service " ++ srvc ++ maybe "" (\ot -> "/" ++ ot) mbOt
