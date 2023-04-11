-- | Monad for generating User Interaction Output - information for producing a
-- User Interface that lets the user input information to the program.
--
-- | Import qualified.

{-# LANGUAGE FlexibleInstances #-}

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

         Monad,
         UserInteractionOutputResult,
         UserInteractionOutputError(..),
         Presentation.Error(..),

         -- * Monad Execution

         run,

         -- * Monad Environment

         UserInteractionOutputEnvironment(envMedia, envCustomEnvironment, envOutputing),
         newEnvironment,
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

         toUiOMonad_wDefaultDbConn,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Prelude hiding (Monad(..))

import qualified Control.Monad as M
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import qualified Wilde.Utils.ExceptReaderT as ExceptReaderT
import qualified Wilde.Utils.Logging.Class as Logger
import qualified Wilde.Utils.Logging.Monad as Logging

import qualified Wilde.Media.ElementSet as ES
import           Wilde.Media.CustomEnvironment
import           Wilde.Media.UserInteraction
import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Media.Database.Monad as DbConn
import qualified Wilde.Media.Presentation as Presentation

import           Wilde.Media.MonadWithInputMedia

import qualified Wilde.Application.Service.PopUp as PopUp
import           Wilde.Media.Translations
import qualified Wilde.Application.StandardServices as StandardServices


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type WidgetConstructorGetter defaultType =
  Monad
  (WidgetConstructorForObjectWithDefault defaultType)

-- | Widget constructor for an object.
type WidgetConstructorForObjectWithDefault defaultType =
  Maybe defaultType -> ObjectName -> AnyWIDGET


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
  , envDbConfiguration   :: DbConf.Configuration
  , envOutputing         :: Presentation.Outputing
  , envLogger            :: Logger.AnyLogger
  }

newEnvironment :: ES.ElementSet  -- ^ media
               -> ES.ElementSet  -- ^ custom environment
               -> DbConf.Configuration
               -> Presentation.Outputing
               -> Logger.AnyLogger
               -> UserInteractionOutputEnvironment
newEnvironment = UserInteractionOutputEnvironment

withEnv :: (UserInteractionOutputEnvironment -> UserInteractionOutputEnvironment) -> Monad a -> Monad a
withEnv modifyEnv (Monad m) = Monad $ ExceptReaderT.withEnv modifyEnv m

setLogger :: Logger.AnyLogger -> UserInteractionOutputEnvironment -> UserInteractionOutputEnvironment
setLogger l env = env { envLogger = l }

-- | Gets the 'StandardServices.StandardServiceLinkRenderer' from a
-- 'Environment'.
envStandardServiceLinkRenderer :: UserInteractionOutputEnvironment
                               -> StandardServices.StandardServiceLinkRenderer
envStandardServiceLinkRenderer = Presentation.standardServiceLinkRenderer . Presentation.outServiceLinks . envOutputing

-- | Gets the 'PopUpButtonTexter' from a
-- 'UserInteractionOutputEnvironment'.
envButtonTexter :: UserInteractionOutputEnvironment
                -> PopUpButtonTexter
envButtonTexter = trButtonTexter . Presentation.outTranslations . envOutputing

-- | A function that provides the label for Pop Up Buttons.
type PopUpButtonTexter = PopUp.Button -> String

newtype Monad a =
  Monad (ExceptT UserInteractionOutputError (ReaderT UserInteractionOutputEnvironment IO) a)

instance MonadWithInputMedia Monad where
  getInputMedia = fmap envMedia getEnv

instance MonadWithInputMediaAndLookup Monad where
  inInputMedia = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getInputMedia
        , ES.throwError    = throwElementLookupError
        }

instance MonadWithCustomEnvironment Monad where
  getCustomEnvironment = fmap envCustomEnvironment getEnv

instance MonadWithCustomEnvironmentAndLookup Monad where
  inCustomEnvironment = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getCustomEnvironment
        , ES.throwError    = throwElementLookupError
        }

instance Logging.MonadWithLogging Monad where
  getLogger = getEnvs envLogger
  withLogger logger = withEnv (setLogger logger)

throwElementLookupError :: ES.ElementLookupError -> Monad a
throwElementLookupError err = throwErr $ Presentation.MediaLookupError err

instance M.Monad Monad where
  (Monad m) >>= f = Monad $
                    do a <- m
                       let Monad m' = f a
                       m'

instance Applicative Monad where
  pure = Monad . pure
  (Monad ma) <*> (Monad mb) =
    Monad $ ma <*> mb

instance Functor Monad where
  fmap f (Monad m) = Monad $ fmap f m

instance MonadIO Monad where
  liftIO = Monad . lift . lift

-- | \"Computations\" (e.g. monads) that are instances of this class
-- can be integrated into the 'Monad'.
class ToUserInteractionOutputMonad m where
  toUserInteractionOutputMonad :: m a -> Monad a

-- | Runs a 'Monad' computation.
run :: UserInteractionOutputEnvironment
    -> Monad a
    -> IO (UserInteractionOutputResult a)
run env (Monad errT) = runReaderT (runExceptT errT) env

getEnv :: Monad UserInteractionOutputEnvironment
getEnv = Monad $ lift ask

-- | Gets the environment of the 'Monad'.
getEnvs :: (UserInteractionOutputEnvironment -> a)
        -> Monad a
getEnvs = Monad . lift . asks

-- | Corresponds to 'Control.Monad.Trans.Error's throwError.
throwErr :: Presentation.ToPresentationError err
         => err
         -> Monad a
throwErr err = Monad $ throwE (Presentation.toError err)

-- | Corresponds to 'Control.Monad.Trans.Error's catchError.
catchErr :: Monad a                                 -- ^ The computation that can throw an error.
         -> (UserInteractionOutputError -> Monad a) -- ^ Error handler
         -> Monad a
catchErr m handler =
  let
    (Monad errT) = m
    handlerErrT err = let (Monad errT) = handler err
                      in  errT
  in
   Monad $ catchE errT handlerErrT

instance ToUserInteractionOutputMonad Presentation.Monad where
  toUserInteractionOutputMonad m =
    do
      env        <- getEnv
      let presEnv = toPresEnv env
      res        <- liftIO $ Presentation.run presEnv m
      toUserInteractionOutputMonad res
    where
      toPresEnv :: UserInteractionOutputEnvironment -> Presentation.Environment
      toPresEnv UserInteractionOutputEnvironment
                { envCustomEnvironment = theEnvCustomEnvironment
                , envDbConfiguration   = theEnvDbConfiguration
                , envOutputing         = theEnvOutputing
                , envLogger            = theLogger
                } =
          Presentation.newEnvironment
          theEnvCustomEnvironment theEnvDbConfiguration theEnvOutputing
          theLogger

instance Presentation.ToPresentationError err => ToUserInteractionOutputMonad (Either err) where
  toUserInteractionOutputMonad (Left err) = throwErr err
  toUserInteractionOutputMonad (Right ok) = pure ok

instance Presentation.ToPresentationError err =>
         ToUserInteractionOutputMonad (ExceptT err IO) where
  toUserInteractionOutputMonad m =
    do
      res <- liftIO $ runExceptT m
      toUserInteractionOutputMonad res

-- | Integrates monads of type "IO (Either err a)"
-- into the Monad monad
liftIOWithError :: Presentation.ToPresentationError err
                => IO (Either err a)
                -> Monad a
liftIOWithError io =
  do
    res <- liftIO io
    case res of
      Left err -> throwErr err
      Right ok -> pure ok

toUiOMonad_wDefaultDbConn ::
  DbConn.Monad a ->
  Monad a
toUiOMonad_wDefaultDbConn dbm =
  do
    dbConnMonadEnv <- getDbConnMonadEnv
    liftIOWithError $ DbConn.run dbConnMonadEnv dbm

  where
    getDbConnMonadEnv :: Monad DbConn.Environment
    getDbConnMonadEnv = do
      env            <- getEnv
      let dbConf      = envDbConfiguration env
      let dmlRenderer = DbConf.dmlRenderer dbConf
      conn           <- liftIO $ DbConf.connectionProvider dbConf
      let custEnv      = envCustomEnvironment env
      let logger       = envLogger env
      pure $ DbConn.newEnv custEnv dmlRenderer conn logger
