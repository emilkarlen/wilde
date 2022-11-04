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
module Wilde.Application.Service
       (
         module MIIA,

         -- * Service definition

         ServicePage,
         Service,
         ServiceName,
         ServiceId(..),

         -- * Service "OK"/"normal" results

         ServiceOkResult,
         pageOkResult,
         popupOkResult,
         processOkResult,
         processPopUpOkResult,

         -- ** PopUps

         ServicePopUp,

         AskIfContinuePopUp(..),
         askIfContinuePopup,

         InformationPopUp(..),
         informationPopup,

         -- * Service "pages"

         servicePageTitle,
         servicePageStyle,
         servicePageContents,

         -- * The service monad

         ServiceMonad,
         runService,
         
         withFinally,
         withDbConnection,
         withDbTransaction,

         withDbConnectionCar,
         withDbTransactionCar,

         ToServiceMonad(..),
         toServiceMonadWithConn,
         toServiceMonadWithCar,

         -- ** Environment

         ServiceEnvironment(..),
         getEnv,
         getEnvs,

         -- ** Errors

         ServiceError(..),
         InvocationError(..),
         ToServiceError(..),
         throwErr,
         catchErr,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader


import qualified Data.Map as Map

import Database.HDBC

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.Database.Executor as SqlExec

import qualified Wilde.Media.MonadWithInputMedia as MIIA
import qualified Wilde.Media.ElementSet as ES
import           Wilde.Media.CustomEnvironment
import           Wilde.Media.WildeMedia as WM
import qualified Wilde.Media.Database as DbM
import qualified Wilde.Media.UserInteraction.Output as UiOM
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.Media.Database.Monad as DBIO
import qualified Wilde.Media.Presentation as Presentation
import qualified Wilde.Application.PopUp as PopUp

import Wilde.Application.ServiceLink


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data ServiceError = SInvocationError    InvocationError
                  | DbIoError           DbM.DatabaseError
                  | UiMediaLookupError  ES.ElementLookupError
                  | UiObjectInputError  (NonEmpty.List ObjectInputErrorInfo)
                  | UnclassifiedError   WM.UnclassifiedError
                  | NormalError         String
                  | SObjectModelError   String
                  | ImplementationError String
                    deriving Show

data InvocationError = MandatoryVariableMissing String (Maybe [String]) -- ^ Var Name,actual value
                     | ValueSyntax String String                        -- ^ Var Name,actual value
                     | ValueValue  String String                        -- ^ Var Name,actual value
                       deriving Show

-- | Class for the error "sub types" of ServiceError.
class ToServiceError a where
  toServiceError :: a -> ServiceError

instance ToServiceError GeneralError where
  toServiceError (GeneralUnclassifiedError s) = UnclassifiedError (unclassifiedError s)
  toServiceError (GeneralObjectModelError  s) = SObjectModelError s

instance ToServiceError ObjectToNativeError where
  toServiceError = SObjectModelError . show

instance ToServiceError UnclassifiedError where
  toServiceError = UnclassifiedError

instance ToServiceError InvocationError where
  toServiceError = SInvocationError

instance ToServiceError DbM.TranslationError where
  toServiceError = DbIoError . DbM.DbTranslationError

instance ToServiceError DbM.DatabaseError where
  toServiceError = DbIoError

instance ToServiceError UiOM.UserInteractionOutputError where
  toServiceError (UiOM.DatabaseError error)        = toServiceError error
  toServiceError (UiOM.ObjectModelError string)    = SObjectModelError string
  toServiceError (UiOM.ImplementationError string) = ImplementationError string
  toServiceError (UiOM.MediaLookupError info)      = UiMediaLookupError info
  toServiceError (UiOM.UnclassifiedError error)    = toServiceError error

instance ToServiceError ServiceError where
  toServiceError = id

instance ToServiceError UiI.Error where
  toServiceError (UiI.ImplementationError string)   = ImplementationError string
  toServiceError (UiI.UnclassifiedError   err)      = UnclassifiedError err
  toServiceError (UiI.MediaLookupError info)        = UiMediaLookupError info

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


-- | Identifies a service.
data ServiceId = ServiceId
                 {
                   sidName       :: String,
                   sidObjectType :: Maybe String
                 }

-- | Media for inputing an attribute FROM the User Interaction.
--
-- A map : NameOfVariable -> Values
type InputMedia  = Map.Map String [String]

-- | The environment accessible by a service.
-- (This is provided by the main program.)
data ServiceEnvironment =
  ServiceEnvironment
  {
    envCurrentService  :: ServiceId
  , envCustomEnvironment :: ES.ElementSet
  , envMedia           :: ES.ElementSet
  , envDbConfiguration :: SqlExec.Configuration
  , envOutputing       :: UiOM.Outputing
  }

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
-- | Executes a computation that uses a DB-connection, and guarranties
-- that the connection is disconnected afterwards (even in the case of
-- errors).
-------------------------------------------------------------------------------
withDbConnection :: (ConnWrapper -> ServiceMonad a) -> ServiceMonad a
withDbConnection action =
  do
    conn <- getConn
    withFinally (toServiceMonad $ disconnect conn) (action conn)

-------------------------------------------------------------------------------
-- | A variant of 'withDbConnection' where the
-- \"connection\" is a 'SqlExec.ConnectionAndRenderer'.
-------------------------------------------------------------------------------
withDbConnectionCar :: (SqlExec.ConnectionAndRenderer -> ServiceMonad a) -> ServiceMonad a
withDbConnectionCar action =
  do
    car <- SqlExec.getDatabaseConnectionAndRenderer
    let conn = SqlExec.carConnection car
    withFinally (toServiceMonad $ disconnect conn) (action car)

-------------------------------------------------------------------------------
-- | Executes a computation that uses a DB-connection, and guarranties
-- that the connection is disconnected afterwards (even in the case of
-- errors).  The transaction is commited iff there is no error.
-------------------------------------------------------------------------------
withDbTransaction :: (ConnWrapper -> ServiceMonad a) -> ServiceMonad a
withDbTransaction action =
  do
    conn <- getConn
    withFinally (toServiceMonad $ disconnect conn) (actionWithCommit conn)
  where
    actionWithCommit conn =
      do
        result <- action conn
        toServiceMonad $ commit conn
        return result

-------------------------------------------------------------------------------
-- | A variant of 'withDbTransaction' where the
-- \"connection\" is a 'SqlExec.ConnectionAndRenderer'.
-------------------------------------------------------------------------------
withDbTransactionCar :: (SqlExec.ConnectionAndRenderer -> ServiceMonad a) -> ServiceMonad a
withDbTransactionCar action =
  do
    car <- SqlExec.getDatabaseConnectionAndRenderer
    let conn = SqlExec.carConnection car
    withFinally (toServiceMonad $ disconnect conn) (actionWithCommit car)
  where
    actionWithCommit car =
      do
        result <- action car
        toServiceMonad $ commit (SqlExec.carConnection car)
        return result

-- | Gets a database connection.
getConn :: ServiceMonad ConnWrapper
getConn =
  do
    getConnection <- getEnvs (SqlExec.connectionProvider . envDbConfiguration)
    liftIO getConnection
    -- toServiceMonad getConnection


-------------------------------------------------------------------------------
-- - ServiceMonad -
-------------------------------------------------------------------------------


newtype ServiceMonad a = ServiceMonad (ExceptT ServiceError (ReaderT ServiceEnvironment IO) a)

-- | Executes a computation in the Service Monad.
runService :: ServiceEnvironment
           -> ServiceMonad a
           -> IO (Either ServiceError a)
runService env (ServiceMonad m) = runReaderT (runExceptT m) env

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

instance MonadIO ServiceMonad where
  liftIO = ServiceMonad . lift . lift

instance SqlExec.MonadWithDatabaseConfiguration ServiceMonad where
  getDatabaseConfiguration = getEnvs envDbConfiguration

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
throwElementLookupError :: ES.ElementLookupError -> ServiceMonad a
throwElementLookupError err = throwErr $ UiMediaLookupError err

-- reader :: MonadReader ServiceEnvironment m => (r -> a) -> m a
-- vill kunna lyfta alla MonadReader ServiceEnvironment ...
-- med toServiceMonad.

-- | Gets the environment of the 'ServiceMonad'.
getEnv :: ServiceMonad ServiceEnvironment
getEnv = ServiceMonad $ lift ask

-- | Gets the environment of the 'ServiceMonad'.
getEnvs :: (ServiceEnvironment -> a) -> ServiceMonad a
getEnvs = ServiceMonad . lift . asks

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
      let presEnv = Presentation.Environment
                    {
                      Presentation.envCustomEnvironment = envCustomEnvironment env
                    , Presentation.envDbConfiguration   = envDbConfiguration env
                    , Presentation.envOutputing         = envOutputing env
                    }
      result     <- liftIO $ Presentation.run presEnv m
      toServiceMonad result

instance ToServiceMonad UiOM.UserInteractionOutputMonad where
  toServiceMonad uiom =
    do
      env        <- getEnv
      let uiomEnv = UiOM.UserInteractionOutputEnvironment
                    {
                      UiOM.envMedia             = envMedia env
                    , UiOM.envCustomEnvironment = envCustomEnvironment env
                    , UiOM.envDbConfiguration   = envDbConfiguration env
                    , UiOM.envOutputing         = envOutputing env
                    }
      result     <- liftIO $ UiOM.run uiomEnv uiom
      toServiceMonad result

instance ToServiceMonad UiI.Monad where
  toServiceMonad uiom =
    do
      inputMap <- UiI.getInputMedia
      custEnv  <- getCustomEnvironment
      result   <- liftIO $ UiI.run (UiI.Environment inputMap custEnv) uiom
      toServiceMonad result

instance ToServiceMonad DBIO.DatabaseMonad where
  toServiceMonad m =
    do
      custEnv <- getCustomEnvironment
      res     <- liftIO $ DBIO.runDatabase custEnv m
      toServiceMonad res

instance ToServiceMonad IO where
  toServiceMonad = ServiceMonad . lift . lift

toServiceMonadWithConn :: (forall conn . IConnection conn => conn -> DBIO.DatabaseMonad a)
                          -> ServiceMonad a
toServiceMonadWithConn f =
  do
    conn <- getConn
    toServiceMonad $ f conn

toServiceMonadWithCar :: (SqlExec.ConnectionAndRenderer -> DBIO.DatabaseMonad a)
                         -> ServiceMonad a
toServiceMonadWithCar f =
  do
    car <- SqlExec.getDatabaseConnectionAndRenderer
    toServiceMonad $ f car


-------------------------------------------------------------------------------
-- - Service -
-------------------------------------------------------------------------------


type Service = ServiceMonad ServiceOkResult

-- | All types of results of a service.
data ServiceOkResult
  = OkResultPage  ServicePage  -- ^ A simple page with custom contents.
  | OkResultPopUp ServicePopUp -- ^ A small dialog that will "pop up".

-- | Processed all forms of 'ServiceOkResult'.
processOkResult :: (ServicePage -> a)
                -> (ServicePopUp -> a)
                -> ServiceOkResult
                -> a
processOkResult processPage _ (OkResultPage x) = processPage x
processOkResult _ processPopUp (OkResultPopUp x) = processPopUp x

-- | Makes the given page the result of the service.
pageOkResult :: ServicePage -> ServiceMonad ServiceOkResult
pageOkResult page = return $ OkResultPage page

-- | Makes the given popup the result of the service.
popupOkResult :: ServicePopUp -> ServiceMonad ServiceOkResult
popupOkResult popup = return $ OkResultPopUp popup

-- | All types of small "pop up" dialogs, possibly with information about how
-- to continue after the user has responded to the choices that the pop up
-- lets the user choose from.
data ServicePopUp
   = AskIfContinue AskIfContinuePopUp
   | Information   InformationPopUp

-- | Processes all kinds of Service "pop up" results.
processPopUpOkResult :: (AskIfContinuePopUp -> a)
                     -> (InformationPopUp -> a)
                     -> ServicePopUp
                     -> a
processPopUpOkResult processAic _ (AskIfContinue x) = processAic x
processPopUpOkResult _ processInf (Information   x) = processInf x

-- | A "pop up" that asks the user wether to continue the service
-- that he/she has started or not.
data AskIfContinuePopUp =
    AskIfContinuePopUp
    {
      askIfContinueMessage      :: PopUp.Message
    , askIfContinueContinuation :: ServiceLink
    }

-- | A "pop up" that gives the user some information.
-- The service might continue or not after the message has been
-- displayed.
data InformationPopUp =
    InformationPopUp
    {
      informationMessage      :: PopUp.Message
    , informationContinuation :: Maybe ServiceLink
    }

askIfContinuePopup :: PopUp.Message -> ServiceLink -> ServicePopUp
askIfContinuePopup msg continuation =
  AskIfContinue (AskIfContinuePopUp msg continuation)

informationPopup :: PopUp.Message -> Maybe ServiceLink -> ServicePopUp
informationPopup msg continuation =
  Information (InformationPopUp msg continuation)

-- | A "page" that is the "result" of the execution of a service.
-- (title,style,contents)
type ServicePage = (StyledTitle
                   ,[AnyCOMPONENT]
                   )

servicePageTitle    :: ServicePage -> Title
servicePageTitle (styledTitle,_) = wildeStyled styledTitle

servicePageStyle    :: ServicePage -> WildeStyle
servicePageStyle (styledTitle,_) = wildeStyle styledTitle

servicePageContents :: ServicePage -> [AnyCOMPONENT]
servicePageContents (_,c) = c
