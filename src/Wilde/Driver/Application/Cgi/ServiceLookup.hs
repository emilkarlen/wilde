-- Utilities for Appliation Drivers that use CGI.
module Wilde.Driver.Application.Cgi.ServiceLookup
       (
         lookupService,
         newServiceEnvironment,
         getServiceAndEnvironment,
         ServiceSpecificationError(..),
         AppServices.LookupError,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.ElementSet as ElementSet

import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames

import qualified Wilde.Media.UserInteraction.Output as UiOm (Outputing(..))
import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo
import           Wilde.Service.ServiceLink
import qualified Wilde.Application.ApplicationInput as AppInput
import           Wilde.Service.Monad
import qualified Wilde.Service.Monad as Service
import qualified Wilde.Application.ApplicationServices as AppServices
import qualified Wilde.Application.ApplicationConfiguration as AppConf
import Wilde.Application.Service.Service


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Errror in looking up the service to execute for the application.
data ServiceSpecificationError
   = ServiceNotSpecified
   | InvalidServiceSpecificationValue { isValueForObjectType :: Bool }
   | InvalidServiceSpecification AppServices.LookupError
   deriving Show

lookupService :: AppServices.ApplicationServices
              -> AppInput.Input
              -> Either ServiceSpecificationError (ServiceId,Service,AppInput.Input)
lookupService applicationServices input@(AppInput.Input { AppInput.inputMedia = theInputMedia }) =
  do
    mbService <- lookupServiceSpecValue theInputMedia False
    mbOt      <- lookupServiceSpecValue theInputMedia True
    case mbService of
      Nothing    -> Left ServiceNotSpecified
      Just sSrvc ->
        case AppServices.lookupService applicationServices sSrvc mbOt of
          Right srvc -> pure (ServiceId sSrvc mbOt,srvc,inputWithoutServiceSpec)
            where inputWithoutServiceSpec =
                    input {
                      AppInput.inputMedia = maybe id ElementSet.deleteRaw_stringKey mbOt $
                                            ElementSet.deleteRaw_stringKey sSrvc theInputMedia
                          }
          Left   err -> Left $ InvalidServiceSpecification err

-- | Looks up one of the variables that specify the service.
lookupServiceSpecValue :: ElementSet.ElementSet
                          -> Bool
                          -- ^ If to lookup an ObjectType-service.
                          -- Otherwise we lookup a global service.
                          -> Either ServiceSpecificationError (Maybe String)
lookupServiceSpecValue es isObjectType =
  case ElementSet.lookupRaw_stringKey varName es of
    Nothing  -> pure Nothing
    Just [v] -> pure (Just v)
    Just   _ -> Left (InvalidServiceSpecificationValue isObjectType)
  where
    varName = if isObjectType
              then VariableNames.objectType
              else VariableNames.service

getServiceAndEnvironment :: AppConf.ApplicationConfiguration
                         -> AppInput.Input
                         -> Either
                            ServiceSpecificationError
                            (Service,ServiceEnvironment)
getServiceAndEnvironment
  appConf@(
    AppConf.ApplicationConfiguration
    {
      AppConf.appServices = theServices
    })
  requestInput =
  do
    (serviceId,service,appInput) <- lookupService theServices requestInput
    pure $ (service,newServiceEnvironment appConf serviceId appInput)

newServiceEnvironment :: AppConf.ApplicationConfiguration
                      -> ServiceId
                      -> AppInput.Input
                      -> ServiceEnvironment
newServiceEnvironment
  AppConf.ApplicationConfiguration
  {
    AppConf.translations                = theTranslations
  , AppConf.dbConfiguration             = theDbConfiguration
  , AppConf.serviceLinks                = theServiceLinks
  , AppConf.appLogger                   = theLogger
  }

  serviceId
  input =
    Service.newEnvironment serviceId
    (ElementSetIo.customEnvironment input)
    (ElementSetIo.inputMedia        input)
    theDbConfiguration outputing
    theLogger
  where
    outputing = UiOm.Outputing
        {
          UiOm.outTranslations = theTranslations
        , UiOm.outServiceLinks = theServiceLinks
        }
