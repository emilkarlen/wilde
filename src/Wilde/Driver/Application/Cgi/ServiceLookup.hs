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
          Right srvc -> return (ServiceId sSrvc mbOt,srvc,inputWithoutServiceSpec)
            where inputWithoutServiceSpec =
                    input {
                      AppInput.inputMedia = maybe id (\otId -> ElementSet.deleteRaw_stringKey otId) mbOt $
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
    Nothing  -> return Nothing
    Just [v] -> return (Just v)
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
    return $ (service,newServiceEnvironment appConf serviceId appInput)

newServiceEnvironment :: AppConf.ApplicationConfiguration
                      -> ServiceId
                      -> AppInput.Input
                      -> ServiceEnvironment
newServiceEnvironment
  AppConf.ApplicationConfiguration
  {
    AppConf.translations                = theTranslations
  , AppConf.dbConfiguration             = theDbConfiguration
  , AppConf.standardServiceLinkRenderer = theStdSrvcLinkRenderer
  , AppConf.getMkStdObjectTypeService   = theStdObjectTypeSrvcLinkRenderer
  , AppConf.getMkStdObjectService       = theStdObjectSrvcLinkRenderer
  , AppConf.getMkGenericServiceLink     = theGetGenericSLR
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
          UiOm.outTranslations                = theTranslations
        , UiOm.outStandardServiceLinkRenderer = theStdSrvcLinkRenderer
        , UiOm.outMkStdObjectTypeServiceLink  = theStdObjectTypeSrvcLinkRenderer
        , UiOm.outMkStdObjectServiceLink      = theStdObjectSrvcLinkRenderer
        , UiOm.outgetMkGenericServiceLink     = theGetGenericSLR
        }
