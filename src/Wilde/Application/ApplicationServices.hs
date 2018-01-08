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

-------------------------------------------------------------------------------
-- | The tools for defining the services of an application.
-------------------------------------------------------------------------------
module Wilde.Application.ApplicationServices
       (
         ObjectTypeService.AnyOtService(..),

         -- * Construction of services for an application

         mkGlobalService,
         mkObjectTypeService,
         mkObjectTypeServiceRaw,

         -- * Stuff for implementation of application main functions

         ServicesMap,
         ApplicationService,
         ApplicationServices,
         Wilde.Application.ApplicationServices.lookupService,
         LookupError(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Wilde.Application.Service

import Wilde.Media.WildeMedia
import Wilde.Application.ObjectTypeService as ObjectTypeService


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | All services of an application.
-------------------------------------------------------------------------------
type ApplicationServices = ServicesMap ApplicationService

-- | The type of map for storage of services.
type ServicesMap a = Map.Map ServiceName a

-- | Either a Global Serivce or an Object Type Service.
type ApplicationService = Either Service ObjectTypeService.AnyOtService

-- | Makes a Global Service.
mkGlobalService :: Service -> ApplicationService
mkGlobalService = Left

-- | Makes an Object Type Service.
mkObjectTypeServiceRaw :: OtService config otConf atConf -> ApplicationService
mkObjectTypeServiceRaw = mkObjectTypeService . ObjectTypeService.AnyOtService

-- | Makes an Object Type Service.
mkObjectTypeService :: ObjectTypeService.AnyOtService -> ApplicationService
mkObjectTypeService = Right

-------------------------------------------------------------------------------
-- | Looks up a service in an application's set of services.
--
-- | Helper for implementing the application main function.
-------------------------------------------------------------------------------
lookupService :: ApplicationServices
              -> ServiceName
              -> Maybe CrossRefIdentifier
              -> Either LookupError Service
lookupService services name mbOtId =
    case Map.lookup name services of
      Nothing -> Left $ UnknownService name
      Just service ->
        case service of
          Left globalService ->
            maybe (return globalService) (const $ Left $ InvalidServiceType True) mbOtId
          Right (ObjectTypeService.AnyOtService otServices) ->
            case mbOtId of
              Nothing   -> Left $ InvalidServiceType False
              Just otId ->
                case ObjectTypeService.lookupService otServices otId of
                  Nothing      -> Left ObjectTypeNotSupportedByService
                  Just service -> return service

-- | Service lookup error.
data LookupError = UnknownService String -- ^ Service does not exist
                   -- | Found OT-service, but it has no implementation
                   -- for the given ObjectType.
                 | ObjectTypeNotSupportedByService
                   -- | Found a service but it is of the wrong type,
                   -- (global/ObjectType).
                 | InvalidServiceType { existingServiceIsGlobal :: Bool }
                 deriving Show
