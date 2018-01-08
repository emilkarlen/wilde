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

-- | Utilities for constructing ServiceLinks for
-- 'ObjectType's and 'Object's.
module Wilde.Application.ObjectModelServiceLink
       (
         newGlobalServiceSpecification,
         newObjectTypeServiceSpecification,
         newObjectServiceSpecification,
         newGlobalServiceReference,

         newObjectTypeServiceReference,
         newObjectServiceReference,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import qualified Wilde.Application.ServiceLink as ServiceLink


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Constructs a 'ServiceLink.ServiceSpecification' for a global service.
newGlobalServiceSpecification :: ServiceLink.ServiceName
                              -> ServiceLink.ServiceSpecification
newGlobalServiceSpecification = ServiceLink.newGlobalServiceSpecification

-- | Constructs a 'ServiceLink.ServiceSpecification' for an 'ObjectType' Service.
newObjectTypeServiceSpecification :: ServiceLink.ServiceName
                                  -> ObjectType otConf atConf dbTable otNative idAtE idAtC
                                  -> ServiceLink.ServiceSpecification
newObjectTypeServiceSpecification theServiceName ot =
  ServiceLink.newObjectTypeServiceSpecification theServiceName otKey
  where
    otKey = otCrossRefKey ot

-- | Constructs a 'ServiceLink.ServiceSpecification' for an 'Object' Service.
newObjectServiceSpecification :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                              => ServiceLink.ServiceName
                              -> ObjectType otConf atConf dbTable otNative idAtE idAtC
                              -> idAtE
                              -> ServiceLink.ServiceSpecification
newObjectServiceSpecification theServiceName ot idAtE =
  ServiceLink.newObjectServiceSpecification
  theServiceName 
  otKey
  (idAtGenericStrRepOutputer idAtE)
  where
    otKey = otCrossRefKey ot
    idAt  = otIdAttributeType ot
    idAtGenericStrRepOutputer = OmGsr.atOutputerForExisting $
                                idAt

newGlobalServiceReference :: ServiceLink.ServiceName
                          -> ServiceLink.GlobalServiceReference
newGlobalServiceReference theServiceName =
  ServiceLink.newGlobalServiceReference theServiceName

newObjectTypeServiceReference :: ServiceLink.ServiceName
                              -> ObjectType otConf atConf dbTable otNative idAtE idAtC
                              -> ServiceLink.ObjectTypeServiceReference
newObjectTypeServiceReference theServiceName ot =
  ServiceLink.newObjectTypeServiceReference theServiceName otKey
  where
    otKey = otCrossRefKey ot

newObjectServiceReference :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                          => ServiceLink.ServiceName
                          -> ObjectType otConf atConf dbTable otNative idAtE idAtC
                          -> ServiceLink.ObjectServiceReference idAtE
newObjectServiceReference theServiceName ot =
  ServiceLink.newObjectServiceReference 
  idAtGenericStrRepOutputer
  theServiceName
  otKey
  where
    otKey = otCrossRefKey ot
    idAt  = otIdAttributeType ot
    idAtGenericStrRepOutputer = OmGsr.atOutputerForExisting $
                                idAt
