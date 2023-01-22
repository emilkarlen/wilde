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

import qualified Wilde.Service.ServiceLink as ServiceLink


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
