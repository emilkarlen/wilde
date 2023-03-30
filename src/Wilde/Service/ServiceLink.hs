-------------------------------------------------------------------------------
-- | Links to services of an application.
--
-- There are three \"forms\" of a link,
-- each form adds information to the previous:
--
-- 1. A reference to a service ('ServiceReferenceWithParamsConstructor')
--
-- 2. A reference to a service together with \"generic parameters\"
--    ('ServiceReferenceWithParams').
--
-- 3. A complete link that can be rendered ('ServiceLink')
-------------------------------------------------------------------------------

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Wilde.Service.ServiceLink
       (
         ServiceId(..),

         -- * Service Links

         ServiceName,
         GenericParameter,
         addGenericParams,
         addElementParams,

         ServiceLink(..),

         ServiceLinkConstructor,

         -- * A monad that can construct Service Links

         MonadWithServiceLinkConstructor(..),
         mkLink,

         -- * Service References

         ServiceSpecification(..),
         ServiceReferenceWithParams(..),
         ServiceReferenceWithParamsConstructor(..),

         -- ** Standard types of references

         GlobalServiceReference,
         ObjectTypeServiceReference,
         ObjectServiceReference,

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


import qualified Wilde.Media.ElementSet as ES
import           Wilde.Media.CustomEnvironment
import           Wilde.Media.GenericStringRep (GenericStringRep)
import           Wilde.Media.WildeMedia (CrossRefIdentifier)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Identifies a service.
data ServiceId =
  ServiceId
  {
    sidName       :: String,
    sidObjectType :: Maybe String
  }


-------------------------------------------------------------------------------
-- - ServiceLink -
-------------------------------------------------------------------------------


-- | A string that identifies a service in the applications set of services.
--
-- (Not for display purposes).
type ServiceName = String

-- | Generic parameter to a service in the form of (name,value).
--
-- TODO: Replace with (String,[String]) to match rep of Media??
type GenericParameter = (String,String)

-- | A HTML target that is a service of the current application.
data ServiceLink =
  ServiceLink
  {
    slServiceReferenceWithParams :: ServiceReferenceWithParams
  , slCustomEnvironment          :: ES.ElementSet
  }

-- | Adds parameters to a given 'ServiceLink'.
addGenericParams :: ServiceLink -> [GenericParameter] -> ServiceLink
addGenericParams serviceLink paramsToAdd =
  serviceLink { slServiceReferenceWithParams = newSrwp }
  where
    newSrwp   = oldSrwp { srwpGenericParams = newParams }
    oldSrwp   = slServiceReferenceWithParams serviceLink
    oldParams = srwpGenericParams oldSrwp
    newParams = paramsToAdd ++ oldParams

-- | Adds parameters to a given 'ServiceLink'.
addElementParams :: ServiceLink -> [ES.Element] -> ServiceLink
addElementParams serviceLink elementsToAdd =
  addGenericParams serviceLink $ map render elementsToAdd
  where
    render (ek,ev) = (ES.elementKeyRender ek,ev)


-------------------------------------------------------------------------------
-- - MonadWithServiceLinkConstructor -
-------------------------------------------------------------------------------


class Monad m => MonadWithServiceLinkConstructor m where
  getServiceLinkConstructor :: m ServiceLinkConstructor

mkLink :: MonadWithServiceLinkConstructor m
       => ServiceReferenceWithParams
       -> m ServiceLink
mkLink serviceReferenceWithParams =
  do
    constructor <- getServiceLinkConstructor
    pure $ constructor serviceReferenceWithParams

instance (Monad m, MonadWithCustomEnvironment m) => MonadWithServiceLinkConstructor m where
  getServiceLinkConstructor =
    do
      custEnv <- getCustomEnvironment
      pure $ \serviceReferenceWithParams ->
        ServiceLink
        {
          slServiceReferenceWithParams = serviceReferenceWithParams
        , slCustomEnvironment          = custEnv
        }

-- | A function that is provided by the system and that constructs
-- (\"concrete\") links from (\"abstract\") references.
type ServiceLinkConstructor =
  ServiceReferenceWithParams -> ServiceLink

-- | A reference to a \"concrete\" service with optional \"Generic Parameters\".
--
-- With \"concrete\" is meant that no further information from the \"user\"
-- is needed to make a 'ServiceLink' of it.
--
-- References to Global and Object Type Services are always \"concrete\".
-- Only Object Services can be \"non-concrete\" since an ID of an 'Object'
-- is needed (and this value cannot be determined (usualy) at compile time).
--
-- What is needed to make a \"concrete\" reference is the
-- CustomEnvironment.  But this is not supplied by the \"user\", but
-- added by the system when constucting the \"concrete\" reference in
-- a monad that has this information.
data ServiceReferenceWithParams =
  ServiceReferenceWithParams
  {
    srwpServiceSpecification :: ServiceSpecification
  , srwpGenericParams        :: [GenericParameter]
  }

-- | Specifies all types of services:
-- global, ObjectType and Object-service.
data ServiceSpecification =
  ServiceSpecification
  {
    ssServiceName    :: ServiceName
  , ssObjectTypeName :: Maybe CrossRefIdentifier
  , ssObjectIdentity :: Maybe GenericStringRep
  }

-- | A reference to a Service.
type ServiceReferenceWithParamsConstructor a = a -> ServiceReferenceWithParams

type GlobalServiceReference       = ServiceReferenceWithParamsConstructor ()
type ObjectTypeServiceReference   = ServiceReferenceWithParamsConstructor ()
type ObjectServiceReference idAtE = ServiceReferenceWithParamsConstructor idAtE


-------------------------------------------------------------------------------
-- - new...ServiceSpecification -
-------------------------------------------------------------------------------


-- | Constructs a 'ServiceSpecification' for a global service.
newGlobalServiceSpecification :: ServiceName
                              -> ServiceSpecification
newGlobalServiceSpecification theServiceName =
  ServiceSpecification
  {
    ssServiceName    = theServiceName
  , ssObjectTypeName = Nothing
  , ssObjectIdentity = Nothing
  }

-- | Constructs a 'ServiceSpecification' for an 'ObjectType' Service.
newObjectTypeServiceSpecification :: ServiceName
                                  -> CrossRefIdentifier
                                  -> ServiceSpecification
newObjectTypeServiceSpecification theServiceName otKey =
  ServiceSpecification
  {
    ssServiceName    = theServiceName
  , ssObjectTypeName = Just otKey
  , ssObjectIdentity = Nothing
  }

-- | Constructs a 'ServiceSpecification' for an 'Object' Service.
newObjectServiceSpecification :: ServiceName
                              -> CrossRefIdentifier
                              -> GenericStringRep
                              -> ServiceSpecification
newObjectServiceSpecification theServiceName otKey idAtGsr =
  ServiceSpecification
  {
    ssServiceName    = theServiceName
  , ssObjectTypeName = Just otKey
  , ssObjectIdentity = Just idAtGsr
  }


-------------------------------------------------------------------------------
-- - new...ServiceReference -
-------------------------------------------------------------------------------


newGlobalServiceReference :: ServiceName
                          -> GlobalServiceReference
newGlobalServiceReference theServiceName _ =
  ServiceReferenceWithParams
  {
    srwpServiceSpecification = newGlobalServiceSpecification theServiceName
  , srwpGenericParams        = []
  }

newObjectTypeServiceReference :: ServiceName
                              -> CrossRefIdentifier
                              -> ObjectTypeServiceReference
newObjectTypeServiceReference theServiceName otKey _ =
  ServiceReferenceWithParams
  {
    srwpServiceSpecification = newObjectTypeServiceSpecification
                               theServiceName
                               otKey
  , srwpGenericParams = []
  }

newObjectServiceReference :: (idAtE -> GenericStringRep)
                          -> ServiceName
                          -> CrossRefIdentifier
                          -> ObjectServiceReference idAtE
newObjectServiceReference idAtRenderer theServiceName otKey idAtE =
  ServiceReferenceWithParams
  {
    srwpServiceSpecification = newObjectServiceSpecification
                               theServiceName
                               otKey
                               (idAtRenderer idAtE)
  , srwpGenericParams = []
  }
