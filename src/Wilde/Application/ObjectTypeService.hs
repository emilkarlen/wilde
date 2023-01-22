{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | The structures that define a service that is generic
-- in the sense that it can be executed for more than one 'ObjectType'.
--
-- Examples of such services are: \"Create One\", \"Show Many\".
--
-- The other types of services are \"global services\" and
-- \"object services\".
--
-- An Object Type Service has a single \"main\" function.
-- This function is passed the 'ObjectType' and \"configuration\" for this
-- 'ObjectType'.
--
-- An Object Type Service is defined for a set of 'ObjectType's.
-- This set is specified by the developer.
-------------------------------------------------------------------------------
module Wilde.Application.ObjectTypeService
       (
         module Wilde.Service.Monad,
         module Wilde.ObjectModel.ObjectModel,

         -- * Representation of Object Type Services

         OtService(..),
         AnyOtService(..),
         AnyOtServiceOtSetup(..), -- experimental
         otServiceAny,
         otService,
         OtServiceOtSetup(..),
         ObjectTypeServiceMainFunction,
         AnyObjectTypeServiceMainFunction(..),

         -- * Utilities for Application main functions

         lookupService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Service.Monad

import Wilde.Application.Service.Service


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Complete information about an Object Type Service, except name.
-- This is what is needed in order to execute the service.
-------------------------------------------------------------------------------
data OtService config otConf atConf =
  OtService
  {
    main  :: AnyObjectTypeServiceMainFunction config otConf atConf
  , types :: [AnyO (OtServiceOtSetup          config otConf atConf)]
  }

-------------------------------------------------------------------------------
-- | Type that hides all types of an Object Type Service.
--
-- Makes it possible to store different Object Type Services in the
-- same container.
-------------------------------------------------------------------------------
data AnyOtService = forall config otConf atConf . AnyOtService (OtService config otConf atConf)

-------------------------------------------------------------------------------
-- | The 'ObjectType' together with configuration.
--
-- The configuration is passed to the service's \"main\" together with
-- the 'ObjectType'.  It makes it possible to configure the main function
-- for individual 'ObjectType's.
-------------------------------------------------------------------------------
data OtServiceOtSetup config otConf atConf dbTable otNative idAtExisting idAtCreate =
  OtServiceOtSetup
  {
    setupOt     :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
  , setupConfig :: config     otConf atConf dbTable otNative idAtExisting idAtCreate
  }

data AnyOtServiceOtSetup =
  forall config otConf atConf dbTable otNative idAtExisting idAtCreate .
  AnyOtServiceOtSetup (OtServiceOtSetup config otConf atConf dbTable otNative idAtExisting idAtCreate)


-------------------------------------------------------------------------------
-- | The type of the \"main\" function of an Object Type Service.
-------------------------------------------------------------------------------
type ObjectTypeServiceMainFunction config otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate ->
  config     otConf atConf dbTable otNative idAtExisting idAtCreate ->
  Service

-------------------------------------------------------------------------------
-- | A main function with all types but the configuration hidden.
--
-- This type makes it possible to make a list of all 'ObjectType's that
-- the service should be able to work on.
-------------------------------------------------------------------------------
data AnyObjectTypeServiceMainFunction config otConf atConf =
  AnyObjectTypeServiceMainFunction (
    forall dbTable otNative idAtExisting idAtCreate .
    ObjectTypeServiceMainFunction config otConf atConf dbTable otNative idAtExisting idAtCreate)

otServiceAny :: AnyObjectTypeServiceMainFunction config otConf atConf
             -> AnyO (OtServiceOtSetup config otConf atConf)
             -> Service
otServiceAny mainFun (AnyO otSetup) = otService mainFun otSetup

otService :: AnyObjectTypeServiceMainFunction config otConf atConf
          -> OtServiceOtSetup                 config otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Service
otService (AnyObjectTypeServiceMainFunction f) setup =
  f (setupOt setup) (setupConfig setup)

-------------------------------------------------------------------------------
-- | Looks up the service for a given 'ObjectType'.
--
-- This is a helper method for application main functions.
--
-- Not needed for implementation of services.
-------------------------------------------------------------------------------
lookupService :: OtService config otConf atConf -> CrossRefIdentifier -> Maybe Service
lookupService service objectTypeId =
  do
    setup <- lookupSetup  (types service)
    return $ otServiceAny (main  service) setup
  where
    lookupSetup :: [AnyO (OtServiceOtSetup config otConf atConf)] -> Maybe (AnyO (OtServiceOtSetup config otConf atConf))
    lookupSetup = find isMatch

    isMatch :: AnyO (OtServiceOtSetup config otConf atConf) -> Bool
    isMatch = anyOApply $ (objectTypeId==) . otCrossRefKey . setupOt
