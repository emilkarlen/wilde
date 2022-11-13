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

-- | Tools for constructing the services of a Wilde application.
module Wilde.ApplicationConstruction.Service.ServiceTools
       (
         module Wilde.Application.Service,

         -- * Object Service

         withObjectIdFromEnv,
         withObjectFromDb,
         withObjectFromDbWithIdFromEnv,
         
         objectService,
         objectServiceForId,
         objectServiceHandle,
         
         -- * Utilities

         currentServiceLink,
         formForCurrentService,
         formForService,
         readAllPlain,
         
         lookupGsr_mandatory,
         
         -- * Error handling
         
         swallowError,
         otUiObjectInputErrorMonad,
         otUiObjectInputErrorMonads,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Either

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.Media.ElementSet as ES

import qualified Wilde.Media.MonadWithInputMedia as MIIA
import Wilde.Media.WildeMedia as WM
import Wilde.Media.GenericStringRep
import Wilde.Media.UserInteraction

import Wilde.ObjectModel.UserInteraction
import Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputWithPresentation
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Execution.SelectSansPresentationInfo as SelectPlain
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames

import Wilde.Application.Service
import qualified Wilde.Application.Service as Service
import qualified Wilde.Application.ServiceLink as ServiceLink

import qualified Wilde.ApplicationConstruction.ElementSetUtils as ESU


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


readAllPlain :: (Database.DATABASE_TABLE otConf
                ,Database.INPUT_FOR_EXISTING atConf
                ,Database.COLUMN_NAMES atConf
                )
             => ObjectType otConf atConf dbTable otN idAtExisting idAtCreate
             -> ServiceMonad [Object otConf atConf dbTable otN idAtExisting idAtCreate]
readAllPlain ot = Service.toServiceMonad_wDefaultDbConn $ SelectPlain.selectAll ot []

formForCurrentService :: FormBlocksAndMetas
                      -> [Element]
                      -> ServiceMonad Form
formForCurrentService formBlocksAndMetas formMetas =
  do
    serviceId  <- getEnvs envCurrentService
    mbObjectId <- MIIA.inInputMedia $ ES.lookupSingleton_optional objectIdElementKey
    let objectIdElements = maybe [] (\val -> [(objectIdElementKey,val)]) mbObjectId
    return $ formForService formBlocksAndMetas (objectIdElements ++ formMetas) serviceId
  where
    objectIdElementKey = globalElementKey VariableNames.pk
    
currentServiceLink :: ServiceMonad ServiceLink.ServiceLink
currentServiceLink =
  do
    ServiceId theServiceName theMbServiceOt <- getEnvs envCurrentService
    mbCustomEnv <- getEnvs envCustomEnvironment
    mbObjectId <- MIIA.inInputMedia $
                  ES.lookupSingleton_optional (globalElementKey VariableNames.pk)
    return $ ServiceLink.ServiceLink
             {
               ServiceLink.slServiceReferenceWithParams =
                  ServiceLink.ServiceReferenceWithParams
                  {
                    ServiceLink.srwpServiceSpecification =
                       ServiceLink.ServiceSpecification
                       {
                         ServiceLink.ssServiceName    = theServiceName
                       , ServiceLink.ssObjectTypeName = theMbServiceOt
                       , ServiceLink.ssObjectIdentity = mbObjectId
                       }
                  , ServiceLink.srwpGenericParams = []
                  }
             , ServiceLink.slCustomEnvironment = mbCustomEnv
             }

formForService :: FormBlocksAndMetas
               -> [Element]
               -> ServiceId
               -> Form
formForService formBlocksAndMetas formMetas (ServiceId serviceName mbServiceOt) =
  formForFormBlocksAndMetas formBlocksAndMetas formMetaValues Nothing
  where
    formMetaValues = valServiceName : valObjectType ++ formMetas :: [Element]
    valServiceName = (globalElementKey VariableNames.service,serviceName)
    valObjectType = maybe
                    []
                    (\otName -> [(globalElementKey VariableNames.objectType,otName)])
                    mbServiceOt


-------------------------------------------------------------------------------
-- - Object Service -
-------------------------------------------------------------------------------

withObjectIdFromEnv :: OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                    => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                    -> (idAtExisting -> ServiceMonad a)
                    -> ServiceMonad a
withObjectIdFromEnv ot f =
  do
    pk <- lookupGsr_mandatory
          (globalElementKey VariableNames.pk)
          (OmGsr.otInputerForIdAtForExisting ot)
    f pk

withObjectFromDb :: (Database.DATABASE_TABLE otConf
                    ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                    ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                    )
                 => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> (Object    otConf atConf dbTable otNative idAtExisting idAtCreate -> ServiceMonad a)
                 -> idAtExisting
                 -> ServiceMonad a
withObjectFromDb ot@(ObjectType {}) f pk =
  do
    mbObj <- Service.toServiceMonad_wDefaultDbConn $ InputWithPresentation.inputOne ot pk
    maybe
      (throwErr $ NormalError $ "No object in the database with id:" ++ show pk)
      f
      mbObj

withObjectFromDbWithIdFromEnv :: (Database.DATABASE_TABLE otConf
                                 ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                                 ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                                 ,OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                                 )
                              => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                              -> (Object    otConf atConf dbTable otNative idAtExisting idAtCreate -> ServiceMonad a)
                              -> ServiceMonad a
withObjectFromDbWithIdFromEnv ot f = withObjectIdFromEnv ot (withObjectFromDb ot f)
  
  
-------------------------------------------------------------------------------
-- | Utility to construct a service that operats on an 'Object' that
-- exists in the database.
--
-- The 'Object' is specified by it's PK, which is given explicitly.
--
-- /Throws/
--
-- * there is no 'Object' in the database with the given PK.
-------------------------------------------------------------------------------
objectServiceHandle :: (Database.DATABASE_TABLE otConf
                       ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                       ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                       )
                    => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                    -> (Object    otConf atConf dbTable otNative idAtExisting idAtCreate -> Service)
                    -> idAtExisting
                    -> Service
objectServiceHandle = withObjectFromDb

-------------------------------------------------------------------------------
-- A variant of 'objectServiceHandle'.
-------------------------------------------------------------------------------
objectServiceForId :: OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                   => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                   -> (ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> idAtExisting
                       -> Service)
                   -> Service
objectServiceForId ot serviceForPk =
  do
    pk <- lookupGsr_mandatory
          (globalElementKey VariableNames.pk)
          (OmGsr.otInputerForIdAtForExisting ot)
    serviceForPk ot pk

-------------------------------------------------------------------------------
-- | Utility to construct a service that operats on an 'Object' that
-- exists in the database.
--
-- The 'Object' is specified by it's PK, which is read from the UI media
-- element with key 'VariableNames.pk'.
--
-- /Throws/
--
-- * there is no 'Object' in the database with the given PK.
--
-- * 'VariableNames.pk' does not exist in the UI media, or have invalid syntax.
-------------------------------------------------------------------------------
objectService :: (Database.DATABASE_TABLE otConf
                 ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                 ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                 ,OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                 )
              => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
              -> (Object otConf atConf dbTable otNative idAtExisting idAtCreate -> Service)
              -> Service
objectService ot serviceForObject =
  do
    pk <- lookupGsr_mandatory
          (globalElementKey VariableNames.pk)
          (OmGsr.otInputerForIdAtForExisting ot)
    mbObj <- Service.toServiceMonad_wDefaultDbConn $ InputWithPresentation.inputOne ot pk
    maybe
      (throwErr $ NormalError "No such object")
      serviceForObject
      mbObj

-- | Looks up a mandatory value encoded as a single string in
-- the Generic String Representation.
lookupGsr_mandatory :: ElementKey
                    -> GenericStringRepInputer a
                    -> ServiceMonad a
lookupGsr_mandatory ek gsrInputer =
  MIIA.inInputMedia (lookuper ek)
  where
    lookuper = ESU.gsr_lookuper gsrInputer

swallowError :: ObjectInputResult a -> ServiceMonad a
swallowError result =
    case result of
      Left errorInfo -> throwErr $ UiObjectInputError $ NonEmpty.singleton errorInfo
      Right x        -> return x

otUiObjectInputErrorMonad :: ServiceMonad (ObjectInputResult a)
                          -> ServiceMonad a
otUiObjectInputErrorMonad m = m >>= swallowError

otUiObjectInputErrorMonads :: (Monad m,ToServiceMonad m)
                           => [m (ObjectInputResult a)]
                           -> ServiceMonad [a]
otUiObjectInputErrorMonads ms =
  do
    result <- toServiceMonad $ combineAllErrors ms
    case result of
      Left error -> throwErr $ UiObjectInputError error
      Right xs   -> return xs
  where
    combineAllErrors :: Monad m => [m (ObjectInputResult a)]
                    -> m (Either (NonEmpty.List ObjectInputErrorInfo) [a])
    combineAllErrors ms = do
      results <- sequence ms
      let (errors,oks) = partitionEithers results
      return $
        case errors of
          (e:es) -> Left $ NonEmpty.mk e es
          _      -> Right oks
