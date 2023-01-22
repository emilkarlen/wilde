-- | Utilities for the implementation of services.
module Wilde.ApplicationConstruction.Service.ObjectTypeServiceUtils
       (
         ObjectServiceMainFunction,
         ObjectIdServiceMainFunction,

         objectServiceMainFunction,
         objectIdServiceMainFunction,
       )
       where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Application.ObjectTypeService

import Wilde.Media.UserInteraction

import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputWithPresentation
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import qualified Wilde.Service.Monad as Service

import Wilde.Application.Service.Service

import Wilde.ApplicationConstruction.Service.ServiceTools

import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type ObjectServiceMainFunction config otConf atConf dbTable otNative idAtExisting idAtCreate =
  Object otConf atConf dbTable otNative idAtExisting idAtCreate ->
  config otConf atConf dbTable otNative idAtExisting idAtCreate ->
  Service

type ObjectIdServiceMainFunction config otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate ->
  config     otConf atConf dbTable otNative idAtExisting idAtCreate ->
  idAtExisting ->
  Service

objectServiceMainFunction :: (Database.DATABASE_TABLE otConf
                             ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                             ,OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                             )
                          => ObjectServiceMainFunction     config otConf atConf dbTable otNative idAtExisting idAtCreate
                          -> ObjectTypeServiceMainFunction config otConf atConf dbTable otNative idAtExisting idAtCreate
objectServiceMainFunction objectMainFun ot otConfig =
  objectIdServiceMainFunction objectIdMainFun ot otConfig
  where
    objectIdMainFun ot otConfig oId =
      do
        mbObj <- Service.toServiceMonad_wDefaultDbConn $ InputWithPresentation.inputOne ot oId
        maybe
          (throwErr $ NormalError "No such object")
          (\obj -> objectMainFun obj otConfig)
          mbObj

objectIdServiceMainFunction :: OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
  => ObjectIdServiceMainFunction   config otConf atConf dbTable otNative idAtExisting idAtCreate
  -> ObjectTypeServiceMainFunction config otConf atConf dbTable otNative idAtExisting idAtCreate
objectIdServiceMainFunction objectIdMainFun ot otConfig =
  do
    pk <- lookupGsr_mandatory
          (globalElementKey VariableNames.pk)
          (OmGsr.otInputerForIdAtForExisting ot)
    objectIdMainFun ot otConfig pk
