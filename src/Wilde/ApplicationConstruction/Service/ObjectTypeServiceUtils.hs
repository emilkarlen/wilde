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

import Wilde.ApplicationConstruction.Service.ServiceTools

import qualified Wilde.Render.Cgi.VariableNames as VariableNames


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
        mbObj <- withDbConnectionCar $ 
          toServiceMonad . InputWithPresentation.inputOne ot oId
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
