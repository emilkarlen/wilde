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
-- | Utilities for implementing services that inputs an 'Object' from the
-- user.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.CreateOneUtils
       (
         module Wilde.Media.UserInteraction.Output,
         module Wilde.Media.UserInteraction.Io,

         inputFromUi_store_show,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.UserInteraction.Output
import Wilde.Media.UserInteraction.Io

import Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.ObjectModel.UserInteraction.Input.ForCreate as InputForCreate
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import Wilde.Application.Service.Service

import qualified Wilde.ApplicationConstruction.Service.ServiceUtils as ServiceUtils

import qualified Wilde.ApplicationConstruction.Service.ServiceTools as ServiceTools


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - output -
-------------------------------------------------------------------------------




-------------------------------------------------------------------------------
-- - input -
-------------------------------------------------------------------------------


inputFromUi_store_show :: (Database.OBJECT_TYPE_INSERT otConf
                          ,Presentation.ATTRIBUTE_PRESENTATION atConf
                          ,Database.DATABASE_IO atConf
                          ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
                          ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                          )
                       => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> StyledTitle
                       -> [Any (AttributeType atConf dbTable)]
                       -> ObjectName
                       -> Service
inputFromUi_store_show ot title attributeTypesOrder objectName =
  ServiceUtils.createObject ot objectName >>=
  ServiceTools.swallowError >>=
  ServiceUtils.showOnePageService attributeTypesOrder title >>=
  pageOkResult
