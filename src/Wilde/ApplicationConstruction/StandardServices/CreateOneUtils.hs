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
import           Wilde.WildeUi.UiPrimitives


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
                       -> WildeTitle
                       -> [Any (AttributeType atConf dbTable)]
                       -> ObjectName
                       -> Service
inputFromUi_store_show ot title attributeTypesOrder objectName =
  ServiceUtils.createObject ot objectName >>=
  ServiceTools.swallowError >>=
  ServiceUtils.showOnePageService attributeTypesOrder title >>=
  pageOkResult
