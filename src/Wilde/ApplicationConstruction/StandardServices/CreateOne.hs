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

{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | The standard service \"Create One\".
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.CreateOne
       (

         Config(..),
         mkService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModel

import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation
import qualified Wilde.ObjectModel.UserInteraction.Output.ForCreateFrom as OutputForCreateFrom
import qualified Wilde.ObjectModel.UserInteraction.Output.ForCreate as OutputForCreate
import qualified Wilde.ObjectModel.UserInteraction.Input.ForCreate as InputForCreate
import qualified Wilde.ObjectModel.Presentation as Presentation

import qualified Wilde.ApplicationConstruction.Service.ServiceUtils as ServiceUtils
import           Wilde.ApplicationConstruction.Service.StepService
import           Wilde.Application.ObjectTypeService

import Wilde.ApplicationConstruction.StandardServices.CreateOneUtils
import Wilde.ApplicationConstruction.StandardServices.SingleObjectServiceCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Config (otConf :: * -> * -> * -> * -> *) atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    titles :: ServiceUtils.TwoStepServiceTitles
    -- | Specifies the order in which
    -- the 'AttributeType's inputers are listed.
    -- The list must be a permutation of all 'AttributeType's of the
    -- 'ObjectType'.
  , attributeTypesOrder :: [Any (AttributeType atConf dbTable)]
  }

mkService :: (Database.OBJECT_TYPE_INSERT otConf
             ,Database.DATABASE_IO atConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
             ,OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
             )
          => [AnyO (OtServiceOtSetup Config otConf atConf)] -> AnyOtService
mkService otss = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction createOneMain
  , types = otss
  }

createOneMain :: (Database.OBJECT_TYPE_INSERT otConf
                 ,Database.DATABASE_IO atConf
                 ,Presentation.ATTRIBUTE_PRESENTATION atConf
                 ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                 ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
                 ,OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                 )
              => ObjectTypeServiceMainFunction Config otConf atConf dbTable otNative idAtExisting idAtCreate
createOneMain ot (Config titles attributeTypesOrder) = stepService def
  where
    def :: StepService
    def = StepService
          {
            mainTitle    = ServiceUtils.page1Title titles
          , nonLastSteps = [outputForm]
          , lastStep     = inputFromUi_store_show ot
                           (ServiceUtils.page2Title titles)
                           attributeTypesOrder theObjectName
          }

    outputForm :: NonLastStep
    outputForm = toServiceMonad outputForm' >>= continue

    outputForm' :: UserInteractionOutputMonad FormBlocksAndMetas
    outputForm' =
      do
        formBlock <- OutputForCreate.outputerForStdSetup attributeTypesOrder theObjectName
        return $ FormBlocksAndMetas [] [formBlock]
