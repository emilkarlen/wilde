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
-- | A services that shows a list of zero or more 'Object's.
--
-- Uses the standard filter in the Custom Environment:
--
-- _selection.<ObjectType-crossRefKey>
--
-- of "Wilde.ApplicationConstruction.StandardFilterExpression".
--
-- Import this module qualified.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.ShowMany
       (
         module Wilde.Application.ObjectTypeService,

         AnyOtService(..),

         Utils.Config(..),

         mkService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputWithPresentation
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import           Wilde.Application.ObjectTypeService
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS

import qualified Wilde.ApplicationConstruction.UserInteraction.Output.StandardFilterExpression as StdFilterExpr

import qualified Wilde.ApplicationConstruction.StandardServices.ShowManyUtils as Utils
import qualified Wilde.Application.Service as Service


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


mkService :: (Database.DATABASE_TABLE otConf
             ,Database.INPUT_FOR_EXISTING atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             )
          => [AnyO (OtServiceOtSetup Utils.Config otConf atConf)] -> AnyOtService
mkService ots = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction serviceMain
  , types = ots
  }

serviceMain :: (Database.DATABASE_TABLE otConf
               ,Presentation.ATTRIBUTE_PRESENTATION atConf
               ,Database.INPUT_FOR_EXISTING atConf
               ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
               )
            => ObjectTypeServiceMainFunction Utils.Config otConf atConf dbTable otNative idAtExisting idAtCreate
serviceMain ot@(ObjectType {}) config@(Utils.Config title objectListSetup) =
    do
      getMbExpr <- StdFilterExpr.lookupExpression_BasedOn ot
      os        <- Service.toServiceMonad_wDefaultDbConn $
                   InputWithPresentation.inputSelection ot theOrderByInDb getMbExpr
      Utils.showMany ot config os
  where
    theOrderByInDb = OLS.orderByInDb $ OLS.displaySetup $ Utils.objectListSetup config
