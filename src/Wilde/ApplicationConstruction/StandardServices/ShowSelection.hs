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

-- | Implementation of the "ShowSelection"
--
-- Import this module qualified.
module Wilde.ApplicationConstruction.StandardServices.ShowSelection
       (
         module Wilde.Application.ObjectTypeService,

         AnyOtService(..),

         Utils.Config(..),

         mkService
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad

import qualified Wilde.Database.SqlJoin as Sql

import qualified Wilde.Media.ElementSet as ES

import Wilde.Media.UserInteraction

import Wilde.Driver.Application.Cgi.VariableNames as VariableNames (selectExpression)

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputWithPresentation
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import qualified Wilde.Media.MonadWithInputMedia as MIIA

import Wilde.Application.Service
import Wilde.Application.ObjectTypeService

import           Wilde.ApplicationConstruction.Service.StepService
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS

import qualified Wilde.ApplicationConstruction.StandardServices.ShowManyUtils as Utils

import qualified Wilde.ApplicationConstruction.UserInteraction.Input.WildeSqlInputer as SqlInputer
import qualified Wilde.ApplicationConstruction.UserInteraction.Widgets as StdWidgets


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


mkService :: (Database.DATABASE_TABLE otConf
             ,Database.INPUT_FOR_EXISTING atConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf)
          => [AnyO (OtServiceOtSetup Utils.Config otConf atConf)] -> AnyOtService
mkService ots = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction serviceMain
  , types = ots
  }

serviceMain :: (Database.DATABASE_TABLE otConf
               ,Database.INPUT_FOR_EXISTING atConf
               ,Presentation.ATTRIBUTE_PRESENTATION atConf
               ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf)
            => ObjectTypeServiceMainFunction Utils.Config otConf atConf dbTable otNative idAtExisting idAtCreate
serviceMain ot@(ObjectType {}) config@(Utils.Config title _) = stepService def
  where
    def :: StepService
    def = StepService
          {
            mainTitle    = title
          , nonLastSteps = [inputExpressionStep]
          , lastStep     = showSelection
          }

    inputExpressionStep :: NonLastStep
    inputExpressionStep = continue formBlockForInputExpression

    formBlockForInputExpression :: FormBlocksAndMetas
    formBlockForInputExpression =
      FormBlocksAndMetas
      {
        fbamMetas  = []
      , fbamBlocks = [inputExpressionBlock]
      }
    inputExpressionBlock =
      FormBlock
      {
        formBlockInteraction = [attributeOutputFormBlockRow attrOutput_whereInput]
      , formBlockMetaValues  = []
      }

    attrOutput_whereInput :: LabelAndWidget
    attrOutput_whereInput = StdWidgets.mkLabelAndWidgetFromInfo
                            ekSelection
                            "WHERE"
                            widgetInfo_whereInput
                            
    widgetInfo_whereInput :: StdWidgets.TextAreaInfo
    widgetInfo_whereInput =
      StdWidgets.TextAreaInfo
      {
        StdWidgets.textAreaSize = (50,10)
      , StdWidgets.textDefault  = Nothing
      }

    showSelection :: Service
    showSelection = do
      mbExpr <- inputExpression ot
      let getWhereExpr = Sql.liftMbExprInMonad mbExpr
      os <- toServiceMonadWithCar $
            InputWithPresentation.inputSelection ot theOrderByInDb getWhereExpr
      Utils.showMany ot config os

    theOrderByInDb = OLS.orderByInDb $ OLS.displaySetup $ Utils.objectListSetup config

-- | Inputs the object ID that was saved by 'outputOriginalObjectId'.
inputExpression :: Database.COLUMN_NAMES atConf
                => ObjectType otConf atConf dbTable otNative idAtE idAtC
                -> ServiceMonad (Maybe (Sql.SqlExpr dbTable))
inputExpression ot@(ObjectType {}) =
  MIIA.inInputMedia $ ES.mkLookuper lookupConverter ekSelection
  where
    lookupConverter = ES.singleton_mandatory >=>
                      ES.trimEmptyIsNothing >=>
                      ES.nothingIsNothing (SqlInputer.expression_mandatory ot)

ekSelection :: ElementKey
ekSelection = globalElementKey VariableNames.selectExpression
