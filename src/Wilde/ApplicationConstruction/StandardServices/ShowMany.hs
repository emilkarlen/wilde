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
import qualified Wilde.Service.Monad as Service


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
