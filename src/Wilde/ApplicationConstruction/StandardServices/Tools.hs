-------------------------------------------------------------------------------
-- | Tools foor constructing the Object Model of an application.
--
-- Functionallity for customizing an 'ObjectTypeSetup'.
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.StandardServices.Tools
       (

         objectTypeSetup,

         -- * Modifying values

         withAlternativeAtsOrder,
         withDependentComponents,
         withObjectListDisplaySetup,
         withModifiedObjectListDisplaySetup,
         setFooterRowsConstructor,
         setOrderByInDb,
         withDeleteSteps,
         withPresentationOutputer,

         -- * 'GetFooterRowsConstructor's

         noFooterRows,
         numberOfObjectsFooterRow,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.WildeUi.TableUtils (dataCellSpaned)
import           Wilde.WildeUi.StdValueTypes (IntValue(..))

import qualified Wilde.Media.Presentation as Presentation
import           Wilde.Media.WildeValue (AnySVALUE)

import           Wilde.ObjectModel.ObjectModel as OM

import qualified Wilde.ObjectModel.Presentation as OmPres
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import qualified Wilde.ApplicationConstruction.StandardServices.DeleteOne as DeleteOne
import           Wilde.ApplicationConstruction.UserInteraction.Output.ObjectDependentComponent
import qualified Wilde.ApplicationConstruction.StandardServices.UpdateOne as UpdateOne
import           Wilde.Application.StandardServices (StandardObjectServiceEnum)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Constructs an 'ObjectTypeSetup' without an "Alternative AttributeType Order", and
-- without any "Dependent Components".
--
-- All but the ID attribute are updatable.
-------------------------------------------------------------------------------
objectTypeSetup
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     (Database.DATABASE_TABLE  otConf
     ,Database.IO_FOR_EXISTING atConf
     ,Database.COLUMN_NAMES    atConf
     ,OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
      )
  => ObjectType                       otConf atConf dbTable otNative idAtExisting idAtCreate
  -> StyledTitle
  -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
objectTypeSetup ot titleWithStyle =
  StandardServices.ObjectTypeSetup
  {
    StandardServices.objectType          = ot
  , StandardServices.titleWithStyle      = titleWithStyle
  , StandardServices.objectListSetup     = ols
  , StandardServices.alternativeAtsOrder = []
  , StandardServices.dependentComponents = []
  , StandardServices.deleteSteps         = DeleteOne.defaultSteps ot
  , StandardServices.updateConfig =
    UpdateOne.mkDisplayed (Any (otIdAttributeType ot)) :
    map UpdateOne.mkUpdatable (otNonIdAttributeTypes ot)
  }
  where
    ols = OLS.ObjectListSetup
          {
            OLS.displaySetup = displaySetup
          , OLS.buttonsSetup = objectListButtonsSetup ot
          }

    displaySetup = OLS.ObjectListDisplaySetup
                   {
                     OLS.displayAts               = otAttributeTypes ot
                   , OLS.orderByInDb              = []
                   , OLS.getFooterRowsConstructor = numberOfObjectsFooterRow
                   }

objectListButtonsSetup
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
  => ObjectType                 otConf atConf dbTable otNative idAtExisting idAtCreate
  -> OLS.ObjectListButtonsSetup otConf atConf dbTable otNative idAtExisting idAtCreate
objectListButtonsSetup ot =
  OLS.ObjectListButtonsSetup
  {
    OLS.objectButtonsLeft      = mkObjectServiceLinkButtonCons
                                 [StandardServices.ShowOne]
  , OLS.objectButtonsRight     = mkObjectServiceLinkButtonCons
                                 [StandardServices.UpdateOne
                                 ,StandardServices.CreateOneFrom
                                 ,StandardServices.DeleteOne
                                 ]
  , OLS.objectTypeButtonsBelow = [StandardServices.createOneLinkButton ot]
  }
  where
    mkObjectServiceLinkButtonCons :: [StandardObjectServiceEnum]
                                  -> [Presentation.Monad (Object otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE)]
    mkObjectServiceLinkButtonCons = map (buttonFromIdToO . mkObjectServiceLinkButtonCon)

    mkObjectServiceLinkButtonCon :: StandardObjectServiceEnum
                                 -> Presentation.Monad (idAtExisting -> AnySVALUE)
    mkObjectServiceLinkButtonCon oServiceEnum =
      StandardServices.newObjectServiceLinkButtonConstructor
      ot
      oServiceEnum

    buttonFromIdToO :: Presentation.Monad (idAtExisting  -> AnySVALUE)
                    -> Presentation.Monad (Object otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE)
    buttonFromIdToO getMkFromId = do
      mkFromId <- getMkFromId
      pure $ \o -> mkFromId (OM.attrValue $ OM.oIdAttribute o)


-------------------------------------------------------------------------------
-- - Builders for ObjectTypeSetup -
-------------------------------------------------------------------------------


-- | Sets the "Alternative AttributeType Order" of an 'ObjectTypeSetup'.
withAlternativeAtsOrder :: StandardServices.ObjectTypeSetup    otConf atConf dbTable otNative idAtExisting idAtCreate
                           -> [Any (AttributeType atConf dbTable)]
                           -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
withAlternativeAtsOrder ots altOrder = ots { StandardServices.alternativeAtsOrder = altOrder }

-- | Sets the "Dependent Components" of an 'ObjectTypeSetup'.
withDependentComponents :: StandardServices.ObjectTypeSetup    otConf atConf dbTable otNative idAtExisting idAtCreate
                           -> [ObjectDependentComponent        otConf atConf dbTable otNative idAtExisting idAtCreate]
                           -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
withDependentComponents ots depComps =
  ots { StandardServices.dependentComponents = depComps }

-- | Sets the "Delete Steps" of an 'ObjectTypeSetup'.
withDeleteSteps :: StandardServices.ObjectTypeSetup    otConf atConf dbTable otNative idAtExisting idAtCreate
                   -> DeleteOne.Steps idAtExisting
                   -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
withDeleteSteps ots steps =
  ots { StandardServices.deleteSteps = steps }

-- | Sets the 'OLS.ObjectListDisplaySetup' of an 'ObjectTypeSetup'.
withObjectListDisplaySetup :: StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
                           -> OLS.ObjectListDisplaySetup       otConf atConf dbTable otNative idAtExisting idAtCreate
                           -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
withObjectListDisplaySetup ots newDisplaySetup =
  ots { StandardServices.objectListSetup = newObjectListSetup }
  where
    newObjectListSetup = oldObjectListSetup { OLS.displaySetup = newDisplaySetup }
    oldObjectListSetup = StandardServices.objectListSetup ots


type ObjectListDisplaySetupModifier otConf atConf dbTable otNative idAtExisting idAtCreate =
     OLS.ObjectListDisplaySetup       otConf atConf dbTable otNative idAtExisting idAtCreate
  -> OLS.ObjectListDisplaySetup       otConf atConf dbTable otNative idAtExisting idAtCreate


-- | Sets the 'OLS.ObjectListDisplaySetup' of an 'ObjectTypeSetup'.
withModifiedObjectListDisplaySetup :: StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
                                   -> ObjectListDisplaySetupModifier   otConf atConf dbTable otNative idAtExisting idAtCreate
                                   -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
withModifiedObjectListDisplaySetup ots modifier =
  ots { StandardServices.objectListSetup = newObjectListSetup }
  where
    newObjectListSetup = oldObjectListSetup { OLS.displaySetup = newDisplaySetup }
    oldObjectListSetup = StandardServices.objectListSetup ots
    newDisplaySetup    = modifier $ OLS.displaySetup oldObjectListSetup

-- | Modifier of 'OLS.ObjectListDisplaySetup'.
setFooterRowsConstructor :: OLS.GetFooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate
                         -> ObjectListDisplaySetupModifier   otConf atConf dbTable otNative idAtExisting idAtCreate
setFooterRowsConstructor x olds =
  OLS.ObjectListDisplaySetup
  {
    OLS.displayAts               = OLS.displayAts olds
  , OLS.orderByInDb              = OLS.orderByInDb olds
  , OLS.getFooterRowsConstructor = x
  }

-- | Modifier of 'OLS.ObjectListDisplaySetup'.
setOrderByInDb :: [Any (AttributeType atConf dbTable)]
               -> ObjectListDisplaySetupModifier   otConf atConf dbTable otNative idAtExisting idAtCreate
setOrderByInDb x olds = olds { OLS.orderByInDb = x }

-- | Sets the 'PresentationOutputer' of the given 'AttributeType'.
withPresentationOutputer :: AttributeType atConf dbTable e c
                         -> PresentationOutputer e
                         -> AttributeType atConf dbTable e c
withPresentationOutputer at newOutputer = at { atPresentationO = newOutputer }


-------------------------------------------------------------------------------
-- - GetFooterRowsConstructor:s -
-------------------------------------------------------------------------------


-- | A 'GetFooterRowsConstructor' that generates no footer rows.
noFooterRows :: OLS.GetFooterRowsConstructor () otConf atConf dbTable otNative idAtExisting idAtCreate
noFooterRows = pure Nothing

-- | A 'GetFooterRowsConstructor' that outputs one line with the
-- number of objects in the list.
numberOfObjectsFooterRow :: OLS.GetFooterRowsConstructor () otConf atConf dbTable otNative idAtExisting idAtCreate
numberOfObjectsFooterRow = pure $ Just frc
  where
    frc =
      OmPres.FooterRowsConstructor
      {
        OmPres.frcInitial     = ()
      , OmPres.frcAccumulator = const ()
      , OmPres.frcMkRows      = \ colInfos numObjects () ->
         if numObjects <= 1
         then ([],[])
         else
           let
             numObjectsCell = dataCellSpaned (numCols,1) (IntValue numObjects)
             numCols        = length colInfos
           in
            ([],[[numObjectsCell]])
      }
