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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MonoLocalBinds #-}

-------------------------------------------------------------------------------
-- | AttributeType:s for references/foreign keys.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
       (
         ReferenceAttributeTypeInfo(..),
         ReferencePresentationSpec(..),
         PresentationAttributeTypeInfo(..),

         reference_mandatory,
         reference_optional,

         attributeOutputForReferenceForExisting,

         getAttrOutputForReferenceAttribute,

         getIdOfInsertedIntoDatabase_fromMandatory,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.Database.SqlJoin as Sql
import qualified Wilde.Database.Executor as SqlExec

import qualified Wilde.Render.ServiceLink as RenderServiceLink

import qualified Wilde.Media.Presentation as Presentation

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.JoinUtils as OmDbJ
import qualified Wilde.ObjectModel.Database.InputExistingSansPresentationInfo as InputExisting
import qualified Wilde.ObjectModel.Database.Execution.SelectSansPresentationInfo as SelectPlain
import           Wilde.ObjectModel.DatabaseAndPresentation
import           Wilde.ObjectModel.UserInteraction.Output.CreateCommon
import           Wilde.ObjectModel.UserInteraction.OutputTypes (AttributeUiDefaultForExisting(..))
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils

import           Wilde.Media.Database.Monad
import           Wilde.Media.UserInteraction
import qualified Wilde.Media.UserInteraction.Io as UiIo
import qualified Wilde.Media.UserInteraction.Output as Ui
import           Wilde.Media.GenericStringRep as GenericStringRep

import           Wilde.WildeUi.StdValueTypes

import qualified Wilde.Application.ServiceLink as ServiceLink

import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import           Wilde.ApplicationConstruction.UserInteraction.Output.LabelAndWidget
import           Wilde.ApplicationConstruction.UserInteraction.Input.UserInteractionInputers
import           Wilde.ApplicationConstruction.GenericStringRepIo
import qualified Wilde.ApplicationConstruction.Database.AttributeTypeDatabaseInfo as AtDbInfo
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo
import qualified Wilde.ApplicationConstruction.UserInteraction.Io as UiIo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Specification of how objects are presented.
-- data ReferencePresentationSpec atConf dbTable otNative idAtExisting idAtCreate =
--   ReferencePresentationSpec
data ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate =
  ReferencePresentationSpec
  {
    otpsPresentationString            :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
                                      -> TranslationMonad MultiItemPresentation,
    -- | Orders the selection in the database when getting only the presentation string.
    otpsPresentationStringOrder       :: [Any (AttributeType atConf dbTable)],
    otpsPresentationAttributeTypeInfo :: Any (PresentationAttributeTypeInfo atConf dbTable)
  }

-- | Specifies how the presentation string is derived from an 'ObjectType'.
data PresentationAttributeTypeInfo atConf dbTable e c =
  PresentationAttributeTypeInfo
  {
    patiAt   :: AttributeType atConf dbTable e c,
    patiShow :: e -> String
   }

data ReferenceAttributeTypeInfo otConf atConf dbTableSrc dbTableDst otNativeDst idAtExistingDst idAtCreateDst =
    (Sql.SQL_IDENTIFIER dbTableSrc
    ,Database.INPUT_FOR_EXISTING atConf
    ,Database.COLUMN_NAMES atConf)
    =>
    ReferenceAttributeTypeInfo
    {
      ratiUiWidgetConstructor :: MultiWidgetConstructor
    , ratiRefDst              :: StandardServices.ObjectTypeSetup otConf atConf dbTableDst otNativeDst idAtExistingDst idAtCreateDst
    , ratiTableColumns        :: NonEmpty.List dbTableSrc
    , ratiPresSpec            :: ReferencePresentationSpec        otConf atConf dbTableDst otNativeDst idAtExistingDst idAtCreateDst
     }

-- A Multi Item with GenericStringRep as key.
type RawMultiItem = (GenericStringRep,MultiItemPresentation)

-- A function that may check that the values are valid.
-- If not, an exception should be thrown.
--
-- This is used to check that there exist at least one target Object
-- for mandatory references.
type RawValuesChecker = [RawMultiItem] -> Ui.UserInteractionOutputMonad ()

-- | A 'RawValuesChecker' for optional values - no check at all.
rawValuesChecker_optional :: RawValuesChecker
rawValuesChecker_optional _ = return ()


-------------------------------------------------------------------------------
-- - Mandatory Reference -
-------------------------------------------------------------------------------


reference_mandatory :: Database.DATABASE_TABLE otConf
  => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTableSrc dbTableDst otNativeDst idAtExistingDst idAtCreateDst
  -> (atConf dbTableDst idAtExistingDst idAtCreateDst
      -> dbTableSrc
      -> atConf dbTableSrc idAtExistingDst idAtExistingDst)
  -> dbTableSrc
  -> StyledTitle
  -> AttributeType           (UiIoAndDbIo.Configuration atConf) dbTableSrc idAtExistingDst idAtExistingDst
reference_mandatory rati@(ReferenceAttributeTypeInfo {
                             ratiRefDst = otSetupRefTarget@(StandardServices.ObjectTypeSetup {
                                                               StandardServices.objectType = otRefTarget@(ObjectType {})
                                                               })
                             }) translateAnnotation fieldSrc title
  =
  AttributeType
  {
    atCrossRefKey                 = attributeName
  , atPresentationO               = presentationOutputer
  , atConfiguration =
    UiIoAndDbIo.Configuration
    {
      UiIoAndDbIo.presentationO            = theAtPresentation
    , UiIoAndDbIo.genericStringRepIo       = attributeGenericStringRepIo
    , UiIoAndDbIo.uiIoForAttributeName     = uiIoExistingForReference valuesChecker rati
    , UiIoAndDbIo.uiCreateOption           = Nothing
    , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate
    , UiIoAndDbIo.databaseConfForExisting  = atDbIForExisting
    , UiIoAndDbIo.dbPresentationInfoGetter = presInfoGetter_mandatory rati fieldSrc
    , UiIoAndDbIo.annotation               = translateAnnotation (UiIoAndDbIo.annotation atConfDstIdAt) fieldSrc
    }
  }
  where
    atConfDstIdAt         = atConfiguration atForDstIdAt
    attributeName         = Sql.sqlIdentifier fieldSrc
    atForDstIdAt          = otAttributeTypeForIdAt rati
    atDbIForExisting      = attributeTypeDatabaseConfigForExisting rati
    atInfoForCreate       = atInfoForCreate_ref_mandatory valuesChecker rati fieldSrc
    presentationOutputer  = atPresentationO atForDstIdAt
    theAtPresentation     = AttributeTypePresentation
                            {
                              atpoOutput = presentationOutputer
                            , atpoTitle  = title
                            }
    atPresentationOForRef = atPresentationO atForDstIdAt
    attributeGenericStringRepIo = atGsrIo_existing_only_forIdAt otRefTarget
    valuesChecker :: RawValuesChecker
    valuesChecker [] = Ui.throwErr $ OmUtils.unclassifiedError errMsg
    valuesChecker _  = return ()
    errMsg = wildeStyled title ++ ": No objects for reference to " ++ targetObjectTypeName
    targetObjectTypeName = wildeStyled . StandardServices.titleWithStyle $ otSetupRefTarget


-------------------------------------------------------------------------------
-- - Optional Reference -
-------------------------------------------------------------------------------


reference_optional :: Database.DATABASE_TABLE otConf
  => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTableSrc dbTableDst otNativeDst idAtExistingDst idAtCreateDst
  -> (atConf dbTableDst idAtExistingDst idAtCreateDst
      -> dbTableSrc
      -> atConf dbTableSrc (Maybe idAtExistingDst) (Maybe idAtExistingDst))
  -> dbTableSrc
  -> StyledTitle
  -> AttributeType (UiIoAndDbIo.Configuration atConf) dbTableSrc (Maybe idAtExistingDst) (Maybe idAtExistingDst)
reference_optional rati@(ReferenceAttributeTypeInfo {
                            ratiRefDst = otSetupRefTarget@(StandardServices.ObjectTypeSetup {
                                                              StandardServices.objectType = otRefTarget@(ObjectType {})
                                                              })
                            })
  translateAnnotation field title
  =
   AttributeType
   {
     atCrossRefKey              = attributeName
   , atPresentationO            = presentationOutputer
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = theAtPresentation
     , UiIoAndDbIo.genericStringRepIo       = attributeGenericStringRepIo
     , UiIoAndDbIo.uiIoForAttributeName     = uiIoExistingForReference_optional rati
     , UiIoAndDbIo.uiCreateOption           = Nothing
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate
     , UiIoAndDbIo.databaseConfForExisting  = atDbIForExisting
     , UiIoAndDbIo.dbPresentationInfoGetter = presInfoGetter_optional rati field
     , UiIoAndDbIo.annotation               = translateAnnotation
                                              (UiIoAndDbIo.annotation atConfDstIdAt)
                                              field
     }
   }
  where
    otRefTarget           = StandardServices.objectType otSetupRefTarget
    attributeName         = Sql.sqlIdentifier field
    atConfDstIdAt         = atConfiguration atForDstIdAt
    atForDstIdAt          = otAttributeTypeForIdAt rati
    atDbIForExisting      = dbIoForExisting_optional $ attributeTypeDatabaseConfigForExisting rati
    atInfoForCreate       = atInfoForCreate_ref_optional rati field
    atPresentationOForRef = atPresentationO atForDstIdAt
    presentationOutputer  = maybe empty (atPresentationO atForDstIdAt)
    theAtPresentation     = AttributeTypePresentation
                            {
                              atpoOutput = presentationOutputer
                            , atpoTitle  = title
                            }
    attributeGenericStringRepIo = atGsrIo_optional_ValueMissing_is_Nothing $
                                  atGsrIo_existing_only_forIdAt otRefTarget

attributeOutputForReferenceForExisting ::
  (Database.DATABASE_TABLE otConf
  ,Database.COLUMN_NAMES atConf
  ,Database.INPUT_FOR_EXISTING atConf
  ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
  )
  => RawValuesChecker
  -> ObjectType                otConf atConf dbTable' otNative idAtExisting idAtCreate'
  -> ReferencePresentationSpec otConf atConf dbTable' otNative idAtExisting idAtCreate'
  -> CrossRefIdentifier
  -> MultiWidgetConstructor
  -> Ui.WidgetConstructorGetter
     (AttributeUiDefaultForExisting idAtExisting)
  -- -> Ui.UserInteractionOutputMonad (Maybe idAtExisting
  --                                     -> UiIo.ObjectName
  --                                     -> UiIo.LabelAndWidget)
attributeOutputForReferenceForExisting valuesChecker otRefTarget@(ObjectType {}) refPresSpecTarget attributeName widgetConstructor =
  do
    attrOutputFunForMaybe <- getAttrOutputForReferenceAttribute
                             valuesChecker
                             otRefTarget
                             refPresSpecTarget
                             attributeName
                             widgetConstructor
    return $ attrOutputFunForMaybe .
             fmap (OmGsr.gsrOutputer $ OmGsr.otIoForIdAtForExisting otRefTarget)

atInfoForCreate_ref_mandatory :: Database.DATABASE_TABLE otConf
                              => RawValuesChecker
                              -> ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTableSrc dbTableDst otNativeDst idAtExistingDst typeForCreate'
                              -> dbTableSrc
                              -> UiIoAndDbIo.AttributeTypeMediaIoForCreate idAtExistingDst idAtExistingDst
atInfoForCreate_ref_mandatory valuesChecker
  rati@(ReferenceAttributeTypeInfo {
           ratiRefDst = otSetupRefTarget@(StandardServices.ObjectTypeSetup {
                                             StandardServices.objectType = otRefTarget@(ObjectType {})
                                             })
           })
  field =
  let
    attributeName               = Sql.sqlIdentifier field
    dbIoForExistingForRefTarget = attributeTypeDatabaseConfigForExisting rati
    uiIoForExistingForRefTarget = uiIoForAttibuteName rati
  in
   UiIoAndDbIo.AttributeTypeMediaIoForCreate
   {
     UiIoAndDbIo.aticDatabaseOutputer = AtDbInfo.mkOutputerWithConnection $
                                        dbOutputer $
                                        AtDbInfo.atdbioeIo dbIoForExistingForRefTarget,
     UiIoAndDbIo.aticUiIo =
       UiIo.UserInteractionIo
       { UiIo.uiInputer  = UiIo.uiInputer uiIoForExistingForRefTarget
       , UiIo.uiOutputer = uiOutputForCreate_mandatory valuesChecker rati
       }
   }


uiOutputForCreate_mandatory :: Database.DATABASE_TABLE otConf
                            => RawValuesChecker
                            -> ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                            -> UiIo.AttributeName
                            -> AttributeTypeOutputerForCreate typeForExisting typeForExisting
uiOutputForCreate_mandatory valuesChecker = uiOutputForCreate valuesChecker Just

uiOutputForCreate_optional :: Database.DATABASE_TABLE otConf
                           => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                           -> UiIo.AttributeName
                           -> AttributeTypeOutputerForCreate (Maybe typeForExisting) (Maybe typeForExisting)
uiOutputForCreate_optional = uiOutputForCreate rawValuesChecker_optional refExisting2RefTargetExisting
  where
    refExisting2RefTargetExisting = maybe Nothing Just


atInfoForCreate_ref_optional :: Database.DATABASE_TABLE otConf
                             => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                             -> dbTable
                             -> UiIoAndDbIo.AttributeTypeMediaIoForCreate (Maybe typeForExisting) (Maybe typeForExisting)
atInfoForCreate_ref_optional rati@(ReferenceAttributeTypeInfo {}) field =
  let
    UiIoAndDbIo.AttributeTypeMediaIoForCreate dbOutputer (UiIo.UserInteractionIo _ uiInputer_mandatory)
      = atInfoForCreate_ref_mandatory rawValuesChecker_optional rati field
    dbOutputer_o = \mbV conn -> maybe (return [SqlNull]) (\v -> dbOutputer v conn) mbV
    uiInputer_o  = inputerForObjectName_optional_from_mandatory uiInputer_mandatory
    uiOutputer_o = uiOutputForCreate_optional rati
  in
   UiIoAndDbIo.AttributeTypeMediaIoForCreate
   {
     UiIoAndDbIo.aticDatabaseOutputer = dbOutputer_o,
     UiIoAndDbIo.aticUiIo             = UiIo.UserInteractionIo
                                        { UiIo.uiInputer  = uiInputer_o
                                        , UiIo.uiOutputer = uiOutputer_o
                                        }
   }

attributeTypeDatabaseConfigForExisting :: ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                                       -> AtDbInfo.AttributeTypeDatabaseConfigForExisting dbTable typeForExisting
attributeTypeDatabaseConfigForExisting (ReferenceAttributeTypeInfo {
                        ratiRefDst       = otSetup,
                        ratiTableColumns = tableColumns
                        }) =
  AtDbInfo.AttributeTypeDatabaseConfigForExisting
  {
    AtDbInfo.atdbioeIo        = dbIo
  , AtDbInfo.atdbioeStructure = fmap DatabaseColumn tableColumns
  }
  -- (atDatabaseConfForExisting $ otIdAttributeType ot) { atdbioeStructure = fmap DatabaseColumn tableColumns }
  where
    dbIo = AtDbInfo.atdbioeIo . UiIoAndDbIo.databaseConfForExisting . atConfiguration . otIdAttributeType $ ot
    ot = StandardServices.objectType otSetup

otAttributeTypeForIdAt :: ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTableSrc dbTableDst otNativeDst idAtExistingDst idAtCreateDst
                       -> AttributeType (UiIoAndDbIo.Configuration atConf) dbTableDst idAtExistingDst idAtCreateDst
otAttributeTypeForIdAt = otIdAttributeType . StandardServices.objectType . ratiRefDst

uiIoExistingForReference :: Database.DATABASE_TABLE otConf
                         => RawValuesChecker
                         -> ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                         -> UiIo.AttributeTypeUiIoForExisting typeForExisting
uiIoExistingForReference valuesChecker
  rati@(ReferenceAttributeTypeInfo
                                  {
                                    ratiUiWidgetConstructor = widgetConstructor,
                                    ratiRefDst              = otSetupRefTarget,
                                    ratiPresSpec            = presSpec
                                  })
  =
  UiIo.UserInteractionIo
  {
    UiIo.uiOutputer = outputer
  , UiIo.uiInputer  = UiIo.uiInputer uiIoForFromRef
  }
  where
    otRefTarget    = StandardServices.objectType otSetupRefTarget
    outputer       = \attributeName -> attributeOutputForReferenceForExisting
                                       valuesChecker
                                       otRefTarget presSpec
                                       attributeName
                                       widgetConstructor
    uiIoForFromRef = uiIoForAttibuteName rati

uiIoForAttibuteName :: ReferenceAttributeTypeInfo
                       otConf (UiIoAndDbIo.Configuration atConf)
                       dbTable1 dbTable otNative typeForExisting typeForCreate
                    -> UiIo.AttributeTypeUiIoForExisting typeForExisting
uiIoForAttibuteName =
  UiIoAndDbIo.atUiIoForExisting .
  otIdAttributeType .
  StandardServices.objectType .
  ratiRefDst

uiIoExistingForReference_optional :: Database.DATABASE_TABLE otConf
                                  => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                                  -> UiIo.AttributeTypeUiIoForExisting (Maybe typeForExisting)
uiIoExistingForReference_optional rati@(ReferenceAttributeTypeInfo {}) =
  UiIo.UserInteractionIo
  {
    UiIo.uiOutputer = \attributeName -> do
       forMaybe <- output_mandatory attributeName
       return $ maybe (forMaybe Nothing) forMaybe,
    UiIo.uiInputer  = inputerForObjectName_optional_from_mandatory input_mandatory
  }
  where
    UiIo.UserInteractionIo output_mandatory input_mandatory = uiIoExistingForReference
                                                              rawValuesChecker_optional
                                                              rati

presInfoGetter_mandatory :: Database.DATABASE_TABLE otConf
                         => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                         -> dbTable
                         -> AttributeWithPresentationInfoDbInputerInfo dbTable typeForExisting
presInfoGetter_mandatory rati@(ReferenceAttributeTypeInfo {
                                  ratiRefDst = otSetupRefTarget@(StandardServices.ObjectTypeSetup {
                                                                    StandardServices.objectType = otRefTarget@(ObjectType {})
                                                                    })
                                  })
  field
  =
    AttributeWithPresentationInfoDbInputerInfo (Just (sqlExprsGetter,presAtReader))
  where
    sqlAtRef           = Sql.newAttribute (NonEmpty.singleton field)
    -- otSetupRefTarget   = ratiRefDst rati
    -- otRefTarget        = StandardServices.objectType otSetupRefTarget
    -- presAtInfoTarget  :: Any
    --                      (PresentationAttributeTypeInfo
    --                       (UiIoAndDbIo.Configuration atConf) dbTable')
    presAtInfoTarget   = ratiPresentationAttributeTypeInfo rati
    sqlExprsGetter     = selectPresCols_mandatory sqlAtRef otRefTarget presAtInfoTarget
    patiAnyEcRefTarget = ratiPresentationAttributeTypeInfo rati
    presAtReader       = OmUtils.anyValueApply
                         (attributeWithPresInfoReader_mandatory otSetupRefTarget)
                         patiAnyEcRefTarget

presInfoGetter_optional :: Database.DATABASE_TABLE otConf
                        => ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                        -> dbTable
                        -> AttributeWithPresentationInfoDbInputerInfo dbTable (Maybe typeForExisting)
presInfoGetter_optional rati@(ReferenceAttributeTypeInfo {
                                 ratiRefDst = otSetupRefTarget@(StandardServices.ObjectTypeSetup {
                                                                   StandardServices.objectType = otRefTarget@(ObjectType {})
                                                                   })
                                 })
  field =
   AttributeWithPresentationInfoDbInputerInfo (Just (sqlExprsGetter,presAtReader))
  where
    sqlAtRef           = Sql.newAttribute (NonEmpty.singleton field)
    presAtInfoTarget   = ratiPresentationAttributeTypeInfo rati
    sqlExprsGetter     = selectPresCols_optional sqlAtRef otRefTarget presAtInfoTarget
    patiAnyEcRefTarget = ratiPresentationAttributeTypeInfo rati
    presAtReader       = OmUtils.anyValueApply
                         (attributeWithPresInfoReader_optional otSetupRefTarget)
                         patiAnyEcRefTarget


-------------------------------------------------------------------------------
-- Helper method for constructing a UI outputer for create, for
-- reference attributes.
--
-- Generalized so that it may be used to construct both mandatory and
-- optional references.
-------------------------------------------------------------------------------
uiOutputForCreate :: Database.DATABASE_TABLE otConf
                  => RawValuesChecker
                  -> (e -> Maybe typeForExisting)
                  -> ReferenceAttributeTypeInfo otConf (UiIoAndDbIo.Configuration atConf) dbTable dbTable' otNative' typeForExisting typeForCreate'
                  -> UiIo.AttributeName
                  -> Ui.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate e e)
uiOutputForCreate valuesChecker
  refExisting2RefTargetExisting
  rati@(ReferenceAttributeTypeInfo {})
  attributeName
  =
  do
    let outputMaybeExistingForRefTarget = UiIo.uiOutputer $
                                          uiIoExistingForReference
                                          valuesChecker
                                          rati
    let otRefTarget = StandardServices.objectType $ ratiRefDst rati
    outputerForMaybeExisting <- outputMaybeExistingForRefTarget attributeName
    return $ \mbAttributeDefault ->
      let
        translateToTypeForExisting = OmGsr.otInputerForIdAtForExisting otRefTarget
        mbExisting = case mbAttributeDefault of
          Nothing                  -> Nothing
          Just (DefaultCreateFromUiPreFill genericValue) -> either (const Nothing) Just
                                                            (translateToTypeForExisting genericValue)
          Just (DefaultCreateFromExisting e) -> refExisting2RefTargetExisting e
          Just (DefaultCreateFromCreate   e) -> refExisting2RefTargetExisting e
      in
       outputerForMaybeExisting mbExisting


atGsrIo_existing_only_forIdAt = atGsrIo_existing_only .
                                UiIoAndDbIo.genericStringRepIo .
                                atConfiguration .
                                otIdAttributeType

-- | Translates a 'AttributeGenericStringRepIo' to one who's
-- type-for-existing and type-for-create are both the type-for-existing of the
-- given object.
atGsrIo_existing_only :: AttributeGenericStringRepIo e c
                      -> AttributeGenericStringRepIo e e
atGsrIo_existing_only (AttributeGenericStringRepIo existingIo _) =
  AttributeGenericStringRepIo existingIo existingIo


dbIoForExisting_optional :: AtDbInfo.AttributeTypeDatabaseConfigForExisting dbTable typeForExisting
                         -> AtDbInfo.AttributeTypeDatabaseConfigForExisting dbTable (Maybe typeForExisting)
dbIoForExisting_optional (AtDbInfo.AttributeTypeDatabaseConfigForExisting {
                             AtDbInfo.atdbioeIo        = DatabaseIo output input,
                             AtDbInfo.atdbioeStructure = structure
                             }) =
  AtDbInfo.AttributeTypeDatabaseConfigForExisting
  {
    AtDbInfo.atdbioeIo =
       DatabaseIo
       { dbOutputer = maybe (return [SqlNull]) output
       , dbInputer  = inputMaybe
       },
    AtDbInfo.atdbioeStructure = structure
  }
  where
    inputMaybe [SqlNull] = return Nothing
    inputMaybe xs        = fmap Just $ input xs


selectPresCols_mandatory :: (Database.DATABASE_TABLE otConf
                            ,Database.COLUMN_NAMES atConf
                            ,Sql.SQL_IDENTIFIER dbTableSrc
                            )
                         => Sql.Attribute dbTableSrc idAE
                         -> ObjectType otConf atConf dbTableDst otN idAE idAC
                         -> Any (PresentationAttributeTypeInfo atConf dbTableDst)
                         -> Sql.JoinMonad dbTableSrc [Sql.SqlExpr (Sql.BasedOn dbTableSrc)]
selectPresCols_mandatory sqlAtRef otRefTarget@(ObjectType {}) patiAnyEcRefTarget =
  do
    join <- sqlAtRef
            `Sql.joinNatural`
            (OmDbJ.newSqlEntity otRefTarget,
             OmDbJ.newSqlAttribute $ otIdAttributeType otRefTarget)
    OmUtils.anyValueApply (getColExprs join) patiAnyEcRefTarget

selectPresCols_optional :: (Database.DATABASE_TABLE otConf
                           ,Sql.SQL_IDENTIFIER dbTableSrc
                           ,Database.COLUMN_NAMES atConf
                           )
                        => Sql.Attribute dbTableSrc (Maybe idAE)
                        -> ObjectType otConf atConf dbTableDst otN idAE idAC
                        -> Any (PresentationAttributeTypeInfo atConf dbTableDst)
                        -> Sql.JoinMonad dbTableSrc [Sql.SqlExpr (Sql.BasedOn dbTableSrc)]
selectPresCols_optional sqlAtRef otRefTarget@(ObjectType {}) patiAnyEcRefTarget =
  do
    join <- sqlAtRef
            `Sql.joinLeftOuter`
            (OmDbJ.newSqlEntity otRefTarget,
             OmDbJ.newSqlAttribute $ otIdAttributeType otRefTarget)
    OmUtils.anyValueApply (getColExprs join) patiAnyEcRefTarget

getColExprs :: (Sql.SQL_IDENTIFIER base
               ,Sql.SQL_IDENTIFIER other
               ,Database.COLUMN_NAMES atConf
               )
            => Sql.Join base other
            -> PresentationAttributeTypeInfo atConf other e c
            -> Sql.JoinMonad base [Sql.SqlExpr (Sql.BasedOn base)]
getColExprs join patiOther =
  let
    atPres     = patiAt patiOther
    dbAtInBase = Sql.includeFromJoined join (OmDbJ.newSqlAttribute atPres)
  in
   Sql.fieldExprList dbAtInBase

attributeWithPresInfoReader_mandatory :: (Database.INPUT_FOR_EXISTING atConf
                                         ,OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                                         )
                                      => StandardServices.ObjectTypeSetup   otConf atConf dbTable' otN targetIdAtE c''
                                      -> PresentationAttributeTypeInfo      atConf dbTable'     e' c'
                                      -> AttributeWithPresentationInfoDbInputer              targetIdAtE
attributeWithPresInfoReader_mandatory (StandardServices.ObjectTypeSetup
                                       {
                                         StandardServices.objectType = ot@(ObjectType {})
                                       }) pati repVal sqlValsForPres =
  do
    let presAt     = patiAt pati
    presAtVal     <- InputExisting.inputAttributeValue presAt sqlValsForPres
    return $ getObjectPresValForExisting ot pati repVal presAtVal

attributeWithPresInfoReader_optional :: (Database.INPUT_FOR_EXISTING atConf
                                        ,OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                                        )
                                     => StandardServices.ObjectTypeSetup       otConf atConf dbTable' otN e         c''
                                     -> PresentationAttributeTypeInfo          atConf dbTable'     e'        c'
                                     -> AttributeWithPresentationInfoDbInputer                  (Maybe e)
attributeWithPresInfoReader_optional _ _ Nothing _ = return (return empty)
attributeWithPresInfoReader_optional (StandardServices.ObjectTypeSetup
                                      {
                                        StandardServices.objectType = ot@(ObjectType {})
                                      }) pati (Just repVal) sqlValues =
  do
    let presAt     = patiAt pati
    presAtVal     <- InputExisting.inputAttributeValue presAt sqlValues
    return $ getObjectPresValForExisting ot pati repVal presAtVal

-- | Generates the presentation string for a reference to an existing object.
getObjectPresValForExisting :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                            => ObjectType otConf atConf dbTable otNative e c
                            -> PresentationAttributeTypeInfo atConf dbTable e' c'
                            -> e
                            -> e'
                            -> Presentation.Monad AnySVALUE
getObjectPresValForExisting otTarget pati objIdVal presAtVal =
  mkShowOneLink otTarget objIdVal presStr
  where
    presStr = (patiShow pati) presAtVal

-------------------------------------------------------------------------------
-- | Helper to construct, foremost, a widget for an 'AttributeType' that is a
-- reference.
--
-- Operates on the UI representation of default values - does not know about other
-- representations of default values.
--
-- Reads the 'Object's of the referenced 'ObjectType' from the database, and
-- constructs a widget using a given \"widget constructor\" that take these objects
-- as input.
-------------------------------------------------------------------------------
getAttrOutputForReferenceAttribute :: (Database.DATABASE_TABLE otConf
                                      ,Database.COLUMN_NAMES atConf
                                      ,Database.INPUT_FOR_EXISTING atConf
                                      ,OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                                      )
                                   => RawValuesChecker
                                      -- ^ Possibility to check that the values are valid.
                                      -- If they are not, an exception should be thrown.
                                   -> ObjectType                otConf atConf dbTable otNative idAtExisting idAtCreate
                                   -> ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate
                                   -> CrossRefIdentifier
                                   -> MultiWidgetConstructor
                                   -> Ui.UserInteractionOutputMonad (Maybe GenericStringRep
                                                                     -> UiIo.ObjectName
                                                                     -> UiIo.AnyWIDGET)
getAttrOutputForReferenceAttribute valuesChecker otRefTarget@(ObjectType {}) refPresSpecTarget attributeName widgetConstructor =
    do
      values <- Ui.toUserInteractionOutputMonadWithCar $
                \car ->
                readObjectKeyAndPresentationStringList otRefTarget refPresSpecTarget car
      valuesChecker values
      return $ attrOutput_oneOfManyAttribute
        widgetConstructor attributeName values


-- | Returns (key,presentation) for each object in the database.
readObjectKeyAndPresentationStringList :: (Database.DATABASE_TABLE otConf
                                          ,Database.INPUT_FOR_EXISTING atConf
                                          ,Database.COLUMN_NAMES atConf
                                          ,OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                                          )
                                       => ObjectType                otConf atConf dbTable otNative idAtExisting idAtCreate
                                       -> ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate
                                       -> SqlExec.ConnectionAndRenderer
                                       -> DatabaseMonad [RawMultiItem]
readObjectKeyAndPresentationStringList ot@(ObjectType {}) refPresSpec conn =
  do
    let getPresStr = otpsPresentationString      refPresSpec
    let orderByAts = otpsPresentationStringOrder refPresSpec
    let getObjectInfo o =
          do
            presStr <- toDatabaseMonad $ getPresStr o
            return (OmGsr.objOutputForIdAt o,presStr)
    objects <- SelectPlain.selectAll ot orderByAts conn
    mapM getObjectInfo objects

-- | Short cut to an 'ObjectType's 'PresentationAttributeTypeInfo'.
ratiPresentationAttributeTypeInfo :: ReferenceAttributeTypeInfo
                                     otConf
                                     (UiIoAndDbIo.Configuration atConf)
                                     dbTableSrc
                                     dbTableDst
                                     otNativeDst
                                     idAtExistingDst
                                     idAtCreateDst
                                  -> Any
                                  (PresentationAttributeTypeInfo (UiIoAndDbIo.Configuration atConf) dbTableDst)
ratiPresentationAttributeTypeInfo = otpsPresentationAttributeTypeInfo . ratiPresSpec

mkShowOneLink :: (ServiceLink.MonadWithServiceLinkConstructor m
                 ,OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                 )
              => ObjectType otConf atConf dbTable otNative e c
              -> e
              -> String
              -> m AnySVALUE
mkShowOneLink ot objId linkText =
  do
    serviceLinkTarget <- ServiceLink.mkLink srvcRefWp
    return $ RenderServiceLink.renderServiceLink_string
      linkText
      serviceLinkTarget
  where
    srvcRefWp = StandardServices.newObjectServiceReference
                ot
                StandardServices.ShowOne
                objId :: ServiceLink.ServiceReferenceWithParams


-------------------------------------------------------------------------------
-- - getIdOfInsertedIntoDatabase -
-------------------------------------------------------------------------------


getIdOfInsertedIntoDatabase_fromMandatory :: Database.GetIdOfInsertedIntoDatabase e e
getIdOfInsertedIntoDatabase_fromMandatory _ value _ = return value
